{-# LANGUAGE TypeOperators #-}

module Hopper.Distributed.Scheduler
  ( -- * Encode tasks, task id and task result
    Encoder (..),
    identityEncoder,

    -- * Running and scheduling
    Hopper.Scheduler.TaskId,
    Hopper.Scheduler.TaskResult,
    Hopper.Scheduler.Task (..),
    Hopper.Scheduler.withScheduler,
    Node,
    run,
  )
where

import qualified Control.Concurrent.Async
import qualified Control.Exception
import qualified Hopper.Distributed.Scheduler.Trace
import qualified Hopper.Distributed.ThriftServer
import qualified Hopper.Scheduler
import qualified Hopper.Thrift.Hopper.Server
import qualified Hopper.Thrift.Hopper.Types
import Prelude hiding (State)

-- | In order to send task ids, tasks and results over the network we need
-- to be able to serialize them.
data Encoder task = Encoder
  { encodeTaskId ::
      Hopper.Scheduler.TaskId task ->
      ByteString,
    decodeTaskId ::
      ByteString ->
      Maybe (Hopper.Scheduler.TaskId task),
    encodeTaskResult ::
      Hopper.Scheduler.TaskResult task ->
      ByteString,
    decodeTaskResult ::
      ByteString ->
      Maybe (Hopper.Scheduler.TaskResult task),
    encodeTask ::
      task ->
      ByteString,
    decodeTask ::
      ByteString ->
      Maybe task
  }

-- | A simple encoder that knows how to encode things that are 'ByteString's
-- already over the network.
identityEncoder ::
  ( Coercible task ByteString,
    Coercible (Hopper.Scheduler.TaskResult task) ByteString,
    Coercible (Hopper.Scheduler.TaskId task) ByteString
  ) =>
  Encoder task
identityEncoder =
  Encoder
    { encodeTaskId = coerce,
      decodeTaskId = Just . coerce,
      encodeTaskResult = coerce,
      decodeTaskResult = Just . coerce,
      encodeTask = coerce,
      decodeTask = Just . coerce
    }

-- | In this scheduler, 'Node' is represented by the endpoint address.
type Node = ByteString

run ::
  Encoder task ->
  Hopper.Distributed.Scheduler.Trace.Tracer task ->
  Hopper.Scheduler.Scheduler Node task ->
  IO ()
run encoder tracer' scheduler =
  Control.Concurrent.Async.race_ runScheduler runServer
  where
    runScheduler =
      Hopper.Scheduler.run scheduler

    runServer =
      Hopper.Distributed.ThriftServer.runSettings
        Hopper.Distributed.ThriftServer.defaultSettings
        mempty
        ( \endpointAddress -> do
            let tracer =
                  Hopper.Distributed.Scheduler.Trace.withTags
                    [Hopper.Distributed.Scheduler.Trace.Endpoint endpointAddress]
                    tracer'
            Hopper.Thrift.Hopper.Server.scheduler_mkServer
              Hopper.Thrift.Hopper.Server.Scheduler
                { requestNextTask = \_context -> requestNextTask encoder endpointAddress tracer scheduler,
                  heartbeat = \_context -> heartbeat encoder tracer scheduler
                }
        )

requestNextTask ::
  Encoder task ->
  Node ->
  Hopper.Distributed.Scheduler.Trace.Tracer task ->
  Hopper.Scheduler.Scheduler Node task ->
  Hopper.Thrift.Hopper.Types.RequestNextTaskRequest ->
  IO Hopper.Thrift.Hopper.Types.RequestNextTaskResponse
requestNextTask Encoder {..} node Hopper.Distributed.Scheduler.Trace.Tracer {..} scheduler _request = do
  withSpan Hopper.Distributed.Scheduler.Trace.RequestNextTaskSpan $ \span -> do
    attempt <-
      Hopper.Scheduler.requestTask scheduler node (Just 1)
    case attempt of
      Just attempt -> do
        tagSpan
          span
          [Hopper.Distributed.Scheduler.Trace.TaskId attempt.task.id]
        pure
          Hopper.Thrift.Hopper.Types.RequestNextTaskResponse
            { requestNextTaskResponse_task_id = Just (encodeTaskId attempt.task.id),
              requestNextTaskResponse_task = Just (encodeTask attempt.task.task),
              requestNextTaskResponse_timeout_in_seconds = Nothing, -- TODO
              requestNextTaskResponse_attempt = Just (fromIntegral attempt.attempt)
            }
      Nothing -> do
        tagSpan span [Hopper.Distributed.Scheduler.Trace.Timeout]
        pure
          Hopper.Thrift.Hopper.Types.RequestNextTaskResponse
            { requestNextTaskResponse_task_id = Nothing,
              requestNextTaskResponse_task = Nothing,
              requestNextTaskResponse_timeout_in_seconds = Nothing,
              requestNextTaskResponse_attempt = Nothing
            }

heartbeat ::
  Encoder task ->
  Hopper.Distributed.Scheduler.Trace.Tracer task ->
  Hopper.Scheduler.Scheduler Node task ->
  Hopper.Thrift.Hopper.Types.HeartbeatRequest ->
  IO ()
heartbeat Encoder {..} Hopper.Distributed.Scheduler.Trace.Tracer {..} scheduler request = void $ do
  let taskStatus =
        [ (taskId, taskResult)
          | taskStatus <- maybe [] toList request.heartbeatRequest_task_status,
            Just taskId <-
              [ case taskStatus.taskStatus_task_id of
                  Just taskId -> decodeTaskId taskId
                  Nothing -> Nothing -- TODO error?
              ],
            let taskResult =
                  case taskStatus.taskStatus_task_result of
                    Just (Hopper.Thrift.Hopper.Types.TaskResult_Error_message errorMessage) ->
                      Just (Left (Hopper.Scheduler.TaskExecutionException errorMessage))
                    Just (Hopper.Thrift.Hopper.Types.TaskResult_Timeout {}) ->
                      Just (Left Hopper.Scheduler.TaskExecutionTimedOut)
                    Just (Hopper.Thrift.Hopper.Types.TaskResult_Task_result result) ->
                      case decodeTaskResult result of
                        Just result ->
                          Just (Right result)
                        Nothing ->
                          Just (Left (Hopper.Scheduler.TaskExecutionException "could not decode task result"))
                    Nothing ->
                      Nothing
        ]

  withSpan (Hopper.Distributed.Scheduler.Trace.HeartbeatSpan taskStatus) $ \span -> do
    success <-
      Hopper.Scheduler.reportTaskStatus scheduler taskStatus (Just 1)
    unless success $ do
      tagSpan span [Hopper.Distributed.Scheduler.Trace.Timeout]
      -- Updating the result timed out. Signal that to the executor to make it send
      -- the heartbeat again.
      Control.Exception.throwIO Hopper.Thrift.Hopper.Types.TimeoutError
