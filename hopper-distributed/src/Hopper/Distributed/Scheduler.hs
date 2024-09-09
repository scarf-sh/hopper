module Hopper.Distributed.Scheduler
  ( Task (..),
    Hopper.Scheduler.withScheduler,
    Hopper.Scheduler.scheduleTask,
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

type Node = ()

newtype Task = Task {taskToByteString :: ByteString}
  deriving stock (Show)

type instance Hopper.Scheduler.TaskId Task = ByteString

type instance Hopper.Scheduler.TaskResult Task = ByteString

run ::
  Hopper.Distributed.Scheduler.Trace.Tracer ->
  Hopper.Scheduler.Scheduler Node Task ->
  IO ()
run tracer' scheduler =
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
                { requestNextTask = \_context -> requestNextTask tracer scheduler,
                  heartbeat = \_context -> heartbeat tracer scheduler
                }
        )

requestNextTask ::
  Hopper.Distributed.Scheduler.Trace.Tracer ->
  Hopper.Scheduler.Scheduler Node Task ->
  Hopper.Thrift.Hopper.Types.RequestNextTaskRequest ->
  IO Hopper.Thrift.Hopper.Types.RequestNextTaskResponse
requestNextTask Hopper.Distributed.Scheduler.Trace.Tracer {..} scheduler _request = do
  withSpan Hopper.Distributed.Scheduler.Trace.RequestNextTaskSpan $ \span -> do
    attempt <-
      Hopper.Scheduler.requestTask scheduler () (Just 1)
    case attempt of
      Just attempt -> do
        tagSpan
          span
          [Hopper.Distributed.Scheduler.Trace.TaskId attempt.task.id]
        pure
          Hopper.Thrift.Hopper.Types.RequestNextTaskResponse
            { requestNextTaskResponse_task_id = Just attempt.task.id,
              requestNextTaskResponse_task = Just (taskToByteString attempt.task.task),
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
  Hopper.Distributed.Scheduler.Trace.Tracer ->
  Hopper.Scheduler.Scheduler Node Task ->
  Hopper.Thrift.Hopper.Types.HeartbeatRequest ->
  IO ()
heartbeat Hopper.Distributed.Scheduler.Trace.Tracer {..} scheduler request = void $ do
  let taskStatus =
        [ (taskId, taskResult)
          | taskStatus <- maybe [] toList request.heartbeatRequest_task_status,
            Just taskId <- [taskStatus.taskStatus_task_id],
            let taskResult =
                  case taskStatus.taskStatus_task_result of
                    Just (Hopper.Thrift.Hopper.Types.TaskResult_Error_message errorMessage) ->
                      Just (Left (Hopper.Scheduler.TaskExecutionException errorMessage))
                    Just (Hopper.Thrift.Hopper.Types.TaskResult_Timeout {}) ->
                      Just (Left Hopper.Scheduler.TaskExecutionTimedOut)
                    Just (Hopper.Thrift.Hopper.Types.TaskResult_Task_result result) ->
                      Just (Right result)
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
