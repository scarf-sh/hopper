{-# LANGUAGE ScopedTypeVariables #-}

module Hopper.Distributed.Executor (Attempt (..), run) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async
import qualified Control.Monad.Catch
import qualified Control.Retry
import qualified Data.IORef
import qualified Data.Vector
import Hopper.Distributed.Scheduler (Encoder (..), TaskId, TaskResult)
import Hopper.Distributed.ThriftClient (Client, call, newClient)
import qualified Hopper.Thrift.Hopper.Client
import qualified Hopper.Thrift.Hopper.Types

data Attempt task = Attempt
  { attempt :: Int,
    taskId :: TaskId task,
    task :: task
  }

run ::
  Encoder task ->
  ByteString ->
  Int ->
  (Attempt task -> IO (TaskResult task)) ->
  IO ()
run encoder@Encoder {..} schedulerHost schedulerPort executeTask = do
  client <- newClient schedulerHost schedulerPort

  -- We are keeping track of the backoff to apply. In cases where the scheduler
  -- doesn't respond with a task in time - e.g. because there are no tasks we
  -- apply an exponential backoff strategy.
  backoffRef <- newIORef 0

  forever $ do
    Hopper.Thrift.Hopper.Types.RequestNextTask_Result_Success requestNextTaskResponse <-
      call
        client
        ( Hopper.Thrift.Hopper.Client.requestNextTask
            Hopper.Thrift.Hopper.Types.RequestNextTaskRequest
              { requestNextTaskRequest_noop = 0
              }
        )

    case (,,)
      <$> requestNextTaskResponse.requestNextTaskResponse_task_id
      <*> requestNextTaskResponse.requestNextTaskResponse_task
      <*> pure requestNextTaskResponse.requestNextTaskResponse_attempt of
      Just (encodedTaskId, task, attempt)
        | Just taskId <- decodeTaskId encodedTaskId,
          Just task <- decodeTask task -> do
            -- Reset the backoff delay in case we got a task to chew on.
            Data.IORef.writeIORef backoffRef 0

            result <-
              handleTaskExecution
                encoder
                client
                (fmap fromIntegral requestNextTaskResponse.requestNextTaskResponse_timeout_in_seconds)
                taskId
                ( executeTask
                    ( Attempt
                        { attempt =
                            maybe 1 fromIntegral attempt,
                          taskId,
                          task
                        }
                    )
                )

            sendHeartbeat
              client
              ( Hopper.Thrift.Hopper.Types.HeartbeatRequest
                  { heartbeatRequest_task_status =
                      Just $
                        Data.Vector.singleton
                          ( Hopper.Thrift.Hopper.Types.TaskStatus
                              { taskStatus_task_id = Just encodedTaskId,
                                taskStatus_task_result = Just result
                              }
                          )
                  }
              )
      _ -> do
        -- No task received, wait and try again. Increase the backoff for the next round.
        backoff <-
          Data.IORef.readIORef backoffRef

        when (backoff > 0) $
          threadDelay backoff

        let -- 1 milliseconds ~ 1000 microseconds
            baseDelay = 1000

            -- 1 second ~ 1000000 microseconds
            maxDelay = 1000000

        Data.IORef.writeIORef backoffRef $! min maxDelay (2 * max baseDelay backoff)

handleTaskExecution ::
  Encoder task ->
  Client ->
  Maybe Int ->
  TaskId task ->
  IO (TaskResult task) ->
  IO Hopper.Thrift.Hopper.Types.TaskResult
handleTaskExecution Encoder {..} client timeoutInSeconds taskId execute = do
  clockVar <- newTVarIO 0
  Control.Concurrent.Async.withAsync (ticker clockVar) $ \_clockThread ->
    Control.Concurrent.Async.withAsync execute $ \handle -> do
      loop (readTVar clockVar) handle
  where
    -- Moves the clock every second
    ticker :: TVar Int -> IO ()
    ticker clockVar = forever $ do
      threadDelay (1 * 1000000)
      atomically $
        modifyTVar' clockVar (+ 1)

    -- Waits for the task to finish. While waiting we'll send heartbeats to
    -- the scheduler so that it knows things are going alright.
    loop clock handle = do
      t0 <- atomically clock

      -- Wait on the result or on the next clock tick to send a heartbeat
      result <-
        atomically $
          asum
            [ do
                result <- Control.Concurrent.Async.waitCatchSTM handle
                case result of
                  Left exception ->
                    pure $
                      Right
                        ( Hopper.Thrift.Hopper.Types.TaskResult_Error_message
                            (show exception)
                        )
                  Right result ->
                    pure $
                      Right
                        ( Hopper.Thrift.Hopper.Types.TaskResult_Task_result
                            (encodeTaskResult result)
                        ),
              do
                t1 <- clock
                guard (t1 /= t0)
                pure (Left t1)
            ]

      case result of
        Left time
          | time >= fromMaybe maxBound timeoutInSeconds -> do
              Control.Concurrent.Async.cancel handle
              pure
                ( Hopper.Thrift.Hopper.Types.TaskResult_Timeout
                    Hopper.Thrift.Hopper.Types.Timeout
                )
          | otherwise -> do
              sendHeartbeat
                client
                ( Hopper.Thrift.Hopper.Types.HeartbeatRequest
                    { heartbeatRequest_task_status =
                        Just $
                          Data.Vector.singleton
                            ( Hopper.Thrift.Hopper.Types.TaskStatus
                                { taskStatus_task_id = Just (encodeTaskId taskId),
                                  taskStatus_task_result = Nothing
                                }
                            )
                    }
                )

              loop clock handle
        Right result ->
          pure result

-- Send and retry sending a heartbeat in case the scheduler answers with a TimeoutError.
-- This is especially important when reporting back task results not so much for simple
-- heartbeats.
sendHeartbeat :: Client -> Hopper.Thrift.Hopper.Types.HeartbeatRequest -> IO ()
sendHeartbeat client heartbeatRequest = do
  Control.Retry.recovering
    (Control.Retry.exponentialBackoff 50000 <> Control.Retry.limitRetries 5)
    [ \_retryStatus ->
        Control.Monad.Catch.Handler $
          \(_exception :: Hopper.Thrift.Hopper.Types.TimeoutError) -> pure True
    ]
    ( \_retryStatus ->
        void $
          call client (Hopper.Thrift.Hopper.Client.heartbeat heartbeatRequest)
    )
