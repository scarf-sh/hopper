module Hopper.Distributed.Executor (run) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async
import qualified Data.Vector
import Hopper.Distributed.Scheduler (Task (..))
import Hopper.Distributed.ThriftClient (Client, call, newClient)
import Hopper.Scheduler (TaskId, TaskResult)
import qualified Hopper.Thrift.Hopper.Client
import qualified Hopper.Thrift.Hopper.Types

run ::
  ByteString ->
  Int ->
  (TaskId Task -> Task -> IO (TaskResult Task)) ->
  IO ()
run schedulerHost schedulerPort executeTask = do
  client <- newClient schedulerHost schedulerPort

  forever $ do
    Hopper.Thrift.Hopper.Types.RequestNextTask_Result_Success requestNextTaskResponse <-
      call
        client
        ( Hopper.Thrift.Hopper.Client.requestNextTask
            Hopper.Thrift.Hopper.Types.RequestNextTaskRequest
              { requestNextTaskRequest_noop = 0
              }
        )

    case (,)
      <$> requestNextTaskResponse.requestNextTaskResponse_task_id
      <*> requestNextTaskResponse.requestNextTaskResponse_task of
      Just (taskId, task) -> do
        result <-
          handleTaskExecution
            client
            (fmap fromIntegral requestNextTaskResponse.requestNextTaskResponse_timeout_in_seconds)
            taskId
            (executeTask taskId (Task task))

        void $
          call
            client
            ( Hopper.Thrift.Hopper.Client.heartbeat
                Hopper.Thrift.Hopper.Types.HeartbeatRequest
                  { heartbeatRequest_task_status =
                      Just $
                        Data.Vector.singleton
                          ( Hopper.Thrift.Hopper.Types.TaskStatus
                              { taskStatus_task_id = Just taskId,
                                taskStatus_task_result = Just result
                              }
                          )
                  }
            )
      Nothing ->
        pure ()

handleTaskExecution ::
  Client ->
  Maybe Int ->
  TaskId Task ->
  IO (TaskResult Task) ->
  IO Hopper.Thrift.Hopper.Types.TaskResult
handleTaskExecution client timeoutInSeconds taskId execute = do
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
                            result
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
              _ <-
                call
                  client
                  ( Hopper.Thrift.Hopper.Client.heartbeat
                      Hopper.Thrift.Hopper.Types.HeartbeatRequest
                        { heartbeatRequest_task_status =
                            Just $
                              Data.Vector.singleton
                                ( Hopper.Thrift.Hopper.Types.TaskStatus
                                    { taskStatus_task_id = Just taskId,
                                      taskStatus_task_result = Nothing
                                    }
                                )
                        }
                  )
              loop clock handle
        Right result ->
          pure result
