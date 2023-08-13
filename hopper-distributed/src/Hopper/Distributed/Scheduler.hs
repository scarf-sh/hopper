module Hopper.Distributed.Scheduler
  ( Task (..),
    Hopper.Scheduler.withScheduler,
    Hopper.Scheduler.scheduleTask,
    run,
  )
where

import qualified Control.Concurrent.Async
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
  Hopper.Scheduler.Scheduler Node Task ->
  IO ()
run scheduler =
  Control.Concurrent.Async.race_ runScheduler runServer
  where
    runScheduler =
      Hopper.Scheduler.run scheduler

    runServer =
      Hopper.Distributed.ThriftServer.runSettings
        Hopper.Distributed.ThriftServer.defaultSettings
        mempty
        ( Hopper.Thrift.Hopper.Server.scheduler_mkServer
            Hopper.Thrift.Hopper.Server.Scheduler
              { requestNextTask = \_context -> requestNextTask scheduler,
                heartbeat = \_context -> heartbeat scheduler
              }
        )

requestNextTask ::
  Hopper.Scheduler.Scheduler Node Task ->
  Hopper.Thrift.Hopper.Types.RequestNextTaskRequest ->
  IO Hopper.Thrift.Hopper.Types.RequestNextTaskResponse
requestNextTask scheduler _request = do
  task <-
    Hopper.Scheduler.requestTask scheduler () (Just 1)
  case task of
    Just task ->
      pure
        Hopper.Thrift.Hopper.Types.RequestNextTaskResponse
          { requestNextTaskResponse_task_id = Just task.id,
            requestNextTaskResponse_task = Just (taskToByteString task.task),
            requestNextTaskResponse_timeout_in_seconds = Nothing -- TODO
          }
    Nothing ->
      pure
        Hopper.Thrift.Hopper.Types.RequestNextTaskResponse
          { requestNextTaskResponse_task_id = Nothing,
            requestNextTaskResponse_task = Nothing,
            requestNextTaskResponse_timeout_in_seconds = Nothing
          }

heartbeat ::
  Hopper.Scheduler.Scheduler Node Task ->
  Hopper.Thrift.Hopper.Types.HeartbeatRequest ->
  IO ()
heartbeat scheduler request = do
  void $
    Hopper.Scheduler.reportTaskStatus
      scheduler
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
      (Just 1)
