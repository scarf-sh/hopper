{-# LANGUAGE NoFieldSelectors #-}

module Hopper.Scheduler
  ( withScheduler,
    run,
    shutdown,
    scheduleTask,
    requestTask,
    reportTaskStatus,
    Timeout (..),
    Scheduler,
    Scheduler.Task (..),
    Scheduler.TaskId,
    Scheduler.TaskResult,
    TaskExecutionError (..),
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Hopper.Scheduler.Internal qualified as Scheduler

data TaskExecutionError
  = TaskExecutionTimedOut
  | TaskExecutionException Text
  deriving stock (Eq, Show, Generic)

data Scheduler node task = Scheduler
  { -- | Current epoch
    epoch :: STM Scheduler.Epoch,
    -- | Blocking action that runs the scheduler until shutdown is requested.
    driver :: Scheduler.Driver,
    -- | Request the shutdown, once initiated, can't be undone. Action is idemptotent and non-blocking.
    requestShutdown :: STM (),
    -- | Schedule a task to the scheduler. This operation will block in case
    -- the scheduler is not ready to schedule the task.
    scheduleTask :: Scheduler.Task task -> STM (),
    -- | Heartbeat a task and maybe report the result of the task. This operation will
    -- block in case the scheduler is not ready to receive the status.
    reportTaskStatus :: [(Scheduler.TaskId task, Bool)] -> STM (),
    -- | Workers request tasks from the scheduler. This is a blocking action.
    requestTask :: node -> STM (Scheduler.Task task),
    -- | Report back the result of task execution back to the driver application.
    reportTaskResult :: Scheduler.TaskId task -> Either TaskExecutionError (Scheduler.TaskResult task) -> IO ()
  }

withScheduler ::
  ( Hashable (Scheduler.TaskId task),
    Eq (Scheduler.TaskId task),
    Show task,
    Show node,
    Show (Scheduler.TaskId task),
    Show (Scheduler.TaskResult task)
  ) =>
  -- | A way for the scheduler to report a task has been lost.
  ([Scheduler.Task task] -> Scheduler.Reason -> IO ()) ->
  -- | A way for the scheduler to report a task result back to the driver
  (Scheduler.TaskId task -> Either TaskExecutionError (Scheduler.TaskResult task) -> IO ()) ->
  (Scheduler node task -> IO r) ->
  IO r
withScheduler reportLostTasks reportTaskResult action =
  withClock 1 $ \epoch -> do
    -- For now, the task queue has room for exactly one element, making the
    -- task producer and scheduler run at the same rate without any intermediate
    -- buffering.
    taskQueue <- newEmptyTMVarIO
    lostTaskQueue <- newEmptyTMVarIO

    scheduler <-
      atomically $
        Scheduler.scheduler
          Scheduler.Inputs
            { epoch,
              taskToSchedule =
                takeTMVar taskQueue,
              lostTask = \attemptedTasks reason ->
                putTMVar lostTaskQueue ((fmap (.task) attemptedTasks), reason)
            }

    let lostTaskLoop = forever $ do
          (tasks, reason) <- atomically $ takeTMVar lostTaskQueue
          reportLostTasks tasks reason

    withAsync lostTaskLoop $ \_lostTaskThread ->
      action
        Scheduler
          { epoch,
            reportTaskResult,
            driver =
              scheduler.driver,
            requestShutdown =
              scheduler.shutdown,
            scheduleTask = \task ->
              putTMVar taskQueue task,
            reportTaskStatus = \taskStatus ->
              scheduler.reportTaskStatus taskStatus,
            requestTask = \node ->
              fmap (.task) (scheduler.schedule node)
          }

-- | Blocking action running until shutdown is being requested.
run :: Scheduler node task -> IO ()
run runtime = loop runtime.driver
  where
    loop driver = do
      driver <- atomically (Scheduler.runDriver driver)
      case driver of
        Just driver -> loop driver
        Nothing -> pure ()

-- | Request shutdown, non-blocking and idempotent.
shutdown :: Scheduler node task -> IO ()
shutdown runtime = atomically runtime.requestShutdown

-- | Schedules a task. If no timeout is specified this operation will block indenfinitely.
scheduleTask :: Scheduler node task -> Scheduler.TaskId task -> task -> Maybe Timeout -> IO Bool
scheduleTask runtime id task timeout = do
  result <-
    withTimeout
      runtime
      (runtime.scheduleTask (Scheduler.Task {id, task}))
      timeout
  pure (isJust result)

requestTask :: Scheduler node task -> node -> Maybe Timeout -> IO (Maybe (Scheduler.Task task))
requestTask runtime node timeout =
  withTimeout
    runtime
    (runtime.requestTask node)
    timeout

reportTaskStatus ::
  Scheduler node task ->
  [(Scheduler.TaskId task, Maybe (Either TaskExecutionError (Scheduler.TaskResult task)))] ->
  Maybe Timeout ->
  IO Bool
reportTaskStatus runtime taskStatus timeout = do
  result <-
    withTimeout
      runtime
      (runtime.reportTaskStatus [(taskId, isJust taskResult) | (taskId, taskResult) <- taskStatus])
      timeout

  for_ taskStatus $ \(taskId, taskResult) ->
    whenJust taskResult $ \taskResult ->
      runtime.reportTaskResult taskId taskResult

  pure (isJust result)

newtype Timeout
  = -- | Timeout in seconds
    Timeout Int
  deriving newtype (Num)

withTimeout :: Scheduler node task -> STM a -> Maybe Timeout -> IO (Maybe a)
withTimeout !runtime action timeout =
  case timeout of
    Just (Timeout timeout) -> do
      -- Anchor the start time. This atomically must not be merged with the one down below.
      t0 <- atomically runtime.epoch
      -- This transaction will be re-tried as soon as either the epoch changes or the action
      -- succeeeds.
      atomically $
        asum
          [ Just <$> action,
            do
              t1 <- runtime.epoch
              guard (t1 - t0 > timeout)
              pure Nothing
          ]
    Nothing ->
      Just <$> atomically action

withClock ::
  -- | Seconds per 'Epoch'
  Int ->
  (STM Scheduler.Epoch -> IO r) ->
  IO r
withClock secondsPerEpoch action = do
  clockVar <- newTVarIO 0
  withAsync (clock clockVar) $ \_async ->
    action (readTVar clockVar)
  where
    clock clockVar = forever $ do
      atomically $
        modifyTVar' clockVar (+ 1)
      threadDelay (1000000 * secondsPerEpoch)
