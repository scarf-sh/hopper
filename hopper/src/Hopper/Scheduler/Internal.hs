{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Hopper.Scheduler.Internal
  ( Epoch,
    TaskId,
    TaskGroup,
    TaskResult,
    Task (..),
    Attempt (..),
    Reason (..),
    Inputs (..),
    Scheduler (..),
    Driver (..),
    scheduler,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Prelude hiding (State)

-- | Unit of time.
type Epoch = Int

type family TaskId task

type family TaskResult task

type family TaskGroup task

data Task task = Task
  { id :: TaskId task,
    group :: TaskGroup task,
    task :: task
  }

deriving instance (Show task, Show (TaskId task), Show (TaskGroup task)) => Show (Task task)

data Attempt node task = Attempt
  { -- | The task itself.
    task :: !(Task task),
    -- | Attempt. (== 1 on 1st attempt)
    attempt :: !Int,
    -- | Epoch this task was scheduled
    epoch :: !Epoch,
    -- | Node this was scheduled on
    node :: node
  }

deriving instance (Show node, Show task, Show (TaskId task), Show (TaskGroup task)) => Show (Attempt node task)

data Reason
  = -- | The node on which the task was scheduled has gone lost.
    ReasonLostNode
  | -- | The tasks has timeout
    ReasonTaskTimeout
  deriving stock (Show)

data Inputs node task = Inputs
  { -- | A discrete, monotonically increasing counter. A driver can choose the time interval
    -- in which to increase the epoch. Using 'Epoch's abstracts away time and allows for simulating
    -- a scheduler over time more easily.
    epoch :: STM Epoch,
    -- | Signal that a task was lost due to node becoming unavailable.
    lostTask :: [Attempt node task] -> Reason -> STM (),
    -- | Request the next task to schedule on a given node. Also passes the current
    -- scheduler state to the function. This allows the function to attempt.
    requestNextTask ::
      node ->
      HashMap (TaskGroup task) Int ->
      HashMap (TaskId task) (Attempt node task) ->
      IO (Task task)
  }

data State node task = State
  { epoch :: {-# UNPACK #-} !(TVar Epoch),
    shutdown :: {-# UNPACK #-} !(TVar Bool),
    tasks :: {-# UNPACK #-} !(TVar (HashMap (TaskId task) (Attempt node task))),
    taskGroups :: {-# UNPACK #-} !(TVar (HashMap (TaskGroup task) Int))
  }

data Scheduler node task = Scheduler
  { schedule :: node -> IO (Attempt node task),
    reportTaskStatus :: [(TaskId task, Bool)] -> STM (),
    shutdown :: STM (),
    driver :: Driver
  }

newtype Driver = Driver {runDriver :: STM (Maybe Driver)}

data Pair a b = Pair !a !b

scheduler ::
  forall node task.
  ( Eq (TaskId task),
    Hashable (TaskId task),
    Eq (TaskGroup task),
    Hashable (TaskGroup task),
    Show (TaskId task),
    Show (TaskResult task),
    Show (Attempt node task)
  ) =>
  Inputs node task ->
  STM (Scheduler node task)
scheduler !inputs = do
  shutdownVar <- newTVar False
  tasksVar <- newTVar mempty
  taskGroupsVar <- newTVar mempty
  epoch <- inputs.epoch
  epochVar <- newTVar epoch

  let state =
        State
          { shutdown = shutdownVar,
            tasks = tasksVar,
            taskGroups = taskGroupsVar,
            epoch = epochVar
          }

      shutdown =
        writeTVar state.shutdown True

      driver :: Driver
      driver =
        -- Cache the driver here to avoid allocation in the common case
        let driver_ = Just driver
         in Driver $
              asum
                [ do
                    onShutdown state
                    pure Nothing,
                  do
                    onEpochChange state
                    pure driver_
                ]

  pure
    Scheduler
      { driver,
        shutdown,
        reportTaskStatus =
          reportTaskStatus state,
        schedule =
          scheduleTaskOnNode state
      }
  where
    scheduleTaskOnNode :: State node task -> node -> IO (Attempt node task)
    scheduleTaskOnNode !state node = do
      tasks <- readTVarIO state.tasks
      task <- inputs.requestNextTask node mempty tasks
      atomically $ do
        epoch <- inputs.epoch
        let attempt =
              Attempt
                { task,
                  epoch,
                  node,
                  attempt = 1
                }

        modifyTVar' state.tasks $
          HashMap.insert task.id attempt
        modifyTVar' state.taskGroups $
          HashMap.insertWith (+) task.group 1
        pure attempt

    reportTaskStatus :: State node task -> [(TaskId task, Bool)] -> STM ()
    reportTaskStatus !state status = do
      epoch <- inputs.epoch

      tasks <- readTVar state.tasks
      taskGroups <- readTVar state.taskGroups

      let Pair tasks' taskGroups' =
            foldl'
              ( \(Pair tasks taskGroups) (taskId, isCompleted) ->
                  case HashMap.lookup taskId tasks of
                    Just attempt
                      | isCompleted ->
                          Pair
                            (HashMap.delete taskId tasks)
                            ( HashMap.update
                                ( \x ->
                                    let !y = x - 1
                                     in if y > 0 then Just y else Nothing
                                )
                                attempt.task.group
                                taskGroups
                            )
                      | otherwise ->
                          Pair
                            ( HashMap.update
                                ( \Attempt {epoch = _, ..} ->
                                    Just $! Attempt {epoch, ..}
                                )
                                taskId
                                tasks
                            )
                            taskGroups
                    _ ->
                      Pair tasks taskGroups
              )
              (Pair tasks taskGroups)
              status

      writeTVar state.tasks $! tasks'
      writeTVar state.taskGroups $! taskGroups'

    onEpochChange :: State node task -> STM ()
    onEpochChange !state = do
      epoch <- readTVar state.epoch
      newEpoch <- inputs.epoch
      guard (epoch /= newEpoch)
      writeTVar state.epoch newEpoch
      tasks <- readTVar state.tasks
      taskGroups <- readTVar state.taskGroups

      let timedOutTasks =
            HashMap.filter
              (\attempt -> newEpoch - attempt.epoch > 10)
              tasks

          healthyTasks =
            HashMap.difference tasks timedOutTasks

          newTaskGroups =
            foldl'
              ( \taskGroups attempt ->
                  HashMap.update
                    ( \x ->
                        let !y = x - 1
                         in if y > 0 then Just y else Nothing
                    )
                    attempt.task.group
                    taskGroups
              )
              taskGroups
              timedOutTasks

      writeTVar state.tasks $! healthyTasks
      writeTVar state.taskGroups $! newTaskGroups

      unless (null timedOutTasks) $
        inputs.lostTask
          (HashMap.elems timedOutTasks)
          ReasonTaskTimeout

    onShutdown :: State node task -> STM ()
    onShutdown !state = do
      shutdown <- readTVar state.shutdown
      guard shutdown
      pure ()
{-# INLINEABLE scheduler #-}
