{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O2 -ddump-prep -ddump-to-file -dsuppress-all #-}

module Hopper.Scheduler.Internal
  ( Epoch,
    TaskId,
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

data Task task = Task
  { id :: TaskId task,
    task :: task
  }

deriving instance (Show task, Show (TaskId task)) => Show (Task task)

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

deriving instance (Show node, Show task, Show (TaskId task)) => Show (Attempt node task)

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
    -- | Returns the next task to schedule. This action blocks if there is no
    -- task for scheduling available.
    taskToSchedule :: STM (Task task),
    -- | Signal that a task was lost due to node becoming unavailable.
    lostTask :: [Attempt node task] -> Reason -> STM ()
  }

data State node task = State
  { epoch :: {-# UNPACK #-} !(TVar Epoch),
    shutdown :: {-# UNPACK #-} !(TVar Bool),
    tasks :: {-# UNPACK #-} !(TVar (HashMap (TaskId task) (Attempt node task)))
  }

data Scheduler node task = Scheduler
  { schedule :: node -> STM (Attempt node task),
    reportTaskStatus :: [(TaskId task, Bool)] -> STM (),
    shutdown :: STM (),
    driver :: Driver
  }

newtype Driver = Driver {runDriver :: STM (Maybe Driver)}

scheduler ::
  forall node task.
  ( Eq (TaskId task),
    Hashable (TaskId task),
    Show (TaskId task),
    Show (TaskResult task),
    Show (Attempt node task)
  ) =>
  Inputs node task ->
  STM (Scheduler node task)
scheduler !inputs = do
  shutdownVar <- newTVar False
  tasksVar <- newTVar mempty
  epoch <- inputs.epoch
  epochVar <- newTVar epoch

  let state =
        State
          { shutdown = shutdownVar,
            tasks = tasksVar,
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
    scheduleTaskOnNode :: State node task -> node -> STM (Attempt node task)
    scheduleTaskOnNode !state node = do
      task <- inputs.taskToSchedule
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
      pure attempt

    reportTaskStatus :: State node task -> [(TaskId task, Bool)] -> STM ()
    reportTaskStatus !state status = do
      epoch <- inputs.epoch
      modifyTVar' state.tasks $ \tasks ->
        foldl'
          ( \tasks (taskId, isCompleted) ->
              if isCompleted
                then HashMap.delete taskId tasks
                else
                  HashMap.alter
                    ( \attempt ->
                        case attempt of
                          Just Attempt {epoch = _epoch, ..} ->
                            Just $! Attempt {epoch, ..}
                          Nothing ->
                            Nothing
                    )
                    taskId
                    tasks
          )
          tasks
          status

    onEpochChange :: State node task -> STM ()
    onEpochChange !state = do
      epoch <- readTVar state.epoch
      newEpoch <- inputs.epoch
      guard (epoch /= newEpoch)
      writeTVar state.epoch newEpoch
      tasks <- readTVar state.tasks
      let timedOutTasks =
            HashMap.filter
              (\attempt -> newEpoch - attempt.epoch > 10)
              tasks

          healthyTasks =
            HashMap.difference tasks timedOutTasks
      writeTVar state.tasks $! healthyTasks

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
