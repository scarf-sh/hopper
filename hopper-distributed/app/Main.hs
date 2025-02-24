module Main where

import Control.Concurrent.Async (Concurrently (..))
import qualified Hopper.Distributed.Executor
import qualified Hopper.Distributed.Scheduler
import Hopper.Distributed.Scheduler.Trace (nullTracer)
import qualified Hopper.Scheduler
import System.Random (randomIO, randomRIO)

-- | Our task payload.
newtype T = T ByteString
  deriving stock (Show)

type instance Hopper.Scheduler.TaskId T = ByteString

type instance Hopper.Scheduler.TaskResult T = ByteString

-- | How to encode/decode our task type 'T', the task id and the result.
encoder :: Hopper.Distributed.Scheduler.Encoder T
encoder =
  Hopper.Distributed.Scheduler.identityEncoder

main :: IO ()
main = do
  args <- getArgs
  if args == ["executor"]
    then do
      Hopper.Distributed.Executor.run encoder "localhost" 4000 $ \attempt -> do
        shouldFail <- randomIO

        when shouldFail $
          error "this one failed"

        print
          ( attempt.taskId,
            attempt.task,
            attempt.attempt
          )

        pure ("done" :: ByteString)
    else do
      let requestNextTask _node _state = do
            id' <- randomRIO (0 :: Int, 100000000)

            let id :: ByteString
                id = encodeUtf8 (show @Text id')

                task :: T
                task = T "cool task"

            print ("scheduling task" :: Text, id)

            pure $
              Hopper.Scheduler.Task {id, task}

          handleLostTask task reason =
            print ("Lost" :: Text, task, reason)

          handleTaskResult taskId taskResult =
            print ("coolio" :: Text, taskId, taskResult)

      Hopper.Scheduler.withScheduler requestNextTask handleLostTask handleTaskResult $ \scheduler -> do
        void $
          runConcurrently
            ( Concurrently $
                Hopper.Distributed.Scheduler.run encoder nullTracer scheduler
            )
