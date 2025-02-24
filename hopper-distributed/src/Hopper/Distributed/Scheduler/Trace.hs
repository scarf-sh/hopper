module Hopper.Distributed.Scheduler.Trace
  ( Tracer (..),
    Tag (..),
    Span (..),
    nullTracer,
    withTags,
  )
where

import Hopper.Scheduler (TaskExecutionError)
import qualified Hopper.Scheduler

data Tag task
  = Timeout
  | TaskId (Hopper.Scheduler.TaskId task)
  | Endpoint ByteString

data Span task
  = RequestNextTaskSpan
  | HeartbeatSpan [(Hopper.Scheduler.TaskId task, Maybe (Either TaskExecutionError (Hopper.Scheduler.TaskResult task)))]

-- | A lightweight abstraction to trace execution in the distributed scheduler.
data Tracer task = forall span.
  Tracer
  { -- | Wrap a computation inside a @span@.
    withSpan :: forall a. Span task -> (span -> IO a) -> IO a,
    -- | Tag a @span@ with additional info.
    tagSpan :: span -> [Tag task] -> IO ()
  }

-- | Tracer that doesn't trace anything.
nullTracer :: Tracer task
nullTracer =
  Tracer
    { withSpan =
        \_span action -> action (),
      tagSpan =
        \() _tags -> pure ()
    }

-- | Tag each @span@ with @tags@.
withTags :: [Tag task] -> Tracer task -> Tracer task
withTags tags Tracer {..} =
  Tracer
    { withSpan = \span action ->
        withSpan span $ \span -> do
          tagSpan span tags
          action span,
      tagSpan
    }
