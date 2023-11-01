module Hopper.Distributed.Scheduler.Trace
  ( Tracer (..),
    Tag (..),
    Span (..),
    nullTracer,
    withTags,
  )
where

import Hopper.Scheduler (TaskExecutionError)

data Tag
  = Timeout
  | TaskId ByteString
  | Endpoint ByteString

data Span
  = RequestNextTaskSpan
  | HeartbeatSpan [(ByteString, Maybe (Either TaskExecutionError ByteString))]

-- | A lightweight abstraction to trace execution in the distributed scheduler.
data Tracer = forall span.
  Tracer
  { -- | Wrap a computation inside a @span@.
    withSpan :: forall a. Span -> (span -> IO a) -> IO a,
    -- | Tag a @span@ with additional info.
    tagSpan :: span -> [Tag] -> IO ()
  }

-- | Tracer that doesn't trace anything.
nullTracer :: Tracer
nullTracer =
  Tracer
    { withSpan =
        \_span action -> action (),
      tagSpan =
        \() _tags -> pure ()
    }

-- | Tag each @span@ with @tags@.
withTags :: [Tag] -> Tracer -> Tracer
withTags tags Tracer {..} =
  Tracer
    { withSpan = \span action ->
        withSpan span $ \span -> do
          tagSpan span tags
          action span,
      tagSpan
    }
