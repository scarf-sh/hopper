{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}

module Hopper.Thrift.Hopper.Client where

import Control.Applicative qualified
import Control.Exception qualified
import Data.ByteString qualified
import Data.HashMap.Strict qualified
import Data.HashSet qualified
import Data.Hashable qualified
import Data.Int qualified
import Data.Text qualified
import Data.Vector qualified
import Data.Vector.Instances ()
import GHC.Generics qualified
import Hopper.Thrift.Hopper.Types
import Pinch qualified
import Pinch.Client qualified
import Pinch.Internal.RPC qualified
import Pinch.Server qualified
import Prelude qualified

requestNextTask :: (RequestNextTaskRequest) -> (Pinch.Client.ThriftCall RequestNextTask_Result)
requestNextTask requestNextTaskRequest = Pinch.Client.TCall ("requestNextTask") (RequestNextTask_Args (requestNextTaskRequest))

heartbeat :: (HeartbeatRequest) -> (Pinch.Client.ThriftCall Heartbeat_Result)
heartbeat heartbeat = Pinch.Client.TCall ("heartbeat") (Heartbeat_Args (heartbeat))
