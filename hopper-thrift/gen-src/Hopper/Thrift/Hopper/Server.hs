{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}

module Hopper.Thrift.Hopper.Server where

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
import Pinch.Internal.RPC qualified
import Pinch.Server qualified
import Prelude qualified

data Scheduler = Scheduler {requestNextTask :: (Pinch.Server.Context) -> (RequestNextTaskRequest) -> (Prelude.IO RequestNextTaskResponse), heartbeat :: (Pinch.Server.Context) -> (HeartbeatRequest) -> (Prelude.IO ())}

scheduler_mkServer :: (Scheduler) -> Pinch.Server.ThriftServer
scheduler_mkServer server =
  let functions =
        Data.HashMap.Strict.fromList
          ( [ ( "requestNextTask",
                Pinch.Server.CallHandler ((\ctx (RequestNextTask_Args a) -> Pinch.Internal.RPC.wrap @(RequestNextTask_Result) (requestNextTask (server) (ctx) (a))))
              ),
              ( "heartbeat",
                Pinch.Server.CallHandler ((\ctx (Heartbeat_Args a) -> Pinch.Internal.RPC.wrap @(Heartbeat_Result) (heartbeat (server) (ctx) (a))))
              )
            ]
          )
   in Pinch.Server.createServer ((\nm -> Data.HashMap.Strict.lookup (nm) (functions)))
