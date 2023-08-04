{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}

module Hopper.Thrift.Hopper.Types where

import Control.Applicative qualified
import Control.DeepSeq qualified
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
import Pinch qualified
import Pinch.Internal.RPC qualified
import Pinch.Server qualified
import Prelude qualified

type TaskId = Data.ByteString.ByteString

data RequestNextTaskRequest = RequestNextTaskRequest {requestNextTaskRequest_noop :: Data.Int.Int32}
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable RequestNextTaskRequest where
  type Tag RequestNextTaskRequest = Pinch.TStruct

  pinch (RequestNextTaskRequest requestNextTaskRequest_noop) = Pinch.struct ([(1 Pinch..= requestNextTaskRequest_noop)])

  unpinch value = (Prelude.pure (RequestNextTaskRequest) Prelude.<*> (value Pinch..: 1))

instance Control.DeepSeq.NFData RequestNextTaskRequest

instance Data.Hashable.Hashable RequestNextTaskRequest

data RequestNextTaskResponse = RequestNextTaskResponse {requestNextTaskResponse_task_id :: (Prelude.Maybe TaskId), requestNextTaskResponse_task :: (Prelude.Maybe Data.ByteString.ByteString), requestNextTaskResponse_timeout_in_seconds :: (Prelude.Maybe Data.Int.Int32)}
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable RequestNextTaskResponse where
  type Tag RequestNextTaskResponse = Pinch.TStruct

  pinch (RequestNextTaskResponse requestNextTaskResponse_task_id requestNextTaskResponse_task requestNextTaskResponse_timeout_in_seconds) = Pinch.struct ([(1 Pinch.?= requestNextTaskResponse_task_id), (2 Pinch.?= requestNextTaskResponse_task), (3 Pinch.?= requestNextTaskResponse_timeout_in_seconds)])

  unpinch value = (((Prelude.pure (RequestNextTaskResponse) Prelude.<*> (value Pinch..:? 1)) Prelude.<*> (value Pinch..:? 2)) Prelude.<*> (value Pinch..:? 3))

instance Control.DeepSeq.NFData RequestNextTaskResponse

instance Data.Hashable.Hashable RequestNextTaskResponse

data TaskResult
  = TaskResult_Task_result Data.ByteString.ByteString
  | TaskResult_Error_message Data.Text.Text
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable TaskResult where
  type Tag TaskResult = Pinch.TUnion

  pinch (TaskResult_Task_result x) = Pinch.union (1) (x)
  pinch (TaskResult_Error_message x) = Pinch.union (2) (x)

  unpinch v = ((Control.Applicative.empty Control.Applicative.<|> (TaskResult_Task_result Prelude.<$> (v Pinch..: 1))) Control.Applicative.<|> (TaskResult_Error_message Prelude.<$> (v Pinch..: 2)))

instance Control.DeepSeq.NFData TaskResult

instance Data.Hashable.Hashable TaskResult

data HeartbeatRequest = HeartbeatRequest {heartbeatRequest_task_id :: (Prelude.Maybe TaskId), heartbeatRequest_task_result :: (Prelude.Maybe TaskResult)}
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable HeartbeatRequest where
  type Tag HeartbeatRequest = Pinch.TStruct

  pinch (HeartbeatRequest heartbeatRequest_task_id heartbeatRequest_task_result) = Pinch.struct ([(1 Pinch.?= heartbeatRequest_task_id), (2 Pinch.?= heartbeatRequest_task_result)])

  unpinch value = ((Prelude.pure (HeartbeatRequest) Prelude.<*> (value Pinch..:? 1)) Prelude.<*> (value Pinch..:? 2))

instance Control.DeepSeq.NFData HeartbeatRequest

instance Data.Hashable.Hashable HeartbeatRequest

data RequestNextTask_Args = RequestNextTask_Args {requestNextTask_Args_requestNextTaskRequest :: RequestNextTaskRequest}
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable RequestNextTask_Args where
  type Tag RequestNextTask_Args = Pinch.TStruct

  pinch (RequestNextTask_Args requestNextTask_Args_requestNextTaskRequest) = Pinch.struct ([(1 Pinch..= requestNextTask_Args_requestNextTaskRequest)])

  unpinch value = (Prelude.pure (RequestNextTask_Args) Prelude.<*> (value Pinch..: 1))

instance Control.DeepSeq.NFData RequestNextTask_Args

instance Pinch.Internal.RPC.ThriftResult RequestNextTask_Result where
  type ResultType RequestNextTask_Result = RequestNextTaskResponse

  unwrap (RequestNextTask_Result_Success x) = Prelude.pure (x)

  wrap m = Control.Exception.catches ((RequestNextTask_Result_Success Prelude.<$> m)) ([])

data RequestNextTask_Result
  = RequestNextTask_Result_Success RequestNextTaskResponse
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable RequestNextTask_Result where
  type Tag RequestNextTask_Result = Pinch.TUnion

  pinch (RequestNextTask_Result_Success x) = Pinch.union (0) (x)

  unpinch v = (Control.Applicative.empty Control.Applicative.<|> (RequestNextTask_Result_Success Prelude.<$> (v Pinch..: 0)))

instance Control.DeepSeq.NFData RequestNextTask_Result

data Heartbeat_Args = Heartbeat_Args {heartbeat_Args_heartbeat :: HeartbeatRequest}
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

instance Pinch.Pinchable Heartbeat_Args where
  type Tag Heartbeat_Args = Pinch.TStruct

  pinch (Heartbeat_Args heartbeat_Args_heartbeat) = Pinch.struct ([(1 Pinch..= heartbeat_Args_heartbeat)])

  unpinch value = (Prelude.pure (Heartbeat_Args) Prelude.<*> (value Pinch..: 1))

instance Control.DeepSeq.NFData Heartbeat_Args
