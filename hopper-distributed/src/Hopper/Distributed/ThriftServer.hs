{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hopper.Distributed.ThriftServer
  ( Settings (..),
    defaultSettings,
    runSettings,
    runSettingsSocket,
    runSettingsConnection,
    runSettingsChannelMaker,
  )
where

import Control.Concurrent (forkIOWithUnmask)
import Control.Exception
  ( AsyncException (..),
    allowInterrupt,
    bracket,
    catch,
    handle,
    mask_,
    try,
  )
import Data.Streaming.Network (HostPreference, bindPortTCP)
import qualified Data.Text.IO
import Foreign.C.Error (Errno (..), eCONNABORTED)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Socket
  ( SockAddr,
    Socket,
    SocketOption (NoDelay),
    accept,
    close,
    setCloseOnExecIfNeeded,
    setSocketOption,
    withFdSocket,
  )
import Pinch.Protocol.Compact (compactProtocol)
import Pinch.Server
  ( Channel (..),
    Context,
    ThriftServer,
    createChannel,
    runConnection,
  )
import Pinch.Transport
  ( Connection (..),
    Transport (..),
    unframedTransport,
  )
import System.IO.Error (ioeGetErrorType)
import qualified System.TimeManager

data Settings = Settings
  { port :: Int,
    host :: HostPreference,
    installShutdownHandler :: IO () -> IO (),
    timeout :: Int,
    accept :: Socket -> IO (Socket, SockAddr),
    createChannel :: forall connection. (Connection connection) => connection -> IO Channel,
    onException :: SomeException -> IO (),
    manager :: Maybe System.TimeManager.Manager,
    fork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
  }

defaultSettings :: Settings
defaultSettings =
  Settings
    { port = 4000,
      host = "*4",
      timeout = 30,
      installShutdownHandler = \_ -> pure (),
      accept = defaultAccept,
      createChannel = defaultCreateChannel,
      onException = defaultOnException,
      manager = Nothing,
      fork = defaultFork
    }

runSettings :: Settings -> Context -> ThriftServer -> IO ()
runSettings settings context server = do
  bracket
    (bindPortTCP settings.port settings.host)
    close
    ( \socket -> do
        withFdSocket socket setCloseOnExecIfNeeded
        runSettingsSocket settings socket context server
    )

runSettingsSocket :: Settings -> Socket -> Context -> ThriftServer -> IO ()
runSettingsSocket settings socket context server = do
  settings.installShutdownHandler closeListeningSocket
  runSettingsConnection settings getConnection context server
  where
    closeListeningSocket :: IO ()
    closeListeningSocket = close socket

    getConnection :: IO (Socket, IO ())
    getConnection = do
      (socket, _socketAddr) <- settings.accept socket
      withFdSocket socket setCloseOnExecIfNeeded
      -- NoDelay causes an error for AF_UNIX.
      setSocketOption socket NoDelay 1 `catch` \(_ :: SomeException) -> return ()
      pure (socket, close socket)

runSettingsConnection :: (Connection connection) => Settings -> IO (connection, IO ()) -> Context -> ThriftServer -> IO ()
runSettingsConnection settings@Settings {createChannel} getConnection context server = do
  runSettingsChannelMaker settings getChannel context server
  where
    getChannel :: IO (IO (Channel, IO ()))
    getChannel = do
      (connection, closeConnection) <- getConnection
      pure $ do
        channel <- createChannel connection
        pure (channel, closeConnection)

runSettingsChannelMaker :: Settings -> IO (IO (Channel, IO ())) -> Context -> ThriftServer -> IO ()
runSettingsChannelMaker !settings@Settings {fork = forkWithUnmask} getChannelMaker context server = do
  -- First mask all exceptions in acceptLoop. This is necessary to
  -- ensure that no async exception is throw between the call to
  -- acceptNewConnection and the registering of connClose.
  --
  -- acceptLoop can be broken by closing the listening socket.
  withTimeoutManager $ \manager ->
    mask_ (acceptLoop manager)
  where
    timeoutInSeconds =
      settings.timeout * 1000000

    withTimeoutManager action =
      case settings.manager of
        Just timerManager ->
          action timerManager
        Nothing ->
          bracket
            (System.TimeManager.initialize timeoutInSeconds)
            System.TimeManager.stopManager
            action

    acceptLoop manager = do
      -- Allow async exceptions before receiving the next connection maker.
      allowInterrupt
      mchannelMaker <- acceptNewConnection
      case mchannelMaker of
        Just channelMaker -> do
          fork manager channelMaker
          acceptLoop manager
        Nothing ->
          pure ()

    acceptNewConnection = do
      result <- try getChannelMaker
      case result of
        Right channelMaker -> do
          return (Just channelMaker)
        Left e -> do
          let eConnAborted = getErrno eCONNABORTED
              getErrno (Errno cInt) = cInt
          if ioe_errno e == Just eConnAborted
            then acceptNewConnection
            else do
              settings.onException (toException e)
              return Nothing

    fork manager channelMaker = forkWithUnmask $ \unmask ->
      -- Call the user-supplied on exception code if any
      -- exceptions are thrown.
      handle settings.onException $
        -- Run the connection maker to get a new connection, and ensure
        -- that the connection is closed. If the channelMaker call throws an
        -- exception, we will leak the connection.
        bracket channelMaker cleanup (serve manager unmask)

    cleanup (_channel, closeChannel) =
      closeChannel

    serve manager unmask (channel, closeChannel) =
      bracket register cancel $ \timer ->
        -- We now have fully registered a connection close handler in
        -- the case of all exceptions, so it is safe to once again
        -- allow async exceptions.
        unmask
          ( runConnection
              context
              server
              (wrapChannel timer channel)
          )
      where
        register =
          System.TimeManager.registerKillThread manager closeChannel

        cancel =
          System.TimeManager.cancel

defaultCreateChannel :: (Connection c) => c -> IO Channel
defaultCreateChannel connection =
  createChannel connection unframedTransport compactProtocol

defaultAccept :: Socket -> IO (Socket, SockAddr)
defaultAccept =
  accept

defaultFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
defaultFork action =
  void $ forkIOWithUnmask action

defaultOnException :: SomeException -> IO ()
defaultOnException exception =
  when (defaultShouldDisplayException exception) $
    Data.Text.IO.hPutStrLn stderr (show exception)

defaultShouldDisplayException :: SomeException -> Bool
defaultShouldDisplayException se
  | Just ThreadKilled <- fromException se = False
  | Just (ioeGetErrorType -> et) <- fromException se,
    et == ResourceVanished || et == InvalidArgument =
      False
  | Just System.TimeManager.TimeoutThread <- fromException se = False
  | otherwise = True

wrapTransport :: System.TimeManager.Handle -> Transport -> Transport
wrapTransport handle transport =
  transport
    { readMessage = \get -> do
        result <- readMessage transport get
        System.TimeManager.tickle handle
        pure result
    }

wrapChannel :: System.TimeManager.Handle -> Channel -> Channel
wrapChannel handle channel =
  channel
    { cTransportIn = wrapTransport handle channel.cTransportIn
    }
