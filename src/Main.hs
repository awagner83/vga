{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_,
                           forkIO)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS
import qualified Web.Scotty as S

-- List of connected clients
type ServerState = IntMap WS.Connection


-- | Startup application
main :: IO ()
main = do
    state <- newMVar newServerState

    -- Start up REST interface that interacts with websocket clients
    _ <- forkIO $ S.scotty 3000 $ do
        S.get "/status" $ do
            n <- liftIO $ numConnections state
            S.html $ mconcat ["Running; ", showLT n, " connected clients"]
        S.post "/push" $ do
            payload <- S.body
            liftIO $ broadcast (showT payload) state

    -- Start WebSocket server
    WS.runServer "0.0.0.0" 9160 $ application state

-- | Main application instance
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    idx <- modifyMVar state (return . addConnection conn)

    flip finally (disconnect state idx) $ do
        clients <- readMVar state
        broadcast ("New client connected! (" <> (showT idx) <>")") state
        forever $ receiveText conn >> return ()  -- Keep it alive!

-- | Handle client disconnect
disconnect :: MVar ServerState -> Int -> IO ()
disconnect state idx = do
    modifyMVar_ state (return . removeConnection idx)
    broadcast ("Client " <> (showT idx) <> " disconnected") state

-- | Create empty server stat
newServerState :: ServerState
newServerState = M.empty

-- | Add connection to ServerState and return the new connection id
addConnection :: WS.Connection -> ServerState -> (ServerState, Int)
addConnection x xs = let k = M.size xs in (M.insert k x xs, k)

-- | Remove a connection from the server state
removeConnection :: Int -> ServerState -> ServerState
removeConnection = M.delete

-- | Read number of connections from server-state
numConnections :: MVar ServerState -> IO Int
numConnections s = M.size <$> readMVar s

-- | Send message to all clients
broadcast :: Text -> MVar ServerState -> IO ()
broadcast message state = do
    T.putStrLn message      -- output to the console
    clients <- readMVar state
    forM_ (M.toList clients) $ \(_, conn) -> WS.sendTextData conn message


{-- Utilities --}

showT :: Show a => a -> Text
showT = T.pack . show

showLT :: Show a => a -> LT.Text
showLT = LT.pack . show

receiveText :: WS.Connection -> IO Text
receiveText = WS.receiveData

