{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar, modifyMVar_,
                           forkIO)
import Control.Exception (finally)
import Control.Monad (forM_, forever, mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import Data.IntMap (IntMap)
import Data.Aeson (Value(..), FromJSON, ToJSON, (.:), (.=))
import Data.ByteString.Lazy (toStrict)
import qualified Data.Aeson as A
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import qualified Network.WebSockets as WS
import qualified Web.Scotty as S

-- List of connected clients
type ServerState = IntMap WS.Connection

data Message = Message MessageType Text deriving Show
data MessageType = Text | HTML deriving Show

instance FromJSON MessageType where
    parseJSON (String x) | x == "Text" = return Text
                         | x == "HTML" = return HTML
    parseJSON _                        = mzero

instance ToJSON MessageType where
    toJSON HTML = "HTML"
    toJSON Text = "Text"

instance FromJSON Message where
    parseJSON (Object v) = Message <$> v .: "type" <*> v .: "data"
    parseJSON _          = mzero

instance ToJSON Message where
    toJSON (Message t d) = A.object ["type" .= t, "data" .= d]

-- | Startup application
main :: IO ()
main = do
    state <- newMVar newServerState

    -- Start up REST interface that interacts with websocket clients
    _ <- forkIO $ S.scotty 3000 $ do
        S.get "/" $ do
            clientHtml <- liftIO $ LT.readFile "client.html"
            S.html clientHtml

        S.get "/status" $ do
            n <- liftIO $ numConnections state
            S.html $ mconcat ["Running; ", showLT n, " connected clients"]

        S.post "/push/html" $ do
            payload <- S.body
            liftIO $ broadcast HTML (T.decodeUtf8 $ toStrict payload) state

        S.post "/push" $ do
            payload <- S.body
            liftIO $ broadcast Text (showT payload) state

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
        broadcast Text ("New client connected! (" <> (showT idx) <>")") state
        forever $ receiveText conn >> return ()  -- Keep it alive!

-- | Handle client disconnect
disconnect :: MVar ServerState -> Int -> IO ()
disconnect state idx = do
    modifyMVar_ state (return . removeConnection idx)
    broadcast Text ("Client " <> (showT idx) <> " disconnected") state

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
broadcast :: MessageType -> Text -> MVar ServerState -> IO ()
broadcast t message state = do
    let message' = A.encode (Message t message)
    T.putStrLn message      -- output to the console
    clients <- readMVar state
    forM_ (M.toList clients) $ \(_, conn) -> WS.sendTextData conn message'


{-- Utilities --}

showT :: Show a => a -> Text
showT = T.pack . show

showLT :: Show a => a -> LT.Text
showLT = LT.pack . show

receiveText :: WS.Connection -> IO Text
receiveText = WS.receiveData

