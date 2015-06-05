{-# LANGUAGE OverloadedStrings #-}
module Vga where

import Data.ByteString (ByteString)
import Network.HTTP.Conduit
import qualified Data.Conduit as C

push :: ByteString -> IO ()
push = pushTo "http://localhost:3000/push/html"

pushTo :: String -> ByteString -> IO ()
pushTo u b = do
    req <- parseUrl u
    let req' = req { method = "POST", requestBody = RequestBodyBS b }
    _ <- withManager $ \manager -> http req' manager
    return ()

