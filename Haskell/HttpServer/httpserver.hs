{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Concurrent
import Control.Parallel.Strategies

import Data.Text (Text)
import qualified Data.Text     as Text
import qualified Data.Text.IO  as Text

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

import Data.Maybe
import Data.String (fromString)
import Network
import System.IO

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

hGetLines :: Handle -> IO [Text]
hGetLines hdl = do
  line <- Text.hGetLine hdl
  if Text.null line
  then return []
  else do
    rest <- hGetLines hdl
    return $ line : rest

acceptHandle :: Socket -> IO Handle
acceptHandle serverSocket = do
  (hdl, _, _) <- accept serverSocket
  hSetBuffering hdl NoBuffering
  hSetNewlineMode hdl $ NewlineMode CRLF CRLF
  return hdl
 
response :: Text -> Text
response str = Text.unlines
  [ "HTTP/1.0 200 OK"
  , "Connection: close"
  , "Content-Type: text/plain"
  , ""
  , str
  ]

acceptFork :: Socket -> (Handle -> IO ()) -> IO ()
acceptFork socket action = do
  connection <- acceptHandle socket
  forkIO (action connection)
  acceptFork socket action

--handleClient :: MVar (Int, Int) -> Handle -> IO ()
requestedResource :: [Text] -> Maybe Text
requestedResource [] = Nothing
requestedResource (x:xs) = do
  let request = Text.words x
  if length request < 3 || Text.toUpper (request !! 0) /= Text.pack "GET" || head (Text.unpack (request !! 1)) /= '/'
    then Nothing
    else Text.stripPrefix (Text.pack "/") (request !! 1)

--handleRequest :: Text -> (Int, Int) -> (Int, Int)
--drawState :: (Int, Int) -> Text
--main :: IO ()

-- TESTS

test_requestedResource =
  [ requestedResource [""]                      == Nothing
  , requestedResource ["ALMA"]                  == Nothing
  , requestedResource ["GET"]                   == Nothing
  , requestedResource ["GET /index.html"]       == Nothing
  , requestedResource ["ALMA /index.html alma"] == Nothing
  , requestedResource ["GET /index.html alma"]  == Just "index.html"
  , requestedResource ["get /index.html alma"]  == Just "index.html"
  , requestedResource ["GET /alma.html HTTP/1.1", "Korte: very"] == Just "alma.html"
  ]

{-
test_handleRequest :: [Bool]
test_handleRequest =
  [ handleRequest "n" (2,3) == (2,2)
  , handleRequest "s" (2,3) == (2,4)
  , handleRequest "e" (2,3) == (3,3)
  , handleRequest "w" (2,3) == (1,3)
  , handleRequest "foobar" (2,3) == (2,3)
  ]
-}
{-
test_drawState :: [Bool]
test_drawState =
  [ drawState (-1, -1) ==
    "..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \(-1,-1)"
  , drawState (1, 5) ==
    "..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \X.........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \(1,5)"
  ]
-}