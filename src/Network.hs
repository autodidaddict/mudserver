-- Network.hs
{-# LANGUAGE OverloadedStrings #-}

module Network
  ( resolve
  , open
  , acceptLoop
  , recvLine
  , recvPasswordLine
  , sendText
  , sendLine
  , sendRawBytes
  , promptForInput
  , promptForPassword
  ) where

import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar)
import Data.Char (chr)

import Game.Types.Common (PlayerMap, ObjectsMap)
import Game.Types.Player (Player)
import Game.Scripts.ScriptMap (ScriptMap)
import Config (ServerConfig(..))

-- | Resolve the server address
resolve :: ServiceName -> IO AddrInfo
resolve port = do
  addrs <- getAddrInfo (Just hints) Nothing (Just port)
  case addrs of
    (addr:_) -> return addr
    [] -> error $ "No addresses found for port " ++ port
  where
    hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }

-- | Open a socket for listening
open :: AddrInfo -> Int -> IO Socket
open addr backlog = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock backlog
  return sock

-- | Accept and handle client connections
acceptLoop :: Socket -> TVar (PlayerMap Player) -> TVar ObjectsMap -> TVar ScriptMap -> ServerConfig -> (Socket -> TVar (PlayerMap Player) -> TVar ObjectsMap -> TVar ScriptMap -> ServerConfig -> IO ()) -> IO ()
acceptLoop sock playersTVar objectsTVar scriptMapTVar config clientHandler = forever $ do
  (conn, peer) <- accept sock
  putStrLn $ "Client connected from " ++ show peer
  forkIO $ clientHandler conn playersTVar objectsTVar scriptMapTVar config

-- | Receive one line (newline-terminated) from socket with improved efficiency
-- Filters out telnet IAC sequences, carriage returns, and control characters
-- Returns Nothing if client disconnected, Just lineStr otherwise
recvLine :: Socket -> Int -> IO (Maybe String)
recvLine sock bufSize = do
  bs <- NSB.recv sock bufSize
  if B.null bs
    then return Nothing  -- Client disconnected
    else 
      case B.elemIndex '\n' bs of
        Just idx -> do
          let (line, _) = B.splitAt idx bs
              -- Filter out telnet IAC sequences, carriage returns, and control characters
              filteredLine = filterTelnetIAC line
              -- Filter out carriage returns and control characters (ASCII < 32 except tab)
              cleanLine = B.filter (\c -> c == '\t' || c > '\31') filteredLine
              lineStr = B.unpack cleanLine
              
          -- We're not implementing buffer management for remaining data in this version
          -- But we've at least cleaned up control characters that could cause issues
          return (Just lineStr)
        Nothing -> do
          -- No newline found, keep reading
          rest <- recvLine sock bufSize
          case rest of
            Nothing -> return Nothing  -- Propagate disconnection
            Just restStr -> do
              -- Filter out telnet IAC sequences, carriage returns, and control characters
              let filteredBs = filterTelnetIAC bs
                  -- Filter out carriage returns and control characters (ASCII < 32 except tab)
                  cleanBs = B.filter (\c -> c == '\t' || c > '\31') filteredBs
              return $ Just $ B.unpack cleanBs ++ restStr

-- | Filter out telnet IAC sequences from a ByteString
-- IAC sequences start with byte 255 (0xFF)
filterTelnetIAC :: B.ByteString -> B.ByteString
filterTelnetIAC bs = 
  if B.null bs
    then bs
    else
      let (prefix, suffix) = B.span (/= '\255') bs
      in if B.null suffix
         then prefix
         else
           -- Skip the IAC byte and the command byte
           let remainingAfterIAC = 
                 if B.length suffix >= 2
                 then B.drop 2 suffix
                 else B.empty
           in B.append prefix (filterTelnetIAC remainingAfterIAC)

-- | Send text to a socket
sendText :: Socket -> T.Text -> IO ()
sendText sock text = NSB.sendAll sock (TE.encodeUtf8 text)

-- | Send a line of text to a socket (with CRLF)
sendLine :: Socket -> T.Text -> IO ()
sendLine sock text = sendText sock (text `T.append` "\r\n")

-- | Send raw bytes to a socket
sendRawBytes :: Socket -> B.ByteString -> IO ()
sendRawBytes = NSB.sendAll

-- | Prompt for input and handle disconnection
promptForInput :: Socket -> Int -> T.Text -> IO (Maybe String)
promptForInput sock bufSize prompt = do
  sendText sock prompt
  maybeInput <- recvLine sock bufSize
  
  case maybeInput of
    Nothing -> return Nothing  -- Client disconnected
    Just input -> do
      sendRawBytes sock $ B.pack "\r\n"
      return $ Just input

-- | Receive one line for password input (preserves all characters)
-- Similar to recvLine but doesn't filter out control characters
-- Returns Nothing if client disconnected, Just lineStr otherwise
recvPasswordLine :: Socket -> Int -> IO (Maybe String)
recvPasswordLine sock bufSize = do
  bs <- NSB.recv sock bufSize
  if B.null bs
    then return Nothing  -- Client disconnected
    else 
      case B.elemIndex '\n' bs of
        Just idx -> do
          let (line, _) = B.splitAt idx bs
              -- Filter out telnet IAC sequences only, preserve all other characters
              filteredLine = filterTelnetIAC line
              -- Filter out carriage returns only, preserve all other characters
              cleanLine = B.filter (/= '\r') filteredLine
              lineStr = B.unpack cleanLine
          return (Just lineStr)
        Nothing -> do
          -- No newline found, keep reading
          rest <- recvPasswordLine sock bufSize
          case rest of
            Nothing -> return Nothing  -- Propagate disconnection
            Just restStr -> do
              -- Filter out telnet IAC sequences only, preserve all other characters
              let filteredBs = filterTelnetIAC bs
                  -- Filter out carriage returns only, preserve all other characters
                  cleanBs = B.filter (/= '\r') filteredBs
              return $ Just $ B.unpack cleanBs ++ restStr

-- | Prompt for password without echoing characters
-- Uses telnet IAC sequences to suppress character echoing
promptForPassword :: Socket -> Int -> T.Text -> IO (Maybe String)
promptForPassword sock bufSize prompt = do
  -- Send IAC WILL ECHO to tell client we'll handle echoing (but we won't actually echo)
  sendRawBytes sock $ B.pack [chr 255, chr 251, chr 1]  -- IAC WILL ECHO
  
  -- Send the prompt
  sendText sock prompt
  
  -- Get the password without echoing, using special function that preserves all characters
  maybeInput <- recvPasswordLine sock bufSize
  
  -- Send IAC WONT ECHO to return to normal echoing
  sendRawBytes sock $ B.pack [chr 255, chr 252, chr 1]  -- IAC WONT ECHO
  
  case maybeInput of
    Nothing -> return Nothing  -- Client disconnected
    Just input -> do
      sendRawBytes sock $ B.pack "\r\n"
      return $ Just input