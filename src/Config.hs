-- Config.hs
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( ServerConfig(..)
  , defaultConfig
  ) where

import qualified Data.Text as T

-- | Configuration for the MUD server
data ServerConfig = ServerConfig
  { serverPort      :: String      -- ^ Port number to listen on
  , serverBacklog   :: Int         -- ^ Maximum length of the pending connections queue
  , welcomeMessage  :: T.Text      -- ^ Message shown to users after login
  , bufferSize      :: Int         -- ^ Size of buffer for reading from sockets
  , dataDirectory   :: String      -- ^ Directory for storing data files
  }

-- | Default server configuration
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
  { serverPort     = "3000"
  , serverBacklog  = 10
  , welcomeMessage = "Welcome to the MUD server! Type /help for available commands."
  , bufferSize     = 1024
  , dataDirectory  = "data"
  }