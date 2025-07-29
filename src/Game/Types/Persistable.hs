{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.Persistable
  ( Persistable(..)
  , defaultSaveObject
  , defaultLoadObject
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (ToJSON, FromJSON, decode)
import qualified Data.Aeson.Encode.Pretty as AP
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)

-- | Typeclass for objects that can be persisted to and loaded from storage
class (ToJSON a, FromJSON a) => Persistable a where
  -- | Save an object to storage
  -- The String parameter is the data directory
  -- The Text parameter is the object identifier (e.g., username for Player)
  -- Returns Either String () to indicate success or failure
  saveObject :: String -> Text -> a -> IO (Either String ())
  
  -- | Load an object from storage
  -- The String parameter is the data directory
  -- The Text parameter is the object identifier (e.g., username for Player)
  -- Returns Either String a to indicate success or failure
  loadObject :: String -> Text -> IO (Either String a)

-- | Default implementation for saveObject
defaultSaveObject :: (ToJSON a) => String -> Text -> String -> a -> IO (Either String ())
defaultSaveObject dataDir identifier subDir obj = do
  let dirPath = dataDir </> subDir
      filePath = dirPath </> T.unpack identifier <> ".json"
      jsonData = AP.encodePretty obj
  
  result <- try $ do
    -- Create the directory if it doesn't exist
    createDirectoryIfMissing True dirPath
    -- Write the object data to the file
    BL.writeFile filePath jsonData
  
  case result of
    Left e -> return $ Left $ "Error saving object: " ++ show (e :: SomeException)
    Right _ -> return $ Right ()

-- | Default implementation for loadObject
defaultLoadObject :: (FromJSON a) => String -> Text -> String -> IO (Either String a)
defaultLoadObject dataDir identifier subDir = do
  let dirPath = dataDir </> subDir
      filePath = dirPath </> T.unpack identifier <> ".json"
  
  fileExists <- doesFileExist filePath
  if not fileExists
    then return $ Left $ "Object file not found: " ++ filePath
    else do
      result <- try $ do
        jsonData <- BL.readFile filePath
        case decode jsonData of
          Nothing -> return $ Left $ "Error parsing object data"
          Just obj -> return $ Right obj
      
      case result of
        Left e -> return $ Left $ "Error loading object: " ++ show (e :: SomeException)
        Right r -> return r