-- PasswordHash.hs
{-# LANGUAGE OverloadedStrings #-}

module PasswordHash
  ( hashPassword
  , verifyPassword
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Crypto.KDF.BCrypt as BCrypt

-- | Hash a password using BCrypt with a cost factor of 10
-- Returns the hashed password as Text
hashPassword :: T.Text -> IO T.Text
hashPassword password = do
  -- Convert Text to ByteString
  let passwordBS = TE.encodeUtf8 password
  -- Hash the password with cost factor 10
  hashedBS <- BCrypt.hashPassword 10 passwordBS
  -- Convert the hashed password back to Text
  return $ TE.decodeUtf8 hashedBS

-- | Verify a password against a stored hash
-- Returns True if the password matches the hash
verifyPassword :: T.Text -> T.Text -> Bool
verifyPassword password storedHash =
  -- Convert Text to ByteString
  let passwordBS = TE.encodeUtf8 password
      storedHashBS = TE.encodeUtf8 storedHash
  -- Validate the password against the stored hash
  in BCrypt.validatePassword passwordBS storedHashBS