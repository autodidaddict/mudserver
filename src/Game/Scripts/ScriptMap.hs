-- ScriptMap.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Game.Scripts.ScriptMap
  ( ScriptMap
 , emptyScriptMap
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import qualified HsLua as Lua
-- | Type alias for the script map
-- Maps prototype names to Lua states
type ScriptMap = Map.Map T.Text Lua.State

-- | Create an empty script map
emptyScriptMap :: ScriptMap
emptyScriptMap = Map.empty
