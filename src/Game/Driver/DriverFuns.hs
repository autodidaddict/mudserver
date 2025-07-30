{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Game.Driver.DriverFuns where

import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import HsLua

import Game.Scripts.LuaCtx (LuaM)
import Game.Monad (GameM(..))
import Game.Types.CommandState (CommandState)

registerGameFunctions :: LuaM ()
registerGameFunctions = do
  pushDocumentedFunction factorial *> setglobal "factorial"

-- | Factorial function.
factorial :: DocumentedFunction e
factorial = defun "factorial"
  ### liftPure (\n -> product [1..n] :: Prelude.Integer)
  --                 get arg      type of arg      name  description
  <#> parameter      peekIntegral "integer"        "n"   "input number"
  =#> functionResult pushIntegral "integer|string"       "factorial of n"
  #? "Computes the factorial of an integer."