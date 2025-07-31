-- Input.hs
{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.Input
  ( clientHandler
  , promptUsername
  , promptPassword
  , handleClient
  , commandLoop
  , handleCommand
  ) where

import Network.Socket (Socket, close)
import qualified Data.Text as T

import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (unless)
import Control.Monad.State (gets)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map

import Game.Types.Player (PlayerName(..), PlayerMap)
import Game.Monad (GameM, CommandState(..), runGameM, writeLine, amWizard, getCommandState)
import Game.World.GameObjects (addObject, deleteObject, addObjectToEnvironment)
import Game.Actions.SlashCommands (handleSlashCommand)
import Game.Actions.WizardCommands (handleWizardCommand)
import Game.Scripts.ScriptMap (ScriptMap)
import Game.Scripts.Lua (loadPrototype)
import Config (ServerConfig(..))
import Network (recvLine, sendLine, promptForInput, promptForPassword)
import Game.Types.Player (savePlayer, loadPlayer, mkDefaultPlayer, Player(playerBase, playerPasswordHash))
import Game.Types.Object (ObjectRef(..), SomeObjectRef(..), objRef, objEnv, getProto, ObjectsMap)
import PasswordHash (hashPassword, verifyPassword)

-- | Handle a client connection
clientHandler :: Socket -> TVar (PlayerMap Player) -> TVar ObjectsMap -> TVar ScriptMap -> ServerConfig -> IO ()
clientHandler conn playersTVar objectsTVar scriptMapTVar config = 
  finally (handleClient conn playersTVar objectsTVar scriptMapTVar config) (handleDisconnect conn)
  where
    handleDisconnect sock = do
      putStrLn "Client disconnected."
      close sock

-- | Prompt for username and handle disconnection
promptUsername :: Socket -> Int -> IO (Maybe PlayerName)
promptUsername conn bufSize = do
  maybeNameStr <- promptForInput conn bufSize "Enter your name: "
  case maybeNameStr of
    Nothing -> do
      putStrLn "Client disconnected during name prompt."
      return Nothing
    Just nameStr -> return $ Just $ PlayerName (T.pack nameStr)

-- | Prompt for password and handle disconnection
promptPassword :: Socket -> Int -> IO (Maybe String)
promptPassword conn bufSize = do
  maybePasswd <- promptForPassword conn bufSize "Enter your password: "
  case maybePasswd of
    Nothing -> do
      putStrLn "Client disconnected during password prompt."
      return Nothing
    Just passwd -> return $ Just passwd

-- | Main client handling logic
handleClient :: Socket -> TVar (PlayerMap Player) -> TVar ObjectsMap -> TVar ScriptMap -> ServerConfig -> IO ()
handleClient conn playersTVar objectsTVar scriptMapTVar config = do
  -- Ask for name
  maybeName <- promptUsername conn (bufferSize config)
  
  case maybeName of
    Nothing -> return ()
    Just name -> do
      -- Ask for password
      maybePasswd <- promptPassword conn (bufferSize config)
      
      case maybePasswd of
        Nothing -> return ()
        Just passwd -> do
          -- Only proceed with login if we have both name and password
          putStrLn $ "User logged in: " ++ T.unpack (unPlayerName name)

          -- Try to load existing player or create a new one
          let username = unPlayerName name
              startingRoom = RoomRef "std.room.thevoid"
              playerRef = InstRef "std.player" username
          
          -- Hash the password
          passwordHash <- hashPassword (T.pack passwd)
          
          -- Try to load player from file
          playerResult <- loadPlayer (dataDirectory config) username
          
          -- Handle player authentication and login
          case playerResult of
            Right existingPlayer -> do
              putStrLn $ "Loaded player data for: " ++ T.unpack username
              -- Validate the password
              let storedHash = playerPasswordHash existingPlayer
                  isValid = verifyPassword (T.pack passwd) storedHash
              if not isValid
                then do
                  -- Invalid password - reject login and exit early
                  putStrLn $ "Invalid password for: " ++ T.unpack username
                  sendLine conn "Invalid password. Please try again."
                  close conn
                  return () -- Exit the function early
                else do
                  -- Valid password - continue with login
                  putStrLn $ "Password validated for: " ++ T.unpack username
                  let playerObj = existingPlayer
                  
                  -- Continue with login process
                  loginPlayer conn name playerObj playersTVar objectsTVar scriptMapTVar config
            
            Left err -> do
              -- Create new player
              putStrLn $ "Creating new player: " ++ T.unpack username ++ " (Reason: " ++ err ++ ")"
              let playerObj = mkDefaultPlayer username passwordHash startingRoom playerRef
              
              -- Continue with login process
              loginPlayer conn name playerObj playersTVar objectsTVar scriptMapTVar config

-- | Handle the login process for a player
loginPlayer :: Socket -> PlayerName -> Player -> TVar (PlayerMap Player) -> TVar ObjectsMap -> TVar ScriptMap -> ServerConfig -> IO ()
loginPlayer conn name playerObj playersTVar objectsTVar scriptMapTVar config = do
  -- Create a TVar for the Player object
  playerTVar <- atomically $ newTVar playerObj
  
  -- Create CommandState with the TVar Player object and server config
  let st = CommandState conn name playerTVar playersTVar objectsTVar scriptMapTVar config

  -- Add player to the global player list with both socket and Player object
  runGameM st $ do
    pl <- gets playerList
    playerObj' <- liftIO $ atomically $ readTVar playerTVar
    liftIO $ atomically $ modifyTVar' pl (Map.insert name (conn, playerObj'))
    
    -- Add player to the global objects map
    let baseObj = playerBase playerObj'
    addObject (objRef baseObj) baseObj

    -- Get the player's environment reference and prototype
    let envRef = objEnv baseObj
        protoName = getProto envRef
    
    -- Load the environment's prototype script before adding the player to it
    cmdState <- getCommandState
    scriptMapTVar <- gets scriptMap
    scriptMapVal <- liftIO $ readTVarIO scriptMapTVar
    result <- loadPrototype cmdState protoName scriptMapVal
    
    -- Update the script map if loading was successful
    case result of
      Right updatedScriptMap -> 
        liftIO $ atomically $ modifyTVar' scriptMapTVar (const updatedScriptMap)
      Left err ->
        liftIO $ putStrLn $ "Warning: Could not load prototype " ++ T.unpack protoName ++ ": " ++ err
    
    -- Add player to their environment's inventory using the common function
    let playerObjRef = objRef baseObj
    success <- addObjectToEnvironment (SomeRef playerObjRef)
    
    -- Log a warning if the player couldn't be added to their environment
    unless success $
      liftIO $ putStrLn $ "Warning: Could not add player " ++ T.unpack (unPlayerName name) ++ " to their environment"

  -- Welcome message
  runGameM st $ do
    writeLine ""
    writeLine $ "Welcome, " <> unPlayerName name <> "!"
    writeLine "Type /help for available commands."

  -- Main command loop
  commandLoop st

-- | Main command processing loop
commandLoop :: CommandState -> IO ()
commandLoop st = do
  let loop = do
        maybeMsgStr <- recvLine (clientSocket st) 1024
        case maybeMsgStr of
          Nothing -> disconnect  -- Client actually disconnected
          Just msgStr -> do
            let msg = T.pack msgStr
            keepGoing <- runGameM st $ handleCommand msg
            if keepGoing then loop else disconnect

      disconnect = do
        let name = playerName st
            playerTVar = playerObject st
        
        -- Read the player object from the TVar
        player <- atomically $ readTVar playerTVar
        
        -- Save player data to file
        saveResult <- savePlayer (dataDirectory (serverConfig st)) player
        case saveResult of
          Right _ -> putStrLn $ "Saved player data for: " ++ T.unpack (unPlayerName name)
          Left err -> putStrLn $ "Error saving player data: " ++ err
        
        putStrLn (T.unpack (unPlayerName name) ++ " disconnected.")
        runGameM st $ do
          -- Remove player from the global player list
          pl <- gets playerList
          liftIO $ atomically $ modifyTVar' pl (Map.delete name)
          
          -- Use deleteObject to properly remove the player from their environment and the global objects map
          let playerObjRef = objRef (playerBase player)
          deleteObject (SomeRef playerObjRef)
        close (clientSocket st)

  loop

-- | Handle a command from the user
handleCommand :: T.Text -> GameM Bool
handleCommand msg
  | T.null msg = return True
  | T.head msg == '/' = handleSlashCommand (T.drop 1 msg)
  | T.head msg == '@' = do
      isWizard <- amWizard
      if isWizard
        then handleWizardCommand (T.drop 1 msg)
        else do
          writeLine "You don't have permission to use wizard commands."
          return True
  | otherwise = do
      writeLine ("You said: " <> msg)
      return True