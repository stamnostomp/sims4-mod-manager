{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic
  ( ModInfo(..)
  , WindowState(..)
  , defaultWindowState
  , stateFilePath
  , loadWindowState
  , saveWindowState
  , findMods
  , scanMods
  , scanModsFrom
  , formatSize
  , getModsFolder
  , copyModToFolder
  , validateModsFolder
  , validateModFile
  ) where

import Control.Exception (SomeException, catch)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getFileSize
  , getHomeDirectory
  , getXdgDirectory
  , listDirectory
  , XdgDirectory(..)
  )
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)

data ModInfo = ModInfo
  { modName :: Text
  , modPath :: Text
  , modSize :: Integer
  , modType :: Text
  }

data WindowState = WindowState
  { wsWidth :: Int
  , wsHeight :: Int
  , wsPanedPosition :: Int
  , wsMaximized :: Bool
  , wsModPath :: String
  }

defaultWindowState :: WindowState
defaultWindowState = WindowState
  { wsWidth = 900
  , wsHeight = 600
  , wsPanedPosition = 720
  , wsMaximized = False
  , wsModPath = ""
  }

stateFilePath :: IO FilePath
stateFilePath = do
  configDir <- getXdgDirectory XdgConfig "sims4-mod-manager"
  return $ configDir </> "window-state.conf"

loadWindowState :: IO WindowState
loadWindowState = do
  path <- stateFilePath
  exists <- doesFileExist path
  if not exists
    then return defaultWindowState
    else do
      contents <- readFile path
      return $ parseWindowState contents

parseWindowState :: String -> WindowState
parseWindowState contents = foldl applyPair defaultWindowState pairs
  where
    pairs = map parseLine (lines contents)
    parseLine line = case break (== '=') line of
      (key, '=':val) -> (key, val)
      _              -> ("", "")
    applyPair ws ("width", val)          = ws { wsWidth = readDef (wsWidth ws) val }
    applyPair ws ("height", val)         = ws { wsHeight = readDef (wsHeight ws) val }
    applyPair ws ("paned-position", val) = ws { wsPanedPosition = readDef (wsPanedPosition ws) val }
    applyPair ws ("maximized", val)      = ws { wsMaximized = val == "true" }
    applyPair ws ("mod-path", val)       = ws { wsModPath = val }
    applyPair ws _                       = ws
    readDef d s = case reads s of
      [(v, "")] -> v
      _         -> d

saveWindowState :: FilePath -> WindowState -> IO ()
saveWindowState path ws = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path $ unlines
    [ "width=" ++ show (wsWidth ws)
    , "height=" ++ show (wsHeight ws)
    , "paned-position=" ++ show (wsPanedPosition ws)
    , "maximized=" ++ if wsMaximized ws then "true" else "false"
    , "mod-path=" ++ wsModPath ws
    ]

-- | Max recursion depth for mod scanning (prevents runaway traversal).
maxScanDepth :: Int
maxScanDepth = 10

-- | Recursively find all .package and .ts4script files under a directory.
findMods :: FilePath -> IO [ModInfo]
findMods root = do
  exists <- doesDirectoryExist root
  if not exists
    then return []
    else go 0 root
  where
    go depth dir
      | depth > maxScanDepth = return []
      | otherwise = do
          entries <- listDirectory dir `catch` (\(_ :: SomeException) -> return [])
          fmap concat $ mapM (processEntry depth dir) entries

    processEntry depth dir entry = do
      let full = dir </> entry
      isDir <- doesDirectoryExist full
      if isDir
        then go (depth + 1) full
        else case takeExtension entry of
          ext | ext == ".package" || ext == ".ts4script" -> do
            size <- getFileSize full `catch` (\(_ :: SomeException) -> return 0)
            return
              [ ModInfo
                  { modName = T.pack (takeFileName entry)
                  , modPath = T.pack full
                  , modSize = size
                  , modType = T.pack (drop 1 ext)
                  }
              ]
          _ -> return []

-- | Format a file size for display.
formatSize :: Integer -> Text
formatSize bytes
  | bytes >= 1048576 = T.pack (show (bytes `div` 1048576)) <> " MB"
  | bytes >= 1024    = T.pack (show (bytes `div` 1024)) <> " KB"
  | otherwise        = T.pack (show bytes) <> " B"

-- | Default Sims 4 Mods directory candidates.
defaultModPaths :: IO [FilePath]
defaultModPaths = do
  home <- getHomeDirectory
  return
    [ home </> "Documents" </> "Electronic Arts" </> "The Sims 4" </> "Mods"
    , home </> ".local" </> "share" </> "The Sims 4" </> "Mods"
    ]

-- | Scan both common Sims 4 Mods directories and return all mods found.
scanMods :: IO [ModInfo]
scanMods = do
  paths <- defaultModPaths
  fmap concat $ mapM findMods paths

-- | Scan mods from a custom path, or default locations if empty.
scanModsFrom :: String -> IO [ModInfo]
scanModsFrom "" = scanMods
scanModsFrom path = findMods path

-- | Resolve the effective mods directory: custom path if set, otherwise first
-- existing default directory.
getModsFolder :: String -> IO FilePath
getModsFolder customPath
  | not (null customPath) = do
      createDirectoryIfMissing True customPath
      return customPath
  | otherwise = do
      paths <- defaultModPaths
      findFirst paths
  where
    findFirst [] = do
      -- None exist yet; create and return the first default
      (p:_) <- defaultModPaths
      createDirectoryIfMissing True p
      return p
    findFirst (p:ps) = do
      exists <- doesDirectoryExist p
      if exists then return p else findFirst ps

-- | Copy a mod file into the target mods folder and return its ModInfo.
copyModToFolder :: FilePath -> FilePath -> IO ModInfo
copyModToFolder srcFile modsDir = do
  let fileName = takeFileName srcFile
      destFile = modsDir </> fileName
  copyFile srcFile destFile
  size <- getFileSize destFile
  let ext = takeExtension fileName
  return ModInfo
    { modName = T.pack fileName
    , modPath = T.pack destFile
    , modSize = size
    , modType = T.pack (drop 1 ext)
    }

-- | Validate a mods folder path. Returns Right path if valid, Left error if not.
validateModsFolder :: String -> IO (Either Text String)
validateModsFolder "" = return (Right "")
validateModsFolder path = do
  home <- getHomeDirectory
  let blocked = ["/", "/tmp", "/home", home]
  if path `elem` blocked
    then return (Left "That directory is too broad. Please choose a specific mods folder.")
    else do
      exists <- doesDirectoryExist path
      if not exists
        then return (Left $ "Directory not found: " <> T.pack path)
        else return (Right path)

-- | Validate a mod file path. Returns Right path if valid, Left error if not.
validateModFile :: FilePath -> IO (Either Text FilePath)
validateModFile path = do
  exists <- doesFileExist path
  if not exists
    then return (Left $ "File not found: " <> T.pack path)
    else do
      let ext = takeExtension path
      if ext /= ".package" && ext /= ".ts4script"
        then return (Left "Only .package and .ts4script files are supported.")
        else return (Right path)
