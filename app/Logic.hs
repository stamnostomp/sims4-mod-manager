{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic
  ( ModInfo(..)
  , PackageInfo(..)
  , WindowState(..)
  , defaultWindowState
  , stateFilePath
  , loadWindowState
  , saveWindowState
  , findMods
  , scanMods
  , scanModsFrom
  , formatSize
  , formatPackageInfo
  , getModsFolder
  , copyModToFolder
  , validateModsFolder
  , validateModFile
  , importMod
  ) where

import Control.Exception (SomeException, catch)
import Data.Bits ((.&.), shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
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

data PackageInfo = PackageInfo
  { pkgVersion       :: (Int, Int)      -- DBPF major.minor
  , pkgResourceCount :: Int             -- total index entries
  , pkgResourceTypes :: [(Text, Int)]   -- [(human-readable type name, count)]
  , pkgCategory      :: Text            -- inferred: "Custom Content", "Tuning Mod", etc.
  }

data ModInfo = ModInfo
  { modName        :: Text
  , modPath        :: Text
  , modSize        :: Integer
  , modType        :: Text
  , modPackageInfo :: Maybe PackageInfo
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
            pkgInfo <- if ext == ".package"
              then parsePackageFile full
              else return Nothing
            return
              [ ModInfo
                  { modName = T.pack (takeFileName entry)
                  , modPath = T.pack full
                  , modSize = size
                  , modType = T.pack (drop 1 ext)
                  , modPackageInfo = pkgInfo
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
  pkgInfo <- if ext == ".package"
    then parsePackageFile destFile
    else return Nothing
  return ModInfo
    { modName = T.pack fileName
    , modPath = T.pack destFile
    , modSize = size
    , modType = T.pack (drop 1 ext)
    , modPackageInfo = pkgInfo
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

-- | Validate, copy a mod file into the configured mods folder, and return
-- the new ModInfo. Returns Left with an error message on failure.
importMod :: FilePath -> String -> IO (Either Text ModInfo)
importMod filePath customModPath = do
  result <- validateModFile filePath
  case result of
    Left err -> return (Left err)
    Right validPath -> do
      modsDir <- getModsFolder customModPath
      modInfo <- copyModToFolder validPath modsDir
      return (Right modInfo)

-- ---------------------------------------------------------------------------
-- DBPF (.package) parsing
-- ---------------------------------------------------------------------------

-- | Read a little-endian Word32 from 4 bytes.
getWord32LE :: BS.ByteString -> Int -> Word32
getWord32LE bs off =
  let b i = fromIntegral (BS.index bs (off + i)) :: Word32
  in  b 0 .|. (b 1 `shiftL` 8) .|. (b 2 `shiftL` 16) .|. (b 3 `shiftL` 24)

-- | Parse a .package (DBPF) file and extract structural info.
parsePackageFile :: FilePath -> IO (Maybe PackageInfo)
parsePackageFile path = (do
  bs <- BS.readFile path
  if BS.length bs < 96
    then return Nothing
    else do
      -- Verify magic "DBPF"
      let magic = BS.take 4 bs
      if magic /= BS.pack [0x44, 0x42, 0x50, 0x46]  -- "DBPF"
        then return Nothing
        else do
          let major      = fromIntegral (getWord32LE bs 4)  :: Int
              minor      = fromIntegral (getWord32LE bs 8)  :: Int
              indexCount = fromIntegral (getWord32LE bs 36) :: Int
              -- In DBPF 2.0, index position is at offset 64
              indexPos   = fromIntegral (getWord32LE bs 64) :: Int

          -- Parse index entries to extract type IDs
          typeIds <- parseIndex bs indexPos indexCount
          let grouped   = countTypes typeIds
              named     = map (\(tid, cnt) -> (resourceTypeName tid, cnt)) grouped
              sorted    = List.sortBy (\a b' -> compare (snd b') (snd a)) named
              category  = inferCategory (map fst grouped)

          return $ Just PackageInfo
            { pkgVersion       = (major, minor)
            , pkgResourceCount = indexCount
            , pkgResourceTypes = sorted
            , pkgCategory      = category
            }
  ) `catch` (\(_ :: SomeException) -> return Nothing)

-- | Parse index table and extract type IDs.
-- DBPF 2.0 index has a flags field, then entries with conditional fields.
parseIndex :: BS.ByteString -> Int -> Int -> IO [Word32]
parseIndex bs indexPos indexCount
  | indexPos + 4 > BS.length bs = return []
  | indexCount <= 0             = return []
  | otherwise = do
      let indexFlags = getWord32LE bs indexPos
          startOff   = indexPos + 4
      return $ readEntries bs startOff indexFlags indexCount

-- | Read index entries based on the index flags.
-- The flags word determines which fields are constant (shared) vs per-entry.
readEntries :: BS.ByteString -> Int -> Word32 -> Int -> [Word32]
readEntries bs startOff indexFlags count =
  let -- Determine constant header fields from flags
      -- Bit 0: type is constant, Bit 1: group is constant,
      -- Bit 2: instance-hi is constant
      typeConst   = indexFlags .&. 1 /= 0
      groupConst  = indexFlags .&. 2 /= 0
      instHiConst = indexFlags .&. 4 /= 0

      -- Read constant values from the header area
      (constType, off1) = if typeConst
        then (getWord32LE bs startOff, startOff + 4)
        else (0, startOff)
      (_constGroup, off2) = if groupConst
        then (getWord32LE bs off1, off1 + 4)
        else (0 :: Word32, off1)
      (_constInstHi, entriesOff) = if instHiConst
        then (getWord32LE bs off2, off2 + 4)
        else (0 :: Word32, off2)

      -- Each entry has: [type] [group] [instHi] instLo offset filesize memsize compressed iscomp
      -- Fields in brackets are omitted when constant.
      entrySize = 4 * (  (if typeConst then 0 else 1)
                        + (if groupConst then 0 else 1)
                        + (if instHiConst then 0 else 1)
                        + 5 )  -- instLo + offset + filesize + memsize + compressed+flags

      readEntry off
        | off + entrySize > BS.length bs = Nothing
        | typeConst = Just (constType, off + entrySize)
        | otherwise = Just (getWord32LE bs off, off + entrySize)

      go _ 0 acc = acc
      go off n acc = case readEntry off of
        Nothing           -> acc
        Just (tid, off')  -> go off' (n - 1) (tid : acc)

  in go entriesOff count []

-- | Count occurrences of each type ID.
countTypes :: [Word32] -> [(Word32, Int)]
countTypes = foldr (\g acc -> case g of
  (x:_) -> (x, length g) : acc
  []    -> acc) [] . List.group . List.sort

-- | Map a DBPF resource Type ID to a human-readable name.
resourceTypeName :: Word32 -> Text
resourceTypeName tid = case tid of
  0x220557DA -> "String Table"
  0x034AEECB -> "CAS Part"
  0x015A1849 -> "Geometry"
  0x00B2D882 -> "Mesh"
  0x3453CF95 -> "DST Texture"
  0x00AE6C67 -> "Bone Delta"
  0xC0DB5AE7 -> "Object Definition"
  0x545AC67A -> "Object Catalog"
  0x0333406C -> "XML Tuning"
  0x62ECC59A -> "Animation (CLIP)"
  0xA8D58BE5 -> "Jazz State Machine"
  0xD3044521 -> "Region Map"
  0x0166038C -> "Texture Compositor"
  0xB61DE6B4 -> "Texture (DDS)"
  0x2F7D0004 -> "Thumbnail"
  0xD382BF57 -> "Lot Tuning"
  0x6B20C4F3 -> "Swatch"
  _          -> "0x" <> T.pack (showHexWord32 tid)

-- | Show a Word32 as an 8-digit uppercase hex string.
showHexWord32 :: Word32 -> String
showHexWord32 w = go 8 w ""
  where
    go 0 _ acc = acc
    go n v acc =
      let (q, r) = v `divMod` 16
          c = "0123456789ABCDEF" !! fromIntegral r
      in go (n - 1 :: Int) q (c : acc)

-- | Infer a mod category from the resource types present.
inferCategory :: [Word32] -> Text
inferCategory typeIds
  | 0x034AEECB `elem` typeIds = "Custom Content"    -- CAS Part
  | 0xA8D58BE5 `elem` typeIds = "Animation Mod"     -- Jazz State Machine
  | 0x62ECC59A `elem` typeIds = "Animation Mod"     -- CLIP
  | 0x0333406C `elem` typeIds = "Tuning Mod"        -- XML Tuning
  | 0xC0DB5AE7 `elem` typeIds = "Object Mod"        -- Object Definition
  | otherwise                  = "Mod Package"

-- | Format PackageInfo as display text for the details panel.
formatPackageInfo :: PackageInfo -> Text
formatPackageInfo info =
  let countLine = T.pack (show (pkgResourceCount info)) <> " resources"
      catLine   = pkgCategory info
      typesLine = T.intercalate ", "
        [ T.pack (show cnt) <> " " <> name
        | (name, cnt) <- pkgResourceTypes info
        ]
  in catLine <> "\n" <> countLine <> "\n" <> typesLine
