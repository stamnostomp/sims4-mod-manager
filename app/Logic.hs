{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic
  ( ModInfo(..)
  , PackageInfo(..)
  , ModProfile(..)
  , WindowState(..)
  , defaultWindowState
  , stateFilePath
  , loadWindowState
  , saveWindowState
  , profilesFilePath
  , loadProfiles
  , saveProfiles
  , makeProfileFromCurrent
  , applyProfile
  , enableAllMods
  , disableAllMods
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
  , importZip
  , importAny
  , defaultWinePrefixPaths
  , runInWinePrefix
  , disableMod
  , enableMod
  , removeMod
  ) where

import Codec.Archive.Zip (toArchive, zEntries, eRelativePath, fromEntry)
import Control.Exception (SomeException, catch)
import System.Environment (getEnvironment)
import System.Process (createProcess, proc, CreateProcess(..))
import Data.Bits ((.&.), shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , getFileSize
  , getHomeDirectory
  , getXdgDirectory
  , listDirectory
  , removeFile
  , renameFile
  , XdgDirectory(..)
  )
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName, dropExtension)

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
  , modEnabled     :: Bool
  }

data WindowState = WindowState
  { wsWidth :: Int
  , wsHeight :: Int
  , wsPanedPosition :: Int
  , wsMaximized :: Bool
  , wsModPath :: String
  , wsWinePrefix :: String
  , wsWineExe :: String   -- custom wine binary path/name (empty = auto-detect)
  }

defaultWindowState :: WindowState
defaultWindowState = WindowState
  { wsWidth = 900
  , wsHeight = 600
  , wsPanedPosition = 720
  , wsMaximized = False
  , wsModPath = ""
  , wsWinePrefix = ""
  , wsWineExe = ""
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
    applyPair ws ("wine-prefix", val)    = ws { wsWinePrefix = val }
    applyPair ws ("wine-exe", val)       = ws { wsWineExe = val }
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
    , "wine-prefix=" ++ wsWinePrefix ws
    , "wine-exe=" ++ wsWineExe ws
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
        else case classifyMod entry of
          Just (ext, enabled) -> do
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
                  , modEnabled = enabled
                  }
              ]
          Nothing -> return []

    -- | Classify a filename as a mod.  Returns the base extension and
    -- whether it is enabled.
    classifyMod :: FilePath -> Maybe (String, Bool)
    classifyMod name
      | takeExtension name == ".package"   = Just (".package", True)
      | takeExtension name == ".ts4script" = Just (".ts4script", True)
      | takeExtension name == ".disabled"  =
          let base = dropExtension name
              baseExt = takeExtension base
          in if baseExt == ".package" || baseExt == ".ts4script"
               then Just (baseExt, False)
               else Nothing
      | otherwise = Nothing

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
    , modEnabled = True
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

-- | Disable a mod by appending ".disabled" to its filename.
disableMod :: FilePath -> IO (Either Text FilePath)
disableMod path = do
  let newPath = path ++ ".disabled"
  (renameFile path newPath >> return (Right newPath))
    `catch` (\(e :: SomeException) ->
      return (Left $ "Failed to disable mod: " <> T.pack (show e)))

-- | Enable a mod by removing the trailing ".disabled" from its filename.
enableMod :: FilePath -> IO (Either Text FilePath)
enableMod path
  | takeExtension path /= ".disabled" =
      return (Left "File is not disabled.")
  | otherwise = do
      let newPath = dropExtension path
      (renameFile path newPath >> return (Right newPath))
        `catch` (\(e :: SomeException) ->
          return (Left $ "Failed to enable mod: " <> T.pack (show e)))

-- | Remove a mod by deleting its file.
removeMod :: FilePath -> IO (Either Text ())
removeMod path = do
  (removeFile path >> return (Right ()))
    `catch` (\(e :: SomeException) ->
      return (Left $ "Failed to remove mod: " <> T.pack (show e)))

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
              -- Map to human names, grouping unknowns as "Other"
              (known, unknownCount) = foldr (\(tid, cnt) (ks, uc) ->
                case resourceTypeName tid of
                  Just name -> ((name, cnt) : ks, uc)
                  Nothing   -> (ks, uc + cnt)) ([], 0 :: Int) grouped
              withOther = if unknownCount > 0
                then known ++ [("Other", unknownCount)]
                else known
              sorted    = List.sortBy (\a b' -> compare (snd b') (snd a)) withOther
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
-- Returns Nothing for unknown types (grouped as "Other" in display).
resourceTypeName :: Word32 -> Maybe Text
resourceTypeName tid = case tid of
  -- Images / Textures
  0x00B2D882 -> Just "DDS Image"
  0xB6C8B6A0 -> Just "DDS Image"
  0x3453CF95 -> Just "DXT5 Image"
  0xBA856C78 -> Just "DXT5 RLES Image"
  0x3C1AF1F2 -> Just "CAS Thumbnail"
  0x3C2A8647 -> Just "Object Thumbnail"
  0x5B282D45 -> Just "Thumbnail"
  0xCD9DE247 -> Just "Thumbnail PNG"
  0x0D338A3A -> Just "Lot Preview"
  0x2F7D0004 -> Just "Background Blend"
  -- CAS / Sim
  0x034AEECB -> Just "CAS Part"
  0x015A1849 -> Just "CAS Geometry"
  0x0354796A -> Just "Skin Tone"
  0x71BDB8A2 -> Just "Styled Look"
  0x025ED6F4 -> Just "Sim Outfit"
  0x0355E0A6 -> Just "Bone Delta"
  0xDB43E069 -> Just "Deformer Map"
  0x8EAF13DE -> Just "Skeleton/Rig"
  0x067CAA11 -> Just "Blend Geometry"
  -- Objects / Catalog
  0xC0DB5AE7 -> Just "Object Definition"
  0x319E4F1D -> Just "Object Catalog"
  0x01661233 -> Just "Object Model"
  0x01D10F34 -> Just "Model LODs"
  0x01D0E75D -> Just "Material Definition"
  0x02019972 -> Just "Material State"
  0x4F726BBE -> Just "Object Footprint"
  0xB734E44F -> Just "Footprint Resource"
  0xD3044521 -> Just "Object Slots"
  0xE231B3D8 -> Just "Object Modifiers"
  0xD382BF57 -> Just "Footprint Scene"
  0x03B4C61D -> Just "Light/Occluder"
  -- Build/Buy Catalog
  0xB4F762C9 -> Just "Floor Catalog"
  0xD5F0F921 -> Just "Wallpaper Catalog"
  0x91EDBD3E -> Just "Roof Style Catalog"
  0x9A20CD1C -> Just "Stairs Catalog"
  0x0418FE2A -> Just "Fence Catalog"
  0x1C1CF1F7 -> Just "Railing Catalog"
  0x1D6DF1CF -> Just "Column Catalog"
  0x2FAE983E -> Just "Foundation Catalog"
  0x3F0C529A -> Just "Spandrel Catalog"
  0x76BCF80C -> Just "Trim Catalog"
  0xA057811C -> Just "Frieze Catalog"
  0xEBCBB16C -> Just "Terrain Paint Catalog"
  0x84C23219 -> Just "Floor Trim Catalog"
  -- Tuning / Data
  0x545AC67A -> Just "SimData"
  0x62E94D38 -> Just "Combined Tuning"
  0xB61DE6B4 -> Just "Object Tuning"
  0x6017E896 -> Just "Buff Tuning"
  0xCB5FDDC7 -> Just "Trait Tuning"
  0xE882D22F -> Just "Interaction Tuning"
  0x0C772E27 -> Just "Action Tuning"
  0x220557DA -> Just "String Table"
  -- Animation / Audio
  0x6B20C4F3 -> Just "Animation Clip"
  0xBC4A5044 -> Just "Clip Header"
  0x02D5DF13 -> Just "Animation Sequence"
  0x033260E3 -> Just "Animation Trackmask"
  0x01EEF63A -> Just "Audio"
  0x01A527DB -> Just "Voice Audio"
  0xBDD82221 -> Just "Audio Event"
  0xFD04E3BE -> Just "Audio Stinger"
  0x1B25A024 -> Just "Sound Properties"
  -- UI / Misc
  0x62ECC59A -> Just "Scaleform GFX"
  0x25796DCA -> Just "OpenType Font"
  0x276CA4B9 -> Just "TrueType Font"
  0x0333406C -> Just "Font Config"
  0x376840D7 -> Just "Video"
  0xEA5118B0 -> Just "Effects"
  0x2A8A5E22 -> Just "Tray Item"
  -- World / Lot
  0x370EFD6E -> Just "Room Definition"
  0x3924DE26 -> Just "Blueprint"
  0x91568FD8 -> Just "Lot Object List"
  0xAC16FBEC -> Just "Region Map"
  0x9063660D -> Just "World Texture Map"
  0x3BD45407 -> Just "Household Info"
  _          -> Nothing

-- | Infer a mod category from the resource types present.
inferCategory :: [Word32] -> Text
inferCategory typeIds
  | 0x034AEECB `elem` typeIds                  = "CAS / Clothing & Hair"  -- CAS Part
  | 0x0354796A `elem` typeIds
    || 0xDB43E069 `elem` typeIds               = "CAS / Skin"             -- Skin Tone / Deformer Map
  | any (`elem` typeIds) buildBuyTypes         = "Build / Buy"
  | 0x6B20C4F3 `elem` typeIds
    || 0xBC4A5044 `elem` typeIds               = "Animation Mod"
  | any (`elem` typeIds) tuningTypes           = "Gameplay / Tuning"
  | 0xC0DB5AE7 `elem` typeIds                  = "Object Mod"
  | any (`elem` typeIds) worldTypes            = "World / Lot"
  | otherwise                                  = "Mod Package"
  where
    buildBuyTypes = [ 0xB4F762C9, 0xD5F0F921, 0x91EDBD3E, 0x9A20CD1C
                    , 0x0418FE2A, 0x1C1CF1F7, 0x1D6DF1CF, 0x2FAE983E
                    , 0x3F0C529A, 0x76BCF80C, 0x84C23219 ]
    tuningTypes   = [ 0x6017E896, 0xCB5FDDC7, 0xE882D22F, 0x0C772E27
                    , 0x62E94D38, 0xB61DE6B4 ]
    worldTypes    = [ 0x370EFD6E, 0x3924DE26, 0x91568FD8, 0xAC16FBEC
                    , 0x3BD45407 ]

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

-- ---------------------------------------------------------------------------
-- Mod Profiles
-- ---------------------------------------------------------------------------

-- | A named snapshot of which mods are enabled.
data ModProfile = ModProfile
  { profileName        :: Text
  , profileEnabledMods :: [Text]  -- filenames of mods that should be enabled
  } deriving (Show, Eq)

profilesFilePath :: IO FilePath
profilesFilePath = do
  configDir <- getXdgDirectory XdgConfig "sims4-mod-manager"
  return $ configDir </> "profiles.conf"

-- | Save profiles. Format: "[Name]\nfile1.package\nfile2.package\n\n"
saveProfiles :: FilePath -> [ModProfile] -> IO ()
saveProfiles path profs = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (concatMap fmtProf profs)
  where
    fmtProf p =
      "[" ++ T.unpack (profileName p) ++ "]\n"
      ++ unlines (map T.unpack (profileEnabledMods p))
      ++ "\n"

loadProfiles :: FilePath -> IO [ModProfile]
loadProfiles path = do
  exists <- doesFileExist path
  if not exists
    then return []
    else parseProfiles <$> readFile path

parseProfiles :: String -> [ModProfile]
parseProfiles = finish . foldl step (Nothing, []) . lines
  where
    step (Nothing, acc) l
      | isHdr l   = (Just (hdrName l, []), acc)
      | otherwise = (Nothing, acc)
    step (Just (n, ms), acc) l
      | isHdr l        = (Just (hdrName l, []), mkProf n ms : acc)
      | null (trimS l) = (Just (n, ms), acc)
      | otherwise      = (Just (n, T.pack (trimS l) : ms), acc)
    finish (Nothing, acc)      = reverse acc
    finish (Just (n, ms), acc) = reverse (mkProf n ms : acc)
    mkProf n ms  = ModProfile n (reverse ms)
    isHdr l      = case l of { '[':_ -> last l == ']'; _ -> False }
    hdrName l    = T.pack (drop 1 (init l))
    trimS        = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Build a profile from the current enabled/disabled state of all mods.
makeProfileFromCurrent :: Text -> [ModInfo] -> ModProfile
makeProfileFromCurrent name mods = ModProfile
  { profileName        = name
  , profileEnabledMods = [modName m | m <- mods, modEnabled m]
  }

-- | Enable/disable mods to match a saved profile.
applyProfile :: ModProfile -> [ModInfo] -> IO ()
applyProfile profile mods = mapM_ go mods
  where
    enabledSet = profileEnabledMods profile
    go m = do
      let path = T.unpack (modPath m)
      case (modName m `elem` enabledSet, modEnabled m) of
        (True,  False) -> enableMod  path >> return ()
        (False, True)  -> disableMod path >> return ()
        _              -> return ()

-- | Enable all currently disabled mods.
enableAllMods :: [ModInfo] -> IO ()
enableAllMods mods =
  mapM_ (enableMod . T.unpack . modPath) (filter (not . modEnabled) mods)

-- | Disable all currently enabled mods.
disableAllMods :: [ModInfo] -> IO ()
disableAllMods mods =
  mapM_ (disableMod . T.unpack . modPath) (filter modEnabled mods)

-- ---------------------------------------------------------------------------
-- Zip import
-- ---------------------------------------------------------------------------

-- | Extract all .package and .ts4script files from a zip into the mods folder.
-- Returns the imported ModInfo list, or an error message if nothing could be extracted.
importZip :: FilePath -> String -> IO (Either Text [ModInfo])
importZip zipPath customModPath = (do
  modsDir  <- getModsFolder customModPath
  bytes    <- BL.readFile zipPath
  let entries = filter (isModEntry . eRelativePath) (zEntries (toArchive bytes))
  if null entries
    then return (Left "No .package or .ts4script files found in zip.")
    else do
      results <- mapM (extractEntry modsDir) entries
      let successes = [m | Right m <- results]
          failures  = [e | Left  e <- results]
      if null successes
        then return (Left (T.intercalate "; " failures))
        else return (Right successes)
  ) `catch` (\(e :: SomeException) ->
      return (Left $ "Failed to read zip: " <> T.pack (show e)))
  where
    isModEntry path =
      let ext = takeExtension (takeFileName path)
      in ext == ".package" || ext == ".ts4script"

    extractEntry modsDir entry = (do
      let fname    = takeFileName (eRelativePath entry)
          destPath = modsDir </> fname
      BL.writeFile destPath (fromEntry entry)
      size    <- getFileSize destPath
      let ext = takeExtension fname
      pkgInfo <- if ext == ".package"
        then parsePackageFile destPath
        else return Nothing
      return $ Right ModInfo
        { modName        = T.pack fname
        , modPath        = T.pack destPath
        , modSize        = size
        , modType        = T.pack (drop 1 ext)
        , modPackageInfo = pkgInfo
        , modEnabled     = True
        }
      ) `catch` (\(e :: SomeException) ->
          return $ Left $ T.pack (takeFileName (eRelativePath entry))
                        <> ": " <> T.pack (show e))

-- | Import a single mod file or a zip archive, returning all imported mods.
importAny :: FilePath -> String -> IO (Either Text [ModInfo])
importAny path customModPath
  | takeExtension path == ".zip" = importZip path customModPath
  | otherwise = do
      result <- importMod path customModPath
      return $ case result of
        Left err -> Left err
        Right m  -> Right [m]

-- ---------------------------------------------------------------------------
-- Wine prefix / tool launcher
-- ---------------------------------------------------------------------------

-- | Common Sims 4 Wine prefix locations to check.
defaultWinePrefixPaths :: IO [FilePath]
defaultWinePrefixPaths = do
  home <- getHomeDirectory
  return
    [ home </> ".local" </> "share" </> "Steam"
              </> "steamapps" </> "compatdata" </> "1222670" </> "pfx"
    , home </> ".wine"
    ]

-- | Launch an executable under Wine.
-- @customWineExe@: path or name of the wine binary to use; empty = auto-detect.
-- On NixOS, wraps the call in @steam-run@ when available so that the Proton
-- wine binary can find its dynamically linked libraries.
-- Returns immediately (does not wait for the process to exit).
runInWinePrefix :: String -> FilePath -> FilePath -> IO (Either Text ())
runInWinePrefix customWineExe prefix exePath = (do
  mWine <- resolveWineExe customWineExe
  case mWine of
    Nothing -> return (Left
      "wine not found. Set a Wine executable path in Preferences\
      \ (e.g. the Proton wine64 inside your Steam installation).")
    Just wine -> do
      currentEnv <- getEnvironment
      let newEnv = ("WINEPREFIX", prefix)
                   : filter ((/= "WINEPREFIX") . fst) currentEnv
      mSteamRun <- findSteamRun
      let (cmd, args) = case mSteamRun of
            Just sr -> (sr, [wine, exePath])   -- NixOS: steam-run wine64 exe
            Nothing -> (wine, [exePath])        -- standard Linux: wine64 exe
      _ <- createProcess (proc cmd args) { env = Just newEnv }
      return (Right ())
  ) `catch` (\(e :: SomeException) ->
      return (Left $ "Failed to launch: " <> T.pack (show e)))

-- | Resolve which wine binary to use.
-- If @custom@ is non-empty, treat it as a path or executable name.
-- Otherwise search system PATH, common install dirs, then Proton.
resolveWineExe :: String -> IO (Maybe FilePath)
resolveWineExe custom
  | not (null custom) = do
      exists <- doesFileExist custom
      if exists
        then return (Just custom)
        else findExecutable custom   -- treat as a name in PATH
  | otherwise = findWineExecutable

-- | Search for steam-run (NixOS FHS wrapper for Steam binaries).
findSteamRun :: IO (Maybe FilePath)
findSteamRun = do
  viaPath <- findExecutable "steam-run"
  case viaPath of
    Just p  -> return (Just p)
    Nothing -> findFirstExisting
      [ "/run/current-system/sw/bin/steam-run"
      , "/nix/var/nix/profiles/default/bin/steam-run"
      ]
  where
    findFirstExisting []     = return Nothing
    findFirstExisting (p:ps) = doesFileExist p >>= \e ->
      if e then return (Just p) else findFirstExisting ps

-- | Search for a wine binary: PATH → common install dirs → Steam Proton.
findWineExecutable :: IO (Maybe FilePath)
findWineExecutable = do
  viaPath <- findExecutable "wine"
  case viaPath of
    Just p  -> return (Just p)
    Nothing -> do
      fromDirs <- findFirstExisting systemPaths
      case fromDirs of
        Just p  -> return (Just p)
        Nothing -> findProtonWine
  where
    systemPaths =
      [ "/usr/bin/wine64", "/usr/bin/wine"
      , "/usr/local/bin/wine64", "/usr/local/bin/wine"
      , "/opt/wine-stable/bin/wine64", "/opt/wine-stable/bin/wine"
      , "/opt/wine-devel/bin/wine64",  "/opt/wine-devel/bin/wine"
      , "/opt/wine-staging/bin/wine64","/opt/wine-staging/bin/wine"
      ]
    findFirstExisting []     = return Nothing
    findFirstExisting (p:ps) = doesFileExist p >>= \e ->
      if e then return (Just p) else findFirstExisting ps

-- | Search Steam's Proton installations for a wine64/wine binary.
-- Handles both the older @dist/bin/@ and newer @files/bin/@ layouts.
findProtonWine :: IO (Maybe FilePath)
findProtonWine = do
  home <- getHomeDirectory
  let steamCommon = home </> ".local" </> "share" </> "Steam"
                        </> "steamapps" </> "common"
  exists <- doesDirectoryExist steamCommon
  if not exists
    then return Nothing
    else do
      entries <- listDirectory steamCommon
                   `catch` (\(_ :: SomeException) -> return [])
      let protonDirs = filter ("Proton" `List.isPrefixOf`) entries
          candidates = concatMap (\d ->
            let base = steamCommon </> d
            in [ base </> "files" </> "bin" </> "wine64"
               , base </> "files" </> "bin" </> "wine"
               , base </> "dist"  </> "bin" </> "wine64"
               , base </> "dist"  </> "bin" </> "wine"
               ]) protonDirs
      findFirstExisting candidates
  where
    findFirstExisting []     = return Nothing
    findFirstExisting (p:ps) = doesFileExist p >>= \e ->
      if e then return (Just p) else findFirstExisting ps
