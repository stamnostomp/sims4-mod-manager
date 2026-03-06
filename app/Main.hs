{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, getHomeDirectory, getXdgDirectory, listDirectory, XdgDirectory(..))
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)
import Data.IORef
import Control.Monad (when)

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
  , wsPanedPosition = 720  -- 80% of 900 (details pane gets 20%)
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

-- | Recursively find all .package and .ts4script files under a directory.
findMods :: FilePath -> IO [ModInfo]
findMods root = do
  exists <- doesDirectoryExist root
  if not exists
    then return []
    else go root
  where
    go dir = do
      entries <- listDirectory dir
      fmap concat $ mapM (processEntry dir) entries

    processEntry dir entry = do
      let full = dir </> entry
      isDir <- doesDirectoryExist full
      if isDir
        then go full
        else case takeExtension entry of
          ext | ext == ".package" || ext == ".ts4script" -> do
            size <- getFileSize full
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

-- | Scan both common Sims 4 Mods directories and return all mods found.
scanMods :: IO [ModInfo]
scanMods = do
  home <- System.Directory.getHomeDirectory
  let paths =
        [ home </> "Documents" </> "Electronic Arts" </> "The Sims 4" </> "Mods"
        , home </> ".local" </> "share" </> "The Sims 4" </> "Mods"
        ]
  fmap concat $ mapM findMods paths

-- | Scan mods from a custom path, or default locations if empty.
scanModsFrom :: String -> IO [ModInfo]
scanModsFrom "" = scanMods
scanModsFrom path = findMods path

main :: IO ()
main = do
  app <- new Adw.Application [#applicationId := "com.stamno.sims4modmanager"]
  _ <- on app #activate (onActivate app)
  _ <- Gio.applicationRun app Nothing
  return ()

onActivate :: Adw.Application -> IO ()
onActivate app = do
  sm <- Adw.styleManagerGetDefault
  Adw.styleManagerSetColorScheme sm Adw.ColorSchemeForceDark

  -- Custom CSS
  cssProvider <- new Gtk.CssProvider []
  Gtk.cssProviderLoadFromString cssProvider
    ".prefs-bordered { border: 1px solid rgba(255,255,255,0.15); border-radius: 12px; }"
  display <- Gdk.displayGetDefault
  case display of
    Nothing -> return ()
    Just d  -> Gtk.styleContextAddProviderForDisplay d cssProvider 600

  -- Load persisted window state
  state <- loadWindowState
  configPath <- stateFilePath

  -- Mutable app state
  stateRef <- newIORef state

  -- Scan for mods
  mods <- scanModsFrom (wsModPath state)
  modsRef <- newIORef mods

  -- Window
  win <- new Adw.ApplicationWindow
    [ #application := app
    , #defaultWidth := fromIntegral (wsWidth state)
    , #defaultHeight := fromIntegral (wsHeight state)
    , #title := "Sims 4 Mod Manager"
    ]
  Gtk.widgetSetSizeRequest win 500 300

  -- Main vertical box: header bar + content
  outerBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  -- Header bar
  headerBar <- new Adw.HeaderBar []
  winTitle <- new Adw.WindowTitle
    [ #title := "Sims 4 Mod Manager"
    , #subtitle := T.pack (show (length mods)) <> " mods found"
    ]
  Adw.headerBarSetTitleWidget headerBar (Just winTitle)

  -- Settings button
  settingsBtn <- new Gtk.Button [#iconName := "preferences-system-symbolic"]
  Gtk.widgetSetTooltipText settingsBtn (Just "Preferences")
  Adw.headerBarPackEnd headerBar settingsBtn

  Gtk.boxAppend outerBox headerBar

  -- Paned: left = mod list, right = details
  paned <- new Gtk.Paned
    [ #orientation := Gtk.OrientationHorizontal
    , #position := fromIntegral (wsPanedPosition state)
    , #vexpand := True
    ]

  -- Left: scrollable mod list
  listBox <- new Gtk.ListBox
    [ #selectionMode := Gtk.SelectionModeSingle
    ]
  Gtk.widgetAddCssClass listBox "boxed-list"

  scrollWin <- new Gtk.ScrolledWindow
    [ #hscrollbarPolicy := Gtk.PolicyTypeNever
    , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
    , #child := listBox
    ]

  -- Wrap the list in a clamp for nice Adwaita styling
  leftBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #marginTop := 6
    , #marginBottom := 6
    , #marginStart := 6
    , #marginEnd := 6
    ]
  Gtk.widgetSetSizeRequest leftBox 200 (-1)
  Gtk.boxAppend leftBox scrollWin
  Gtk.widgetSetVexpand scrollWin True

  Gtk.panedSetStartChild paned (Just leftBox)

  -- Right: details panel
  detailsBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 12
    , #marginTop := 24
    , #marginBottom := 24
    , #marginStart := 18
    , #marginEnd := 18
    , #valign := Gtk.AlignStart
    ]
  Gtk.widgetSetSizeRequest detailsBox 150 (-1)

  detailTitle <- new Gtk.Label
    [ #label := "Select a mod"
    , #halign := Gtk.AlignStart
    ]
  Gtk.widgetAddCssClass detailTitle "title-2"

  detailPath <- new Gtk.Label
    [ #label := ""
    , #halign := Gtk.AlignStart
    , #wrap := True
    , #selectable := True
    ]

  detailSize <- new Gtk.Label
    [ #label := ""
    , #halign := Gtk.AlignStart
    ]

  detailType <- new Gtk.Label
    [ #label := ""
    , #halign := Gtk.AlignStart
    ]

  Gtk.boxAppend detailsBox detailTitle
  Gtk.boxAppend detailsBox detailPath
  Gtk.boxAppend detailsBox detailSize
  Gtk.boxAppend detailsBox detailType

  Gtk.panedSetEndChild paned (Just detailsBox)

  Gtk.boxAppend outerBox paned

  -- Populate list
  mapM_ (addModRow listBox) mods

  -- Selection handler
  _ <- on listBox #rowSelected $ \mRow -> do
    curMods <- readIORef modsRef
    case mRow of
      Nothing -> return ()
      Just row -> do
        idx <- Gtk.listBoxRowGetIndex row
        let i = fromIntegral idx :: Int
        if i >= 0 && i < length curMods
          then do
            let m = curMods !! i
            set detailTitle [#label := modName m]
            set detailPath  [#label := modPath m]
            set detailSize  [#label := "Size: " <> formatSize (modSize m)]
            set detailType  [#label := "Type: " <> modType m]
          else return ()

  -- Helper: clear and rescan mod list
  let rescanMods = do
        curState <- readIORef stateRef
        newMods <- scanModsFrom (wsModPath curState)
        writeIORef modsRef newMods
        -- Clear listbox
        let clearList = do
              child <- Gtk.widgetGetFirstChild listBox
              case child of
                Nothing -> return ()
                Just c  -> Gtk.listBoxRemove listBox c >> clearList
        clearList
        -- Re-populate
        mapM_ (addModRow listBox) newMods
        Adw.windowTitleSetSubtitle winTitle
          (T.pack (show (length newMods)) <> " mods found")

  -- Settings button opens preferences
  _ <- on settingsBtn #clicked $ do
    curState <- readIORef stateRef
    prefsDlg <- new Adw.PreferencesDialog
      [ #title := "Preferences"
      ]

    page <- new Adw.PreferencesPage []
    Gtk.widgetAddCssClass prefsDlg "prefs-bordered"

    group <- new Adw.PreferencesGroup
      [ #title := "Mod Location"
      , #description := "Set a custom folder or leave empty for default Sims 4 directories"
      ]

    let curPath = wsModPath curState

    pathEntry <- new Adw.EntryRow
      [ #title := "Mods Folder"
      , #text := T.pack curPath
      ]
    Gtk.widgetAddCssClass pathEntry "monospace"

    -- Open current mods folder in file manager
    openBtn <- new Gtk.Button [#iconName := "folder-open-symbolic"]
    Gtk.widgetSetValign openBtn Gtk.AlignCenter
    Gtk.widgetSetTooltipText openBtn (Just "Open in file manager")
    Adw.entryRowAddSuffix pathEntry openBtn

    -- Reset to default
    resetBtn <- new Gtk.Button [#iconName := "edit-clear-symbolic"]
    Gtk.widgetSetValign resetBtn Gtk.AlignCenter
    Gtk.widgetSetTooltipText resetBtn (Just "Reset to default")
    Adw.entryRowAddSuffix pathEntry resetBtn

    _ <- on openBtn #clicked $ do
      entryText <- T.unpack <$> get pathEntry #text
      targetPath <- if null entryText
        then do
          home <- getHomeDirectory
          let p1 = home </> "Documents" </> "Electronic Arts" </> "The Sims 4" </> "Mods"
          exists <- doesDirectoryExist p1
          if exists then return p1
            else return (home </> ".local" </> "share" </> "The Sims 4" </> "Mods")
        else return entryText
      file <- Gio.fileNewForPath targetPath
      uri <- Gio.fileGetUri file
      launcher <- new Gtk.UriLauncher [#uri := uri]
      Gtk.uriLauncherLaunch launcher (Just win) (Nothing @Gio.Cancellable) Nothing

    _ <- on resetBtn #clicked $
      set pathEntry [#text := ("" :: Text)]

    Adw.preferencesGroupAdd group pathEntry
    Adw.preferencesPageAdd page group
    Adw.preferencesDialogAdd prefsDlg page

    _ <- on prefsDlg #closed $ do
      newPath <- T.unpack <$> get pathEntry #text
      modifyIORef stateRef (\s -> s { wsModPath = newPath })
      st <- readIORef stateRef
      saveWindowState configPath st
      rescanMods

    Adw.dialogPresent prefsDlg (Just win)

  -- Save window state on close
  _ <- on win #closeRequest $ do
    maximized <- Gtk.windowIsMaximized win
    pos <- fromIntegral <$> Gtk.panedGetPosition paned
    curState <- readIORef stateRef
    let st = curState { wsPanedPosition = pos, wsMaximized = maximized }
    st' <- if maximized
      then return st
      else do
        w <- fromIntegral <$> Gtk.widgetGetWidth win
        h <- fromIntegral <$> Gtk.widgetGetHeight win
        return st { wsWidth = w, wsHeight = h }
    saveWindowState configPath st'
    return False

  Adw.applicationWindowSetContent win (Just outerBox)
  #present win
  when (wsMaximized state) $ Gtk.windowMaximize win

-- | Add a row to the list box for one mod.
addModRow :: Gtk.ListBox -> ModInfo -> IO ()
addModRow listBox m = do
  row <- new Adw.ActionRow
    [ #title := modName m
    , #subtitle := formatSize (modSize m) <> "  ·  " <> modType m
    ]
  Gtk.listBoxAppend listBox row
