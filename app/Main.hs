{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Control.Monad (when)
import Control.Exception (SomeException, catch)

import Logic

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

  -- ── Settings popover ──────────────────────────────────────────────
  settingsBtn <- new Gtk.MenuButton [#iconName := "preferences-system-symbolic"]
  Gtk.widgetSetTooltipText settingsBtn (Just "Preferences")

  prefsBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 8
    , #marginTop := 8
    , #marginBottom := 8
    , #marginStart := 8
    , #marginEnd := 8
    ]
  Gtk.widgetSetSizeRequest prefsBox 400 (-1)

  prefsLabel <- new Gtk.Label
    [ #label := "Mods Folder (leave empty for default):"
    , #halign := Gtk.AlignStart
    ]

  prefsEntryBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing := 4
    ]

  prefsPathEntry <- new Gtk.Entry
    [ #placeholderText := "/path/to/Mods"
    , #text := T.pack (wsModPath state)
    , #hexpand := True
    ]
  Gtk.widgetAddCssClass prefsPathEntry "monospace"

  prefsBrowseBtn <- new Gtk.Button [#iconName := "folder-open-symbolic"]
  Gtk.widgetSetTooltipText prefsBrowseBtn (Just "Browse...")
  Gtk.widgetSetValign prefsBrowseBtn Gtk.AlignCenter

  Gtk.boxAppend prefsEntryBox prefsPathEntry
  Gtk.boxAppend prefsEntryBox prefsBrowseBtn

  prefsStatusLabel <- new Gtk.Label
    [ #label := ""
    , #halign := Gtk.AlignStart
    , #wrap := True
    ]

  prefsBtnBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing := 8
    , #halign := Gtk.AlignEnd
    ]

  resetBtn <- new Gtk.Button [#label := "Reset"]
  savePrefsBtn <- new Gtk.Button [#label := "Save"]
  Gtk.widgetAddCssClass savePrefsBtn "suggested-action"

  Gtk.boxAppend prefsBtnBox resetBtn
  Gtk.boxAppend prefsBtnBox savePrefsBtn

  Gtk.boxAppend prefsBox prefsLabel
  Gtk.boxAppend prefsBox prefsEntryBox
  Gtk.boxAppend prefsBox prefsStatusLabel
  Gtk.boxAppend prefsBox prefsBtnBox

  prefsPopover <- new Gtk.Popover [#child := prefsBox]
  Gtk.menuButtonSetPopover settingsBtn (Just prefsPopover)

  Adw.headerBarPackEnd headerBar settingsBtn

  -- ── Add Mod popover ───────────────────────────────────────────────
  addModBtn <- new Gtk.MenuButton [#iconName := "list-add-symbolic"]
  Gtk.widgetSetTooltipText addModBtn (Just "Add Mod")

  popoverBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 8
    , #marginTop := 8
    , #marginBottom := 8
    , #marginStart := 8
    , #marginEnd := 8
    ]
  Gtk.widgetSetSizeRequest popoverBox 400 (-1)

  pathLabel <- new Gtk.Label
    [ #label := "Path to .package or .ts4script file:"
    , #halign := Gtk.AlignStart
    ]

  addEntryBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing := 4
    ]

  pathEntry <- new Gtk.Entry
    [ #placeholderText := "/path/to/mod.package"
    , #hexpand := True
    ]
  Gtk.widgetAddCssClass pathEntry "monospace"

  addBrowseBtn <- new Gtk.Button [#iconName := "folder-open-symbolic"]
  Gtk.widgetSetTooltipText addBrowseBtn (Just "Browse...")
  Gtk.widgetSetValign addBrowseBtn Gtk.AlignCenter

  Gtk.boxAppend addEntryBox pathEntry
  Gtk.boxAppend addEntryBox addBrowseBtn

  statusLabel <- new Gtk.Label
    [ #label := ""
    , #halign := Gtk.AlignStart
    , #wrap := True
    ]

  confirmBtn <- new Gtk.Button
    [ #label := "Add Mod"
    , #halign := Gtk.AlignEnd
    ]
  Gtk.widgetAddCssClass confirmBtn "suggested-action"

  Gtk.boxAppend popoverBox pathLabel
  Gtk.boxAppend popoverBox addEntryBox
  Gtk.boxAppend popoverBox statusLabel
  Gtk.boxAppend popoverBox confirmBtn

  addPopover <- new Gtk.Popover [#child := popoverBox]
  Gtk.menuButtonSetPopover addModBtn (Just addPopover)

  Adw.headerBarPackEnd headerBar addModBtn

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
        let clearList = do
              child <- Gtk.widgetGetFirstChild listBox
              case child of
                Nothing -> return ()
                Just c  -> Gtk.listBoxRemove listBox c >> clearList
        clearList
        mapM_ (addModRow listBox) newMods
        Adw.windowTitleSetSubtitle winTitle
          (T.pack (show (length newMods)) <> " mods found")

  -- Helper: import a mod file via Logic, update UI
  let doImportMod filePath = do
        curState <- readIORef stateRef
        result <- importMod filePath (wsModPath curState)
        case result of
          Left err -> set statusLabel [#label := err]
          Right _ -> do
            rescanMods
            Gtk.editableSetText pathEntry ("" :: Text)
            set statusLabel [#label := "Added!"]
            Gtk.popoverPopdown addPopover

  -- ── Add Mod: browse button opens native file picker and imports ───
  _ <- on addBrowseBtn #clicked $ do
    fileDialog <- new Gtk.FileDialog [#title := "Select Mod File"]

    modFilter <- new Gtk.FileFilter [#name := "Sims 4 Mods (*.package, *.ts4script)"]
    Gtk.fileFilterAddPattern modFilter "*.package"
    Gtk.fileFilterAddPattern modFilter "*.ts4script"
    fileFilterType <- glibType @Gtk.FileFilter
    filters <- Gio.listStoreNew fileFilterType
    Gio.listStoreAppend filters modFilter
    Gtk.fileDialogSetFilters fileDialog (Just filters)
    Gtk.fileDialogSetDefaultFilter fileDialog (Just modFilter)

    Gtk.fileDialogOpen fileDialog (Just win) (Nothing @Gio.Cancellable) (Just $ \_obj res -> do
      result <- (do
        file <- Gtk.fileDialogOpenFinish fileDialog res
        Gio.fileGetPath file
        ) `catch` (\(_ :: SomeException) -> return Nothing)
      case result of
        Nothing -> return ()
        Just path -> doImportMod path
      )

  -- Add Mod: confirm button (for manual path entry)
  _ <- on confirmBtn #clicked $ do
    filePath <- T.unpack . T.strip <$> Gtk.editableGetText pathEntry
    if null filePath
      then set statusLabel [#label := "Please enter a file path."]
      else doImportMod filePath

  -- ── Preferences: browse button opens native folder picker ─────────
  _ <- on prefsBrowseBtn #clicked $ do
    folderDialog <- new Gtk.FileDialog [#title := "Select Mods Folder"]

    Gtk.fileDialogSelectFolder folderDialog (Just win) (Nothing @Gio.Cancellable) (Just $ \_obj res -> do
      result <- (do
        file <- Gtk.fileDialogSelectFolderFinish folderDialog res
        Gio.fileGetPath file
        ) `catch` (\(_ :: SomeException) -> return Nothing)
      case result of
        Nothing -> return ()
        Just path -> Gtk.editableSetText prefsPathEntry (T.pack path)
      )

  -- Preferences: Reset button
  _ <- on resetBtn #clicked $ do
    Gtk.editableSetText prefsPathEntry ("" :: Text)
    set prefsStatusLabel [#label := ""]

  -- Preferences: Save button
  _ <- on savePrefsBtn #clicked $ do
    newPath <- T.unpack . T.strip <$> Gtk.editableGetText prefsPathEntry
    result <- validateModsFolder newPath
    case result of
      Left err -> set prefsStatusLabel [#label := err]
      Right validPath -> do
        modifyIORef stateRef (\s -> s { wsModPath = validPath })
        st <- readIORef stateRef
        saveWindowState configPath st
        rescanMods
        set prefsStatusLabel [#label := "Saved."]
        Gtk.popoverPopdown prefsPopover

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
