{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Control.Monad (when, join, forM_)
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
    ".prefs-bordered { border: 1px solid rgba(255,255,255,0.15); border-radius: 12px; } \
    \.mod-disabled { color: alpha(currentColor, 0.55); }"
  display <- Gdk.displayGetDefault
  case display of
    Nothing -> return ()
    Just d  -> Gtk.styleContextAddProviderForDisplay d cssProvider 600

  -- Load persisted state and profiles
  state      <- loadWindowState
  configPath <- stateFilePath
  profPath   <- profilesFilePath
  profiles   <- loadProfiles profPath

  -- Mutable refs
  stateRef    <- newIORef state
  modsRef     <- newIORef ([] :: [ModInfo])
  visibleRef  <- newIORef ([] :: [ModInfo])
  filterRef   <- newIORef ("" :: Text)
  profilesRef <- newIORef profiles

  -- Initial scan
  initialMods <- scanModsFrom (wsModPath state)
  writeIORef modsRef initialMods
  writeIORef visibleRef initialMods

  -- Window
  win <- new Adw.ApplicationWindow
    [ #application   := app
    , #defaultWidth  := fromIntegral (wsWidth state)
    , #defaultHeight := fromIntegral (wsHeight state)
    , #title         := "Sims 4 Mod Manager"
    ]
  Gtk.widgetSetSizeRequest win 500 300

  outerBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  -- Header bar
  headerBar <- new Adw.HeaderBar []
  winTitle  <- new Adw.WindowTitle
    [ #title    := "Sims 4 Mod Manager"
    , #subtitle := T.pack (show (length initialMods)) <> " mods found"
    ]
  Adw.headerBarSetTitleWidget headerBar (Just winTitle)

  -- ── Settings popover ──────────────────────────────────────────────
  settingsBtn <- new Gtk.MenuButton [#iconName := "preferences-system-symbolic"]
  Gtk.widgetSetTooltipText settingsBtn (Just "Preferences")

  prefsBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical, #spacing := 8
    , #marginTop := 8, #marginBottom := 8, #marginStart := 8, #marginEnd := 8
    ]
  Gtk.widgetSetSizeRequest prefsBox 400 (-1)

  prefsLabel <- new Gtk.Label
    [ #label := "Mods Folder (leave empty for default):", #halign := Gtk.AlignStart ]

  prefsEntryBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal, #spacing := 4 ]

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
    [ #label := "", #halign := Gtk.AlignStart, #wrap := True ]

  prefsBtnBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignEnd ]

  resetBtn     <- new Gtk.Button [#label := "Reset"]
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
    [ #orientation := Gtk.OrientationVertical, #spacing := 8
    , #marginTop := 8, #marginBottom := 8, #marginStart := 8, #marginEnd := 8
    ]
  Gtk.widgetSetSizeRequest popoverBox 400 (-1)

  pathLabel <- new Gtk.Label
    [ #label := "Path to .package or .ts4script file:", #halign := Gtk.AlignStart ]

  addEntryBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal, #spacing := 4 ]

  pathEntry <- new Gtk.Entry
    [ #placeholderText := "/path/to/mod.package", #hexpand := True ]
  Gtk.widgetAddCssClass pathEntry "monospace"

  addBrowseBtn <- new Gtk.Button [#iconName := "folder-open-symbolic"]
  Gtk.widgetSetTooltipText addBrowseBtn (Just "Browse...")
  Gtk.widgetSetValign addBrowseBtn Gtk.AlignCenter

  Gtk.boxAppend addEntryBox pathEntry
  Gtk.boxAppend addEntryBox addBrowseBtn

  statusLabel <- new Gtk.Label
    [ #label := "", #halign := Gtk.AlignStart, #wrap := True ]

  confirmBtn <- new Gtk.Button [#label := "Add Mod", #halign := Gtk.AlignEnd]
  Gtk.widgetAddCssClass confirmBtn "suggested-action"

  Gtk.boxAppend popoverBox pathLabel
  Gtk.boxAppend popoverBox addEntryBox
  Gtk.boxAppend popoverBox statusLabel
  Gtk.boxAppend popoverBox confirmBtn

  addPopover <- new Gtk.Popover [#child := popoverBox]
  Gtk.menuButtonSetPopover addModBtn (Just addPopover)
  Adw.headerBarPackEnd headerBar addModBtn

  -- ── Profiles popover ──────────────────────────────────────────────
  profilesBtn <- new Gtk.MenuButton [#iconName := "view-list-symbolic"]
  Gtk.widgetSetTooltipText profilesBtn (Just "Mod Profiles")

  profilesBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical, #spacing := 8
    , #marginTop := 8, #marginBottom := 8, #marginStart := 8, #marginEnd := 8
    ]
  Gtk.widgetSetSizeRequest profilesBox 320 (-1)

  profilesTitle <- new Gtk.Label
    [ #label := "Mod Profiles", #halign := Gtk.AlignStart ]
  Gtk.widgetAddCssClass profilesTitle "title-4"

  saveProfileRow <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal, #spacing := 4 ]

  profileNameEntry <- new Gtk.Entry
    [ #placeholderText := "Profile name", #hexpand := True ]

  saveProfileBtn <- new Gtk.Button [#label := "Save Current"]
  Gtk.widgetAddCssClass saveProfileBtn "suggested-action"
  Gtk.widgetSetValign saveProfileBtn Gtk.AlignCenter

  Gtk.boxAppend saveProfileRow profileNameEntry
  Gtk.boxAppend saveProfileRow saveProfileBtn

  profilesStatusLabel <- new Gtk.Label
    [ #label := "", #halign := Gtk.AlignStart, #wrap := True ]

  profilesSep <- new Gtk.Separator [#orientation := Gtk.OrientationHorizontal]

  profilesListBox <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone]
  Gtk.widgetAddCssClass profilesListBox "boxed-list"

  profilesScrollWin <- new Gtk.ScrolledWindow
    [ #hscrollbarPolicy := Gtk.PolicyTypeNever
    , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
    , #child := profilesListBox
    ]
  Gtk.widgetSetSizeRequest profilesScrollWin (-1) 200

  Gtk.boxAppend profilesBox profilesTitle
  Gtk.boxAppend profilesBox saveProfileRow
  Gtk.boxAppend profilesBox profilesStatusLabel
  Gtk.boxAppend profilesBox profilesSep
  Gtk.boxAppend profilesBox profilesScrollWin

  profilesPopover <- new Gtk.Popover [#child := profilesBox]
  Gtk.menuButtonSetPopover profilesBtn (Just profilesPopover)
  Adw.headerBarPackEnd headerBar profilesBtn

  Gtk.boxAppend outerBox headerBar

  -- ── Paned layout ──────────────────────────────────────────────────
  paned <- new Gtk.Paned
    [ #orientation := Gtk.OrientationHorizontal
    , #position    := fromIntegral (wsPanedPosition state)
    , #vexpand     := True
    ]

  -- Left: search + batch toolbar + mod list
  leftBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #marginTop   := 6, #marginBottom := 6
    , #marginStart := 6, #marginEnd   := 6
    , #spacing     := 4
    ]
  Gtk.widgetSetSizeRequest leftBox 200 (-1)

  searchEntry <- new Gtk.SearchEntry
    [ #placeholderText := "Search mods...", #hexpand := True ]

  batchBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal, #spacing := 4, #homogeneous := True ]

  enableAllBtn  <- new Gtk.Button [#label := "Enable All"]
  disableAllBtn <- new Gtk.Button [#label := "Disable All"]
  Gtk.widgetAddCssClass disableAllBtn "destructive-action"
  Gtk.boxAppend batchBox enableAllBtn
  Gtk.boxAppend batchBox disableAllBtn

  listBox <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle]
  Gtk.widgetAddCssClass listBox "boxed-list"

  scrollWin <- new Gtk.ScrolledWindow
    [ #hscrollbarPolicy := Gtk.PolicyTypeNever
    , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
    , #child   := listBox
    , #vexpand := True
    ]

  Gtk.boxAppend leftBox searchEntry
  Gtk.boxAppend leftBox batchBox
  Gtk.boxAppend leftBox scrollWin
  Gtk.panedSetStartChild paned (Just leftBox)

  -- Right: details panel
  detailsBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing     := 12
    , #marginTop   := 24, #marginBottom := 24
    , #marginStart := 18, #marginEnd   := 18
    , #valign      := Gtk.AlignStart
    ]
  Gtk.widgetSetSizeRequest detailsBox 150 (-1)

  detailTitle     <- new Gtk.Label [#label := "Select a mod", #halign := Gtk.AlignStart]
  Gtk.widgetAddCssClass detailTitle "title-2"
  detailPath      <- new Gtk.Label [#label := "", #halign := Gtk.AlignStart, #wrap := True, #selectable := True]
  detailSize      <- new Gtk.Label [#label := "", #halign := Gtk.AlignStart]
  detailType      <- new Gtk.Label [#label := "", #halign := Gtk.AlignStart]
  detailCategory  <- new Gtk.Label [#label := "", #halign := Gtk.AlignStart]
  detailResources <- new Gtk.Label [#label := "", #halign := Gtk.AlignStart]
  detailBreakdown <- new Gtk.Label [#label := "", #halign := Gtk.AlignStart, #wrap := True]

  Gtk.boxAppend detailsBox detailTitle
  Gtk.boxAppend detailsBox detailPath
  Gtk.boxAppend detailsBox detailSize
  Gtk.boxAppend detailsBox detailType
  Gtk.boxAppend detailsBox detailCategory
  Gtk.boxAppend detailsBox detailResources
  Gtk.boxAppend detailsBox detailBreakdown

  Gtk.panedSetEndChild paned (Just detailsBox)
  Gtk.boxAppend outerBox paned

  -- Knot-tying refs for self-referential callbacks
  rescanRef          <- newIORef (return () :: IO ())
  rebuildProfilesRef <- newIORef (return () :: IO ())

  -- ── repopulateList: apply filter, rebuild listbox ─────────────────
  let repopulateList = do
        allMods <- readIORef modsRef
        query   <- readIORef filterRef
        let visible = if T.null query
              then allMods
              else filter (\m -> T.toLower query `T.isInfixOf` T.toLower (modName m)) allMods
        writeIORef visibleRef visible
        let clearList = do
              child <- Gtk.widgetGetFirstChild listBox
              case child of
                Nothing -> return ()
                Just c  -> Gtk.listBoxRemove listBox c >> clearList
        clearList
        mapM_ (addModRow listBox win (join $ readIORef rescanRef)) visible

  -- ── rescanMods: reload from disk, then repopulate ─────────────────
  let rescanMods = do
        curState <- readIORef stateRef
        newMods  <- scanModsFrom (wsModPath curState)
        writeIORef modsRef newMods
        repopulateList
        visible <- readIORef visibleRef
        Adw.windowTitleSetSubtitle winTitle
          (T.pack (show (length visible)) <> " mods found")

  writeIORef rescanRef rescanMods

  -- ── doImportMod ───────────────────────────────────────────────────
  let doImportMod filePath = do
        curState <- readIORef stateRef
        result   <- importMod filePath (wsModPath curState)
        case result of
          Left err -> set statusLabel [#label := err]
          Right _  -> do
            rescanMods
            Gtk.editableSetText pathEntry ("" :: Text)
            set statusLabel [#label := "Added!"]
            Gtk.popoverPopdown addPopover

  -- ── rebuildProfilesList ───────────────────────────────────────────
  let rebuildProfilesList = do
        profs <- readIORef profilesRef
        let clearList = do
              child <- Gtk.widgetGetFirstChild profilesListBox
              case child of
                Nothing -> return ()
                Just c  -> Gtk.listBoxRemove profilesListBox c >> clearList
        clearList
        forM_ profs $ \prof -> do
          let enabledCount = T.pack (show (length (profileEnabledMods prof)))
          row <- new Adw.ActionRow
            [ #title    := profileName prof
            , #subtitle := enabledCount <> " mods enabled"
            ]

          applyBtn <- new Gtk.Button [#label := "Apply"]
          Gtk.widgetAddCssClass applyBtn "suggested-action"
          Gtk.widgetSetValign applyBtn Gtk.AlignCenter
          _ <- on applyBtn #clicked $ do
            curMods <- readIORef modsRef
            applyProfile prof curMods
            rescanMods
            set profilesStatusLabel [#label := "Applied: " <> profileName prof]

          let profName = profileName prof
          deleteBtn <- new Gtk.Button [#iconName := "user-trash-symbolic"]
          Gtk.widgetAddCssClass deleteBtn "destructive-action"
          Gtk.widgetSetValign deleteBtn Gtk.AlignCenter
          _ <- on deleteBtn #clicked $ do
            curProfs <- readIORef profilesRef
            let newProfs = filter (\p -> profileName p /= profName) curProfs
            writeIORef profilesRef newProfs
            saveProfiles profPath newProfs
            join $ readIORef rebuildProfilesRef
            set profilesStatusLabel [#label := "Deleted: " <> profName]

          Adw.actionRowAddSuffix row applyBtn
          Adw.actionRowAddSuffix row deleteBtn
          Gtk.listBoxAppend profilesListBox row

  writeIORef rebuildProfilesRef rebuildProfilesList

  -- Initial population
  rebuildProfilesList
  mapM_ (addModRow listBox win (join $ readIORef rescanRef)) initialMods

  -- ── Selection handler ─────────────────────────────────────────────
  _ <- on listBox #rowSelected $ \mRow ->
    case mRow of
      Nothing  -> return ()
      Just row -> do
        visible <- readIORef visibleRef
        idx     <- Gtk.listBoxRowGetIndex row
        let i = fromIntegral idx :: Int
        when (i >= 0 && i < length visible) $ do
          let m = visible !! i
          set detailTitle [#label := modName m]
          set detailPath  [#label := modPath m]
          set detailSize  [#label := "Size: " <> formatSize (modSize m)]
          set detailType  [#label := "Type: " <> modType m]
          case modPackageInfo m of
            Nothing   -> do
              set detailCategory  [#label := if modType m == "ts4script"
                                               then "Category: Script Mod"
                                               else ""]
              set detailResources [#label := ""]
              set detailBreakdown [#label := ""]
            Just info -> do
              set detailCategory  [#label := "Category: " <> pkgCategory info]
              set detailResources [#label := "Resources: " <> T.pack (show (pkgResourceCount info))]
              let breakdown = T.intercalate ", "
                    [ T.pack (show cnt) <> " " <> name
                    | (name, cnt) <- pkgResourceTypes info
                    ]
              set detailBreakdown [#label := breakdown]

  -- ── Search: filter by name ────────────────────────────────────────
  _ <- on searchEntry #searchChanged $ do
    query <- Gtk.editableGetText searchEntry
    writeIORef filterRef query
    repopulateList
    visible <- readIORef visibleRef
    Adw.windowTitleSetSubtitle winTitle
      (T.pack (show (length visible)) <> " mods found")

  -- ── Batch enable / disable ────────────────────────────────────────
  _ <- on enableAllBtn #clicked $ do
    allMods <- readIORef modsRef
    enableAllMods allMods
    rescanMods

  _ <- on disableAllBtn #clicked $ do
    allMods <- readIORef modsRef
    disableAllMods allMods
    rescanMods

  -- ── Save current mod state as a profile ───────────────────────────
  _ <- on saveProfileBtn #clicked $ do
    name <- T.strip <$> Gtk.editableGetText profileNameEntry
    if T.null name
      then set profilesStatusLabel [#label := "Please enter a profile name."]
      else do
        curMods  <- readIORef modsRef
        curProfs <- readIORef profilesRef
        let prof     = makeProfileFromCurrent name curMods
            newProfs = prof : filter (\p -> profileName p /= name) curProfs
        writeIORef profilesRef newProfs
        saveProfiles profPath newProfs
        rebuildProfilesList
        Gtk.editableSetText profileNameEntry ("" :: Text)
        set profilesStatusLabel [#label := "Saved: " <> name]

  -- ── Add Mod: browse ───────────────────────────────────────────────
  _ <- on addBrowseBtn #clicked $ do
    fileDialog <- new Gtk.FileDialog [#title := "Select Mod File"]
    modFilter  <- new Gtk.FileFilter [#name := "Sims 4 Mods (*.package, *.ts4script)"]
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
        Nothing   -> return ()
        Just path -> doImportMod path
      )

  -- ── Add Mod: confirm (manual path entry) ──────────────────────────
  _ <- on confirmBtn #clicked $ do
    filePath <- T.unpack . T.strip <$> Gtk.editableGetText pathEntry
    if null filePath
      then set statusLabel [#label := "Please enter a file path."]
      else doImportMod filePath

  -- ── Preferences: browse ───────────────────────────────────────────
  _ <- on prefsBrowseBtn #clicked $ do
    folderDialog <- new Gtk.FileDialog [#title := "Select Mods Folder"]
    Gtk.fileDialogSelectFolder folderDialog (Just win) (Nothing @Gio.Cancellable) (Just $ \_obj res -> do
      result <- (do
        file <- Gtk.fileDialogSelectFolderFinish folderDialog res
        Gio.fileGetPath file
        ) `catch` (\(_ :: SomeException) -> return Nothing)
      case result of
        Nothing   -> return ()
        Just path -> Gtk.editableSetText prefsPathEntry (T.pack path)
      )

  -- ── Preferences: reset ────────────────────────────────────────────
  _ <- on resetBtn #clicked $ do
    Gtk.editableSetText prefsPathEntry ("" :: Text)
    set prefsStatusLabel [#label := ""]

  -- ── Preferences: save ─────────────────────────────────────────────
  _ <- on savePrefsBtn #clicked $ do
    newPath <- T.unpack . T.strip <$> Gtk.editableGetText prefsPathEntry
    result  <- validateModsFolder newPath
    case result of
      Left err        -> set prefsStatusLabel [#label := err]
      Right validPath -> do
        modifyIORef stateRef (\s -> s { wsModPath = validPath })
        st <- readIORef stateRef
        saveWindowState configPath st
        rescanMods
        set prefsStatusLabel [#label := "Saved."]
        Gtk.popoverPopdown prefsPopover

  -- ── Save window state on close ────────────────────────────────────
  _ <- on win #closeRequest $ do
    maximized <- Gtk.windowIsMaximized win
    pos       <- fromIntegral <$> Gtk.panedGetPosition paned
    curState  <- readIORef stateRef
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

-- | Add a row to the list box for one mod with toggle and remove buttons.
addModRow :: Gtk.ListBox -> Adw.ApplicationWindow -> IO () -> ModInfo -> IO ()
addModRow listBox win rescan m = do
  let cat = case modPackageInfo m of
              Just info -> "  ·  " <> pkgCategory info
              Nothing   -> if modType m == "ts4script" then "  ·  Script Mod" else ""
      disabledTag = if modEnabled m then "" else "  (disabled)"
  row <- new Adw.ActionRow
    [ #title    := modName m
    , #subtitle := formatSize (modSize m) <> "  ·  " <> modType m <> cat <> disabledTag
    ]

  when (not (modEnabled m)) $
    Gtk.widgetAddCssClass row "mod-disabled"

  let toggleIcon = if modEnabled m
        then "media-playback-pause-symbolic" :: Text
        else "media-playback-start-symbolic"
      toggleTip = if modEnabled m then "Disable" :: Text else "Enable"
  toggleBtn <- new Gtk.Button [#iconName := toggleIcon]
  Gtk.widgetSetTooltipText toggleBtn (Just toggleTip)
  Gtk.widgetSetValign toggleBtn Gtk.AlignCenter
  _ <- on toggleBtn #clicked $ do
    let path = T.unpack (modPath m)
    result <- if modEnabled m then disableMod path else enableMod path
    case result of
      Left _  -> return ()
      Right _ -> rescan

  removeBtn <- new Gtk.Button [#iconName := "user-trash-symbolic"]
  Gtk.widgetSetTooltipText removeBtn (Just ("Remove" :: Text))
  Gtk.widgetAddCssClass removeBtn "destructive-action"
  Gtk.widgetSetValign removeBtn Gtk.AlignCenter
  _ <- on removeBtn #clicked $ do
    dialog <- new Adw.AlertDialog
      [ #heading := ("Remove Mod?" :: Text)
      , #body    := "This will permanently delete " <> modName m <> "."
      ]
    Adw.alertDialogAddResponse dialog "cancel" "Cancel"
    Adw.alertDialogAddResponse dialog "remove" "Remove"
    Adw.alertDialogSetResponseAppearance dialog "remove" Adw.ResponseAppearanceDestructive
    Adw.alertDialogSetDefaultResponse dialog (Just "cancel")
    Adw.alertDialogSetCloseResponse dialog "cancel"
    _ <- on dialog #response $ \response ->
      when (response == "remove") $ do
        result <- removeMod (T.unpack (modPath m))
        case result of
          Left _  -> return ()
          Right _ -> rescan
    Adw.dialogPresent dialog (Just win)

  Adw.actionRowAddSuffix row toggleBtn
  Adw.actionRowAddSuffix row removeBtn
  Gtk.listBoxAppend listBox row
