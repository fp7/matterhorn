module Types.KeyEvents where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

-- | This enum represents all the possible key events a user might
--   want to use.
data KeyEvent
  = VtyRefreshEvent
  | ShowHelpEvent
  | EnterSelectModeEvent
  | ReplyRecentEvent
  | ToggleMessagePreviewEvent
  | InvokeEditorEvent
  | EnterFastSelectModeEvent
  | QuitEvent
  | NextChannelEvent
  | PrevChannelEvent
  | NextUnreadChannelEvent
  | LastChannelEvent
  | EnterOpenURLModeEvent
  | ClearUnreadEvent
  | ToggleMultiLineEvent
  | EnterFlaggedPostsEvent

  -- generic cancel
  | CancelEvent

  -- channel-scroll-specific
  | LoadMoreEvent
  | OpenMessageURLEvent

  -- scrolling events---maybe rebindable?
  | ScrollUpEvent
  | ScrollDownEvent
  | PageUpEvent
  | PageDownEvent
  | ScrollTopEvent
  | ScrollBottomEvent
    deriving (Eq, Show, Ord, Enum)

allEvents :: [KeyEvent]
allEvents =
  [ VtyRefreshEvent
  , ShowHelpEvent
  , EnterSelectModeEvent
  , ReplyRecentEvent
  , ToggleMessagePreviewEvent
  , InvokeEditorEvent
  , EnterFastSelectModeEvent
  , QuitEvent
  , NextChannelEvent
  , PrevChannelEvent
  , NextUnreadChannelEvent
  , LastChannelEvent
  , EnterOpenURLModeEvent
  , ClearUnreadEvent
  , ToggleMultiLineEvent
  , CancelEvent
  , EnterFlaggedPostsEvent

  , LoadMoreEvent
  , OpenMessageURLEvent
  ]

data Binding = Binding
  { kbMods :: [Vty.Modifier]
  , kbKey  :: Vty.Key
  } deriving (Eq, Show, Ord)

type KeyConfig = M.Map KeyEvent [Binding]

bindingFromString :: T.Text -> Either String Binding
bindingFromString kb = go (T.splitOn "-" kb) []
  where go [k] mods = do
          key <- pKey k
          return Binding { kbMods = mods, kbKey = key }
        go (k:ks) mods = do
          m <- case k of
            "S" -> return Vty.MShift
            "M" -> return Vty.MMeta
            "A" -> return Vty.MAlt
            "C" -> return Vty.MCtrl
            _   -> Left ("Unknown modifier prefix: " ++ show k)
          go ks (m:mods)
        go [] _ = Left "Empty keybinding not allowed"
        pKey "esc"       = return Vty.KEsc
        pKey "backspace" = return Vty.KBS
        pKey "enter"     = return Vty.KEnter
        pKey "left"      = return Vty.KLeft
        pKey "right"     = return Vty.KRight
        pKey "up"        = return Vty.KUp
        pKey "down"      = return Vty.KDown
        pKey "upleft"    = return Vty.KUpLeft
        pKey "upright"   = return Vty.KUpRight
        pKey "downleft"  = return Vty.KDownLeft
        pKey "downright" = return Vty.KDownRight
        pKey "center"    = return Vty.KCenter
        pKey "backtab"   = return Vty.KBackTab
        pKey "prtscr"    = return Vty.KPrtScr
        pKey "pause"     = return Vty.KPause
        pKey "ins"       = return Vty.KIns
        pKey "home"      = return Vty.KHome
        pKey "pgup"      = return Vty.KPageUp
        pKey "del"       = return Vty.KDel
        pKey "end"       = return Vty.KEnd
        pKey "pgdown"    = return Vty.KPageDown
        pKey "begin"     = return Vty.KBegin
        pKey "menu"      = return Vty.KMenu
        pKey t
          | Just n <- T.stripPrefix "f" t =
              return (Vty.KFun (read (T.unpack n)))
          | Just (c, "") <- T.uncons t =
              return (Vty.KChar c)
          | otherwise = Left ("Unknown keybinding: " ++ show t)

bindingListFromString :: T.Text -> Either String [Binding]
bindingListFromString =
  mapM (bindingFromString . T.strip) . T.splitOn ","

keyEventFromString :: T.Text -> Either String KeyEvent
keyEventFromString t =
    let mapping = M.fromList [ (keyEventToString e, e) | e <- allEvents ]
    in case M.lookup t mapping of
        Just e -> return e
        Nothing -> Left ("Unknown event: " ++ show t)

keyEventToString :: KeyEvent -> T.Text
keyEventToString ev = case ev of
  QuitEvent                 -> "quit"
  VtyRefreshEvent           -> "vty-refresh"
  ClearUnreadEvent          -> "clear-unread"

  ReplyRecentEvent          -> "reply-recent"
  ToggleMessagePreviewEvent -> "toggle-message-preview"
  InvokeEditorEvent         -> "invoke-editor"
  ToggleMultiLineEvent      -> "toggle-multiline"
  CancelEvent               -> "cancel"

  EnterFastSelectModeEvent  -> "enter-fast-select"
  NextChannelEvent          -> "focus-next-channel"
  PrevChannelEvent          -> "focus-prev-channel"
  NextUnreadChannelEvent    -> "focus-next-unread"
  LastChannelEvent          -> "focus-last-channel"

  EnterFlaggedPostsEvent    -> "show-flagged-posts"
  ShowHelpEvent             -> "show-help"
  EnterSelectModeEvent      -> "select-mode"
  EnterOpenURLModeEvent     -> "enter-url-open"

  LoadMoreEvent             -> "load-more"
  OpenMessageURLEvent       -> "open-message-url"

  ScrollUpEvent     -> "scroll-up"
  ScrollDownEvent   -> "scroll-down"
  PageUpEvent       -> "page-up"
  PageDownEvent     -> "page-down"
  ScrollTopEvent    -> "scroll-top"
  ScrollBottomEvent -> "scroll-bottom"