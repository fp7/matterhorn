{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

-- | This module provides the Drawing functionality for the
-- ChannelList sidebar.  The sidebar is divided vertically into groups
-- and each group is rendered separately.
--
-- There are actually two UI modes handled by this code:
--
--   * Normal display of the channels, with various markers to
--     indicate the current channel, channels with unread messages,
--     user state (for Direct Message channels), etc.
--
--   * ChannelSelect display where the user is typing match characters
--     into a prompt at the ChannelList sidebar is showing only those
--     channels matching the entered text (and highlighting the
--     matching portion).

module Matterhorn.Draw.ChannelList (renderChannelList, renderChannelListHeader) where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center (hCenter)
import           Data.Bifunctor ( bimap )
import qualified Data.Text as T
import           Lens.Micro.Platform (non)

import qualified Network.Mattermost.Types as MM

import           Matterhorn.Constants ( userSigil )
import           Matterhorn.Draw.Util
import           Matterhorn.State.Channels
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.Common ( sanitizeUserText )
import qualified Matterhorn.Zipper as Z

-- | Internal record describing each channel entry and its associated
-- attributes.  This is the object passed to the rendering function so
-- that it can determine how to render each channel.
data ChannelListEntryData =
    ChannelListEntryData { entrySigil       :: Text
                         , entryLabel       :: Text
                         , entryHasUnread   :: Bool
                         , entryMentions    :: Int
                         , entryIsRecent    :: Bool
                         , entryIsReturn    :: Bool
                         , entryIsCurrent   :: Bool
                         , entryIsMuted     :: Bool
                         , entryUserStatus  :: Maybe UserStatus
                         }

renderChannelListHeader :: ChatState -> Widget Name
renderChannelListHeader st =
    vBox [ teamHeader
         , selfHeader
         , unreadCountHeader
         ]
    where
        myUsername_ = myUsername st
        teamHeader = hCenter $
                     withDefAttr clientEmphAttr $
                     txt $ "Team: " <> teamNameStr
        selfHeader = hCenter $
                     colorUsername myUsername_ myUsername_
                         (T.singleton statusSigil <> " " <> userSigil <> myUsername_)
        teamNameStr = sanitizeUserText $ MM.teamDisplayName $ st^.csMyTeam
        statusSigil = maybe ' ' userSigilFromInfo me
        me = userById (myUserId st) st
        unreadCountHeader = hCenter $ txt $ "Unread: " <> (T.pack $ show unreadCount)
        unreadCount = sum $ (channelListGroupUnread . fst) <$> Z.toList (st^.csFocus)

renderChannelList :: ChatState -> Widget Name
renderChannelList st = vBox $ renderChannelListHeader st : body
    where
        myUsername_ = myUsername st
        vspec = groupVSpec st
        rndr renderEach zipper = let l = Z.toList zipper
                                 in renderChannelListGroup st (vspec l) renderEach <$> l
        body = case appMode st of
            ChannelSelect ->
                let zipper = st^.csChannelSelectState.channelSelectMatches
                in if Z.isEmpty zipper
                   then [hCenter $ txt "No matches"]
                   else rndr (renderChannelSelectListEntry (Z.focus zipper)) zipper
            _ ->
                let renderEntry s = renderChannelListEntry myUsername_ . mkChannelEntryData s
                in [ cached ChannelSidebar $
                     vBox $
                     rndr renderEntry (st^.csFocus)
                   ]

channelGroupName :: ChannelListGroup -> Name
channelGroupName = \case
    ChannelGroupPublicChannels _  -> PublicChannelList
    ChannelGroupPrivateChannels _ -> PrivateChannelList
    ChannelGroupDirectMessages _  -> DirectChannelList

type ChanListGroupVSpecs = [(Name, (Widget Name -> Widget Name, Text))]

-- | The 'groupVSpec' function provides some relatively complex logic
-- that is designed to handle the vertical layout for the ChannelList
-- bar.
--
--  * The layout is based on the vertical layout extent of the channel
--    list area ('csChannelListVSize') the *last* time it was
--    rendered.  This means that the layout could be sub-optimal,
--    especially just after a terminal window resize, although it's
--    expected to self-correct after some updates.
--
--  * If the vertical layout extent is not known, no restrictions are
--    placed on the channel group lists which means that the Brick
--    VBox will treat them all as "Greedy" and split the vertical
--    space equally; this is a reasonable fallback condition.
--
--  * The Public channels are always treated as "Greedy" to allow them
--    to take up any space not needed for other groups.
--
--  * The worst-case for when the contents of all groups exceeds the
--    available space is to allow each equal space.  The maximum
--    amount of space used for all other groups should still leave the
--    Public channel group with its minimum amount of space (vertical
--    space / ngroups).
--
--  * By default the remaining non-Public group space is equally
--    divided among all other groups, but if a group does not *need*
--    the amount of space allocated to it, that space will be
--    distributed to other groups to use.  When all groups do not need
--    the amount of space allowed to them, the Public group will be
--    allowed to grow greedily and consume their space.
--
--  * The minimum vertical size for each group is 2 entries.
--
--  * Only the groups that are constrained vertically will have a Name
--    vLimit entry in the output; any channel group entry without an
--    entry in this list will be layed out with default vertical
--    consumption (i.e. "Greedy").
--
--  * This function makes an assumption that the Public group is
--    present and should be greedy; it makes no assumptions about any
--    other channel groups and is designed to work with any number of
--    channel groups.

groupVSpec :: ChatState -> [(ChannelListGroup, [e])] -> ChanListGroupVSpecs
groupVSpec st cglist =
  let numGroups = length cglist
      Just groupsVSize = st^.csChannelListVSize  -- safe due to main case below
      isGreedy = \case
        ChannelGroupPublicChannels _ -> True
        _ -> False
      fixedChans = filter (not . isGreedy . fst) cglist
      numFixed = length fixedChans
      equalSz = groupsVSize `div` numGroups
      extra l = if l < equalSz then equalSz - l else 0
      fixedExtraPer = sum ((extra . length . snd) <$> fixedChans) `div` numFixed
      fixedGrpSz = min (equalSz + fixedExtraPer)
      extraText n s = T.pack $ if n > s
                               then " (" <> show s <> "/" <> show n <> ")"
                               else "" -- debug: <> show groupsVSize
      vLimFunAndText gEnts = let n = length gEnts
                                 s = fixedGrpSz n
                                 v = clamp 2 s n
                             in (vLimit v, extraText n s)
      name'LimAndText = bimap channelGroupName vLimFunAndText
      name'Text = bimap channelGroupName ((,) id . greedyText)
      greedyText gEnts = let n = length gEnts
                             totalFixedSz = sum $ (fixedGrpSz . length . snd) <$> fixedChans
                             numGreedy = numGroups - numFixed
                             s = (groupsVSize - totalFixedSz) `div` numGreedy
                         in extraText n s
  in case st^.csChannelListVSize of
       Nothing -> []  -- all greedy, Brick vBox handles layout
       Just _ -> (name'LimAndText <$> fixedChans) <>
                 -- add title supplement for greedy sections in case
                 -- not all are shown due to size restrictions
                 (name'Text <$> filter (isGreedy . fst) cglist)

renderChannelListGroupHeading :: ChannelListGroup -> Text -> Widget Name
renderChannelListGroupHeading g extra =
    let (unread, label) = case g of
            ChannelGroupPublicChannels u -> (u, "Public")
            ChannelGroupPrivateChannels u -> (u, "Private")
            ChannelGroupDirectMessages u -> (u, "Direct")
        addUnread = if unread > 0
                    then (<+> (withDefAttr unreadGroupMarkerAttr $ txt "*"))
                    else id
        labelWidget = addUnread $ withDefAttr channelListHeaderAttr $ txt $ label <> extra
    in hBorderWithLabel labelWidget

renderChannelListGroup :: ChatState
                       -> ChanListGroupVSpecs
                       -> (ChatState -> e -> Widget Name)
                       -> (ChannelListGroup, [e])
                       -> Widget Name
renderChannelListGroup st vspec renderEntry (group, es) =
    let heading = renderChannelListGroupHeading group titleExtra
        nm = channelGroupName group
        Just (setHeight, titleExtra) = lookup nm vspec <|> Just (id, "")
        entryWidgets = renderEntry st <$> es
    in if null entryWidgets
       then emptyWidget
       else vBox [ heading,
                   setHeight $ viewport nm Vertical $ vBox entryWidgets
                 ]

mkChannelEntryData :: ChatState
                   -> ChannelListEntry
                   -> ChannelListEntryData
mkChannelEntryData st e =
    ChannelListEntryData { entrySigil       = sigilWithSpace
                         , entryLabel       = name
                         , entryHasUnread   = unread
                         , entryMentions    = mentions
                         , entryIsRecent    = recent
                         , entryIsReturn    = ret
                         , entryIsCurrent   = current
                         , entryIsMuted     = muted
                         , entryUserStatus  = status
                         }
    where
        cId = channelListEntryChannelId e
        Just chan = findChannelById cId (st^.csChannels)
        unread = hasUnread' chan
        recent = isRecentChannel st cId
        ret = isReturnChannel st cId
        current = isCurrentChannel st cId
        muted = isMuted chan
        (name, normalSigil, addSpace, status) = case e of
            CLChannel _ ->
                (chan^.ccInfo.cdDisplayName, Nothing, False, Nothing)
            CLGroupDM _ ->
                (chan^.ccInfo.cdDisplayName, Just " ", True, Nothing)
            CLUserDM _ uId ->
                let Just u = userById uId st
                    uname = if useNickname st
                            then u^.uiNickName.non (u^.uiName)
                            else u^.uiName
                in (uname, Just $ T.singleton $ userSigilFromInfo u,
                    True, Just $ u^.uiStatus)
        sigilWithSpace = sigil <> if addSpace then " " else ""
        prevEditSigil = "»"
        sigil = if current
                then fromMaybe "" normalSigil
                else case chan^.ccEditState.eesInputHistoryPosition of
                    Just _ -> prevEditSigil
                    Nothing ->
                        case chan^.ccEditState.eesLastInput of
                            ("", _) -> fromMaybe "" normalSigil
                            _       -> prevEditSigil
        mentions = chan^.ccInfo.cdMentionCount

-- | Render an individual Channel List entry (in Normal mode) with
-- appropriate visual decorations.
renderChannelListEntry :: Text -> ChannelListEntryData -> Widget Name
renderChannelListEntry myUName entry = body
    where
    body = decorate $ decorateEntry entry $ decorateMentions $ padRight Max $
           entryWidget $ entrySigil entry <> entryLabel entry
    decorate = if | entryIsCurrent entry ->
                      visible . forceAttr currentChannelNameAttr
                  | entryMentions entry > 0 && not (entryIsMuted entry) ->
                      forceAttr mentionsChannelAttr
                  | entryHasUnread entry ->
                      forceAttr unreadChannelAttr
                  | otherwise -> id
    entryWidget = case entryUserStatus entry of
                    Just Offline -> withDefAttr clientMessageAttr . txt
                    Just _       -> colorUsername myUName (entryLabel entry)
                    Nothing      -> txt
    decorateMentions
      | entryMentions entry > 9 =
        (<+> str "(9+)")
      | entryMentions entry > 0 =
        (<+> str ("(" <> show (entryMentions entry) <> ")"))
      | entryIsMuted entry =
        (<+> str "(m)")
      | otherwise = id

-- | Render an individual entry when in Channel Select mode,
-- highlighting the matching portion, or completely suppressing the
-- entry if it doesn't match.
renderChannelSelectListEntry :: Maybe ChannelSelectMatch
                             -> ChatState
                             -> ChannelSelectMatch
                             -> Widget Name
renderChannelSelectListEntry curMatch st match =
    let ChannelSelectMatch preMatch inMatch postMatch _ entry = match
        maybeSelect = if (Just entry) == (matchEntry <$> curMatch)
                      then visible . withDefAttr currentChannelNameAttr
                      else id
        entryData = mkChannelEntryData st entry
    in maybeSelect $
       decorateEntry entryData $
       padRight Max $
         hBox [ txt $ entrySigil entryData <> preMatch
              , forceAttr channelSelectMatchAttr $ txt inMatch
              , txt postMatch
              ]

-- If this channel is the return channel, add a decoration to denote
-- that.
--
-- Otherwise, if this channel is the most recently viewed channel (prior
-- to the currently viewed channel), add a decoration to denote that.
decorateEntry :: ChannelListEntryData -> Widget n -> Widget n
decorateEntry entry =
    if entryIsReturn entry
    then (<+> (withDefAttr recentMarkerAttr $ str returnChannelSigil))
    else if entryIsRecent entry
         then (<+> (withDefAttr recentMarkerAttr $ str recentChannelSigil))
         else id

recentChannelSigil :: String
recentChannelSigil = "<"

returnChannelSigil :: String
returnChannelSigil = "~"
