module Draw.ReactionEmojiListOverlay
  ( drawReactionEmojiListOverlay
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.List ( listSelectedFocusedAttr )
import qualified Data.Text as T

import           Draw.Main
import           Draw.ListOverlay ( drawListOverlay )
import           Emoji ( EmojiCollection, getEmojiText )
import           Types
import           Themes


drawReactionEmojiListOverlay :: ChatState -> [Widget Name]
drawReactionEmojiListOverlay st =
    let em = st^.csResources.crEmoji
        overlay = drawListOverlay (st^.csReactionEmojiListOverlay)
                                  (const $ txt "Search Emoji")
                                  (const $ txt "No matching emoji found.")
                                  (const $ txt "Search emoji:")
                                  (renderEmoji em)
    in joinBorders overlay : drawMain False st

renderEmoji :: EmojiCollection -> Bool -> (Bool, T.Text) -> Widget Name
renderEmoji em sel (mine, e) =
    let maybeForce = if sel
                     then forceAttr listSelectedFocusedAttr
                     else id
    in maybeForce $
       padRight Max $
       hBox [ if mine then txt " * " else txt "   "
            , hLimit 4 $ padRight Max $ txt $ fromMaybe "?" $ getEmojiText em e
            , withDefAttr emojiAttr $ txt $ ":" <> e <> ":"
            ]
