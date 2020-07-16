module State.UrlSelect
  (
  -- * URL selection mode
    startUrlSelect
  , stopUrlSelect
  , openSelectedURL
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Widgets.List ( list, listMoveTo, listSelectedElement )
import qualified Data.Vector as V
import           Lens.Micro.Platform ( (.=), to )

import           State.Common
import           Types
import           Util


startUrlSelect :: MH ()
startUrlSelect = do
    st <- use id
    let baseUrl = getServerBaseUrl st
    urls <- use (csCurrentChannel.to (findUrls baseUrl).to V.fromList)
    setMode UrlSelect
    csUrlList .= (listMoveTo (length urls - 1) $ list UrlList urls 2)

stopUrlSelect :: MH ()
stopUrlSelect = setMode Main

openSelectedURL :: MH ()
openSelectedURL = whenMode UrlSelect $ do
    selected <- use (csUrlList.to listSelectedElement)
    case selected of
        Nothing -> return ()
        Just (_, link) -> do
            opened <- openURL (OpenLinkChoice link)
            when (not opened) $ do
                mhError $ ConfigOptionMissing "urlOpenCommand"
                setMode Main

findUrls :: ServerBaseURL -> ClientChannel -> [LinkChoice]
findUrls serverBaseUrl chan =
    let msgs = chan^.ccContents.cdMessages
    in removeDuplicates $ concat $ toList $ toList <$> msgURLs serverBaseUrl <$> msgs

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = nubOn (\ l -> (l^.linkURL, l^.linkUser))
