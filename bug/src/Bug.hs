{-# LANGUAGE RecursiveDo, OverloadedStrings, ScopedTypeVariables, TupleSections, TemplateHaskell, Rank2Types #-}
module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Network.URI
import Reflex
import Reflex.Dom
import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax

import qualified Data.Map  as M
import qualified Data.Text as T

------------------------------------------------------------------------

data RBFeed = RBFeed {
      _feedName :: T.Text,
      _feedURL  :: URI
    }
    deriving (Ord, Eq, Show)
$(makeLenses ''RBFeed)

data AppState = AppState {
       _asFeeds        :: M.Map RBFeed (Maybe Feed),
       _asSelectedFeed :: RBFeed
      }
$(makeLenses ''AppState)

data AppEvent = FeedLoaded (RBFeed, String)
              | FeedSelected RBFeed


initialState :: AppState
initialState = AppState { _asFeeds        = M.fromList $ zip rawFeeds (repeat Nothing),
                          _asSelectedFeed = head rawFeeds
                        }

selFeed :: AppState -> Maybe (T.Text, Feed)
selFeed as = do
  let sel = as^.asSelectedFeed
  case M.lookup sel $ as^.asFeeds of
    Just mf -> (sel^.feedName,) <$> mf
    _       -> Nothing

processEvent :: AppEvent -> AppState -> AppState
processEvent (FeedLoaded (bf,s)) = asFeeds %~ M.insert bf (parseFeedString s)
processEvent (FeedSelected bf)   = asSelectedFeed .~ bf


------------------------------------------------------------------------

main :: IO ()
main = mainWidgetWithHead (return ()) $
  do

  el "p" $ el "h3" $ text "Reflex 'simpleList' / 'performEventAsync' bug"
  el "p" $ do
           text "Click on the RSS Feeds below to load them. "
           text "The most-recently clicked feed is selected. "
           text "The RSS Feed Items of the currently selected feed are shown below. "
  el "p" $ text "If you switch between the feeds quickly enough you will see that the DOM 'div' nodes for one of the feeds sometimes get 'orphaned' and left in the DOM - this leads to the items of two feeds being shown simultaneously which should never happen."

  -- Manage the AppState as a fold over AppEvents
  rec state      <- foldDyn processEvent initialState $ leftmost [eloaded, fmap FeedSelected esel]
      ditems     <- mapDyn (parseFeedItems . selFeed) state -- Items of selected feed

      -- Render the List of Feeds
      esel       <- el "ul" $ do
                              let [f1,f2,f3] = rawFeeds
                              (elt1,_) <- el' "li" $ text "Feed 1"
                              (elt2,_) <- el' "li" $ text "Feed 2"
                              (elt3,_) <- el' "li" $ text "Feed 3"

                              return $ leftmost [
                                          f1 <$ domEvent Click elt1,
                                          f2 <$ domEvent Click elt2,
                                          f3 <$ domEvent Click elt3
                                         ]

      -- Render the List of Items in the currently-selected Feed
      let td = elAttr "td" ("style" =: "border: solid 1px")
      el "table" $ do
        el "tr" $ el "th" (text "widgetHold") >> el "th" (text "simpleList")
        el "tr" $ do
          td (widgetHold (items []) (items <$> updated ditems)) -- WORKS
          td (simpleList ditems (el "div" . dynText)) -- DOESN'T WORK (intermittently leaves orphan DOM nodes)

      -- Load the selected RSS Feed
      eloaded    <- loadFeed esel

  blank
  return ()

items :: forall t m . MonadWidget t m => [String] -> m ()
items = mapM_ (el "div" . text)

------------------------------------------------------------------------

-- | Returns an Event showing the loaded feed contents
loadFeed :: forall t m . MonadWidget t m => Event t RBFeed -> m (Event t AppEvent)
loadFeed evt = do
      let req f = xhrRequest "GET" (show $ f^.feedURL) def
      fmap FeedLoaded <$>
        performAJAX req (maybe "nope" T.unpack . _xhrResponse_body) evt


parseFeedItems :: Maybe (T.Text, Feed) -> [String]
parseFeedItems (Just (feed, RSSFeed rss)) = [(maybe "NO_TITLE" ((T.unpack feed<>) . (" : "<>)) $ rssItemTitle ri) |
                                             ri <- rssItems $ rssChannel rss]
parseFeedItems _                          = []

------------------------------------------------------------------------

rawFeeds :: [RBFeed]
rawFeeds = [
  RBFeed "Feed 1" (staticUri "http://apps.exeter.gov.uk/dnRSS/Feeds/MediaReleases_rss.xml"),
  RBFeed "Feed 2" (staticUri "https://www.middevonnewscentre.info/feed/"),
  RBFeed "Feed 3" (staticUri "https://www.devoncommunities.org.uk/handlers/rss.ashx?feed=1&amp;IDBlogGroup=e5ec3ceb-8a9c-4a03-ab38-6d0c97b5c49e")
  ]
  where
    staticUri s = fromMaybe (error $ "Bad static URI: " <> s) $ parseURI s


--
-- Function copied from 'reflex-dom-contrib':
-- [https://hackage.haskell.org/package/reflex-dom-contrib-0.1/docs/Reflex-Dom-Contrib-Xhr.html#v:performAJAX]
--
-- | This is the foundational primitive for the XHR API because it gives you
-- full control over request generation and response parsing and also allows
-- you to match things that generated the request with their corresponding
-- responses.
performAJAX
    :: (MonadWidget t m)
    => (a -> XhrRequest)  -- ^ Function to build the request
    -> (XhrResponse -> b) -- ^ Function to parse the response
    -> Event t a
    -> m (Event t (a, b))
performAJAX mkRequest parseResponse req =
    performEventAsync $ ffor req $ \a cb -> do
      _ <- newXMLHttpRequest (mkRequest a) $ \response ->
             liftIO $ cb (a, parseResponse response)
      return ()

