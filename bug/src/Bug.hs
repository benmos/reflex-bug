{-# LANGUAGE RecursiveDo, OverloadedStrings, ScopedTypeVariables, TupleSections, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Network.URI
import Reflex
import Reflex.Dom
import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax

import qualified Data.Map as M
import qualified Data.Text as T

------------------------------------------------------------------------

data RBFeed = RBFeed {
      _feedName :: T.Text,
      _feedURL :: URI
    }
    deriving (Ord, Eq, Show)
$(makeLenses ''RBFeed)

data RBFeedState = NotLoaded
                 | Loading
                 | Loaded (Maybe Feed)
                  deriving (Show)

data RBFeedItem = RBFeedItem {
        itemTitle :: T.Text,
        itemURI   :: Maybe URI
      }
      deriving (Eq)

data AppState = AppState {
       _asFeeds        :: M.Map RBFeed RBFeedState,
       _asSelectedFeed :: RBFeed
      }
$(makeLenses ''AppState)

data AppEvent = FeedFetched (RBFeed, String)
              | FeedSelected RBFeed

------------------------------------------------------------------------

-- | Fold for 'RBFeedState'
rbfeedstate :: a -> a -> (Maybe Feed -> a) -> RBFeedState -> a
rbfeedstate nl _ _ NotLoaded   = nl
rbfeedstate _  l _ Loading     = l
rbfeedstate _  _ f (Loaded mf) = f mf

initialState :: AppState
initialState = AppState {..}
    where
      _asFeeds        = M.fromList $ zip rawFeeds (repeat NotLoaded)
      _asSelectedFeed = head rawFeeds


selFeed :: Getter AppState (Maybe (T.Text,Feed))
selFeed = to $ \as -> do
            let sel = as ^. asSelectedFeed
            bfs <- M.lookup sel $ as ^. asFeeds
            rbfeedstate Nothing Nothing (fmap (sel^.feedName,)) bfs

processEvent :: AppEvent -> AppState -> AppState
processEvent (FeedFetched (bf,s)) =  asFeeds %~ M.insert bf (Loaded $ parseFeedString s)
processEvent (FeedSelected bf)    = (asFeeds %~ M.insert bf Loading) .
                                    (asSelectedFeed .~ bf)


------------------------------------------------------------------------

main :: IO ()
main = mainWidgetWithHead (el "style" $ do
                             text ".sel       {color:blue}"
                             text ".loading   {color:green}"
                             text ".loaded    {color:pink}"
                             text ".notloaded {color:grey}"
                             text ".failed    {color:red}"
                          ) $
  do

  el "p" $ el "h3" $ text "Reflex 'simpleList' combinator bug"
  el "p" $ do
           text "Click on the RSS Feeds below to load them. "
           text "The most-recently clicked feed is selected. "
           text "The RSS Feed Items of the currently selected feed are shown below. "
  el "p" $ text "If you switch between the feeds quickly enough you will see that the DOM 'div' nodes for one of the feeds sometimes get 'orphaned' and left in the DOM - this leads to the items of two feeds being shown simultaneously which should never happen."
  rec state      <- foldDyn processEvent initialState $ leftmost [efetched, fmap FeedSelected esel]
      ditems     <- mapDyn (feedRBItems . view selFeed) state -- Items of selected feed

      -- Render the UI
      esel       <- feedList
      void        $ simpleList ditems (el "div" . dynText <=< mapDyn (T.unpack . itemTitle))

      efetched   <- fetchFeed esel -- Load the selected RSS Feed

  blank
  return ()

------------------------------------------------------------------------

-- | Returns an Event showing the retrieved feed contents as a String
fetchFeed :: forall t m . MonadWidget t m => Event t RBFeed -> m (Event t AppEvent)
fetchFeed evt = do
      let req f = xhrRequest "GET" (show $ f^.feedURL) def
      fmap FeedFetched <$>
        performAJAX req (maybe "nope" T.unpack . _xhrResponse_body) evt

-- | Parse the Feed
feedRBItems :: Maybe (T.Text,Feed) -> [RBFeedItem]
feedRBItems (Just (s, RSSFeed rss)) = [RBFeedItem (maybe "NO_TITLE" prepTitle $ rssItemTitle ri)
                                                  (parseURI =<< rssItemLink ri) |
                                       let prepTitle = ((s <>) . (" : " <>) . T.pack),
                                       ri <- rssItems $ rssChannel rss]
feedRBItems _                       = []

------------------------------------------------------------------------

-- | Render a list of Feeds.
--   Returns an Event of clicked-on RBFeeds
feedList :: forall t m . MonadWidget t m => m (Event t RBFeed)
feedList = el "ul" $ do
  let [f1,f2,f3] = rawFeeds
  (elt1,_) <- el' "li" $ text "Feed 1"
  (elt2,_) <- el' "li" $ text "Feed 2"
  (elt3,_) <- el' "li" $ text "Feed 3"

  return $ leftmost [
              f1 <$ domEvent Click elt1,
              f2 <$ domEvent Click elt2,
              f3 <$ domEvent Click elt3
             ]
  

------------------------------------------------------------------------

rawFeeds :: [RBFeed]
rawFeeds = [
  RBFeed "Feed 1" (staticUri "http://apps.exeter.gov.uk/dnRSS/Feeds/MediaReleases_rss.xml"),
  RBFeed "Feed 2" (staticUri "https://www.middevonnewscentre.info/feed/"),
  RBFeed "Feed 3" (staticUri "https://www.devoncommunities.org.uk/handlers/rss.ashx?feed=1&amp;IDBlogGroup=e5ec3ceb-8a9c-4a03-ab38-6d0c97b5c49e")
  ]
  where
    staticUri s = fromMaybe (error $ "Bad static URI: " <> s) $ parseURI s


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

