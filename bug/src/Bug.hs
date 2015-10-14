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
      dbfms      <- mapDyn (view asFeeds)               state
      dsel       <- mapDyn (view asSelectedFeed)        state

      -- Render the UI
      esel       <- feedList dsel dbfms -- Returns Event of RBFeed clicks
      void        $ itemList ditems

      efetched   <- fetchFeed esel -- Load the selected RSS Feed

  blank
  return ()

------------------------------------------------------------------------

-- | Returns an Event showing the retrieved feed contents as a String
fetchFeed :: forall t m . MonadWidget t m => Event t RBFeed -> m (Event t AppEvent)
fetchFeed evt = do
      let req f = xhrRequest "GET" (show $ f^.feedURL) def
      fmap FeedFetched . traceEventWith (\s -> "[AJAX_RETURNED] " <> show (length s)) <$>
        performAJAX req (maybe "nope" T.unpack . _xhrResponse_body) evt

-- | Parse the Feed
feedRBItems :: Maybe (T.Text,Feed) -> [RBFeedItem]
feedRBItems (Just (s, RSSFeed rss)) = [RBFeedItem (maybe "NO_TITLE" ((s <>) . (" : " <>) . T.pack) $ rssItemTitle ri)
                                                 (parseURI =<< rssItemLink ri) |
                                      ri <- rssItems $ rssChannel rss]
feedRBItems _                       = []

------------------------------------------------------------------------

-- | Render a list of Feeds.
--   Returns an Event of clicked-on RBFeeds
feedList :: forall t m . MonadWidget t m => Dynamic t RBFeed -> Dynamic t (M.Map RBFeed RBFeedState) -> m (Event t RBFeed)
feedList dsel ds = el "ul" $ selectViewListWithKey_ dsel ds feed


-- | Render an individual Feed
--   Returns an Event of when the Feed is clicked-on
feed :: forall t m . MonadWidget t m => RBFeed -> Dynamic t RBFeedState -> Dynamic t Bool -> m (Event t RBFeed)
feed f dfeedst dsel = mdo
  let styleItem :: RBFeedState -> M.Map String String
      styleItem = ("class" =:) . rbfeedstate "notloaded" "loading" (maybe "failed" (const "loaded"))

      numItems :: RBFeedState -> String
      numItems = rbfeedstate "[?] " "[...] " (("["<>) . (<>"] ") . show . length . feedRBItems . fmap ("",))

  dfeedcnt  <- mapDyn numItems  dfeedst
  datts     <- mapDyn styleItem dfeedst
  dattssel  <- selClassIf datts dsel

  (elt,_)   <- elDynAttr' "li" dattssel (do
                                      el "span" $ dynText dfeedcnt
                                      text $ T.unpack $ f^.feedName
                                     )
  return $ traceEvent "[FEED_CHOSEN]:"             $ fmap (const f) $
           traceEventWith (const "[FEED_CLICKED]") $ domEvent Click elt



itemList :: MonadWidget t m => Dynamic t [RBFeedItem] -> m ()
itemList items = void $ simpleList items item

item :: forall t m . MonadWidget t m => Dynamic t RBFeedItem -> m ()
item disel = do
  dtitle <- mapDyn (T.unpack . itemTitle) disel
  el "div" $ dynText dtitle

------------------------------------------------------------------------


staticUri :: String -> URI
staticUri s = fromMaybe (error $ "Bad static URI: " <> s) $ parseURI s

rawFeeds :: [RBFeed]
rawFeeds = [
  RBFeed "Feed 1" (staticUri "http://apps.exeter.gov.uk/dnRSS/Feeds/MediaReleases_rss.xml"),
  RBFeed "Feed 2" (staticUri "https://www.middevonnewscentre.info/feed/"),
  RBFeed "Feed 3" (staticUri "https://www.devoncommunities.org.uk/handlers/rss.ashx?feed=1&amp;IDBlogGroup=e5ec3ceb-8a9c-4a03-ab38-6d0c97b5c49e")
  ]


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

-- | Add a 'sel' class to (dynamically) indicate whether something is selected or not
selClassIf :: MonadWidget t m => Dynamic t (M.Map String String) -> Dynamic t Bool -> m (Dynamic t (M.Map String String))
selClassIf dattr dsel = combineDyn (\attr sel -> if sel then ("class" =: "sel") .<>. attr else attr) dattr dsel
  where
    (.<>.) = M.unionWith (\v1 v2 -> v1 <> "  " <> v2) -- Combine eg multiple "class" attr vlas

