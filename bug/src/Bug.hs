{-# LANGUAGE RecursiveDo, OverloadedStrings, ScopedTypeVariables, TupleSections, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

-- import GHCJS.Types
-- import GHCJS.Marshal

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
       _asSelectedFeed :: Maybe RBFeed
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
      _asSelectedFeed = Nothing


selFeed :: Getter AppState (Maybe (T.Text,Feed))
selFeed = to $ \as -> do
            sel <- as ^. asSelectedFeed
            bfs <- M.lookup sel $ as ^. asFeeds
            rbfeedstate Nothing Nothing (fmap (sel^.feedName,)) bfs

processEvent :: AppEvent -> AppState -> AppState
processEvent (FeedFetched (bf,s)) =  asFeeds %~ M.insert bf (Loaded $ parseFeedString s)
processEvent (FeedSelected bf)    = (asFeeds %~ M.insert bf Loading) .
                                    (asSelectedFeed .~ Just bf)


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

  rec efeed      <- feedList dbfms -- Returns Event of RBFeed clicks
      void        $ itemList ditems
      ebffetched <- fmap FeedFetched <$> fetchFeed efeed 
      state      <- foldDyn processEvent initialState $ leftmost [ebffetched, fmap FeedSelected efeed]
      ditems     <- mapDyn (feedRBItems . view selFeed) state -- Items of selected feed
      dbfms      <- mapDyn (view asFeeds) state
  blank
  -- wombat =<< getPostBuild
  return ()

------------------------------------------------------------------------

-- | Returns an Event showing the retrieved feed contents as a String
fetchFeed :: forall t m . MonadWidget t m => Event t RBFeed -> m (Event t (RBFeed,String))
fetchFeed evt = do
      let req f = xhrRequest "GET" (show $ f^.feedURL) def
      traceEventWith (\s -> "[AJAX_RETURNED] " <> show (length s)) <$>
        performAJAX req (maybe "nope" T.unpack . _xhrResponse_body) evt

-- | Parse the Feed
feedRBItems :: Maybe (T.Text,Feed) -> [RBFeedItem]
feedRBItems (Just (s, RSSFeed rss)) = [RBFeedItem (maybe "NO_TITLE" ((s <>) . (" : " <>) . T.pack) $ rssItemTitle ri)
                                                 (parseURI =<< rssItemLink ri) |
                                      ri <- rssItems $ rssChannel rss]
feedRBItems _                       = [] -- [RBFeedItem "ERR..." Nothing]

------------------------------------------------------------------------

-- | Render a list of Feeds.
--   Returns an Event of clicked-on RBFeeds
feedList :: forall t m . MonadWidget t m => Dynamic t (M.Map RBFeed RBFeedState) -> m (Event t RBFeed)
feedList ds = do
    dfeeds <- mapDyn M.toList ds
    el "ul" $ selectList fst dfeeds feed


-- | Render an individual Feed
--   Returns an Event of when the Feed is clicked-on
feed :: forall t m . MonadWidget t m => Dynamic t ((RBFeed, RBFeedState), Bool) -> m (Event t RBFeed)
feed dbffs = mdo
  let styleItem :: RBFeedState -> M.Map String String
      styleItem = ("class" =:) . rbfeedstate "notloaded" "loading" (maybe "failed" (const "loaded"))

      numItems :: RBFeedState -> String
      numItems = rbfeedstate "[?] " "[...] " (("["<>) . (<>"] ") . show . length . feedRBItems . fmap ("",))

  dfeed     <- mapDyn (fst . fst) dbffs
  dfeedst   <- mapDyn (snd . fst) dbffs
  dsel      <- mapDyn snd         dbffs

  dfeedcnt  <- mapDyn numItems                   dfeedst
  datts     <- mapDyn styleItem                  dfeedst
  dfeedname <- mapDyn (T.unpack . view feedName) dfeed
  dattssel  <- selClassIf datts dsel

  (elt,_)   <- elDynAttr' "li" dattssel (do
                                      el "span" $ dynText dfeedcnt
                                      dynText dfeedname
                                     )
  return $ traceEvent "[FEED_CHOSEN]:"             $ tag (current dfeed) $
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


-- | This is a variant of 'simpleList' which manages a current selection.
--   The 'Bool' accepted by the rendering function indicates whether the
--   element in question is currently selected or not.
selectList :: forall t m a i . (Eq i, MonadWidget t m) =>
              (a -> i) -> Dynamic t [a] -> (Dynamic t (a, Bool) -> m (Event t i)) -> m (Event t i)
selectList getItem ds fn = mdo
  let ev :: Event t i
      ev = switch $ fmap leftmost $ current evs

      markSel :: [a] -> Maybe i -> [(a,Bool)]
      markSel as ma = [(a, ma == Just (getItem a)) | a <- as]

  dbs <- combineDyn markSel ds (dma :: Dynamic t (Maybe i))
  evs <- simpleList (dbs :: Dynamic t [(a,Bool)]) (fn :: (Dynamic t (a, Bool) -> m (Event t i)))
  dma <- holdDyn Nothing (Just <$> ev)
  return ev


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

------------------------------------------------------------------------
-- JS FFI Interop
{-
foreign import javascript unsafe "console['log']($1)" consoleLog :: JSRef a -> IO ()

-- Basic Haskell -> JS FFI Interop example
wombat :: forall t m . MonadWidget t m => Event t () -> m (Event t ())
wombat ev = performEventAsync (go <$ ev)
    where
      go :: (() -> IO ()) -> WidgetHost m ()
      go cb = do
              liftIO $ consoleLog =<< toJSRef ("Wombat" :: T.Text)
              liftIO $ cb ()
-}


