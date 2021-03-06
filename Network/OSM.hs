{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, OverloadedStrings
    , GeneralizedNewtypeDeriving, FlexibleContexts, RecordWildCards
    , QuasiQuotes, GADTs, UndecidableInstances, ViewPatterns #-}
module Network.OSM
  (  -- * Basic Types
    TileID(..)
  , TileCoords(..)
  , Zoom
    -- * Types for tile caching
  , OSMConfig(..)
  , OSMState
  , OSM
    -- * High-level (caching) Operations
  , evalOSM
  , getBestFitTiles
  , getTiles
  , getTile
  , defaultOSMConfig
    -- * Network Operations
  , downloadBestFitTiles
  , downloadTiles
  , downloadTile
  , osmTileURL
  -- * Frame-oriented operations
  , Frame(..)
  , selectTilesForFrame
  , tileCoordsForFrame
  , point2pixel
  , pixel2point
  , getFrameHeight
  -- * Helper Functions
  , tile2point
  , point2tile
  , pixelPosForCoord
  , coordForPixelPos
  , determineTileCoords
  , selectedTiles
  -- * Legal
  , osmCopyrightText
  ) where

import Control.Monad
import Control.Monad.Base (liftBase)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Geo.Computations
import Data.Maybe
import Data.Word
import Network.HTTP.Conduit
import Network.HTTP.Types ( Status, ok200, ResponseHeaders
                          , parseSimpleQuery, serviceUnavailable503)
import Data.Default

-- For the cacheing
import Control.Concurrent.MonadIO (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ask)
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Database.Persist
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH
import Database.Sqlite (Error)
import Data.Char (isDigit)
import Data.Conduit
import Data.Data
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import System.Directory
import Paths_osm_download
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Control.Exception as X

import Network.OSM.Types

-- FIXME zoom should be a newtype with Num instance that saturates (doesn't over/under flow)
type Zoom = Int

-- | The official OSM tile server.
osmTileURL :: String
osmTileURL = "http://tile.openstreetmap.org"

-- |The coordinates associated with any particular GPS location
-- can be computed using 'determineTileCoords' and converted into tile ids
-- using 'selectedTiles' before final download with 'downloadTiles'.

-- |OSM defined method of converting a coordinate and zoom level to a tile
point2tile :: Point -> Zoom -> TileID
point2tile a z =
  let t = pntLat a
      g = pntLon a
      (x,y) = point2tileRaw t g z
  in TID (floor x, floor y)

-- @
--   n = 2 ^ zoom
--   xtile = ((lon_deg + 180) / 360) * n
--   ytile = (1 - (log(tan(lat_rad) + sec(lat_rad)) / π)) / 2 * n
-- @
point2tileRaw :: Double -> Double -> Zoom -> (Double,Double)
point2tileRaw latitude longitude zoom =
             let n = 2^zoom
                 xtile = ((longitude+180) / 360) * n
                 tmp = log (tan (latitude*pi / 180) + secant (latitude * pi / 180))
                 ytile = ((1-tmp / pi) / 2.0) * n
             in (xtile,ytile)
 where
  secant :: Floating a => a -> a
  secant a = 1 / cos a

-- Equations provided by OSM to compute GPS coordinate from a tile
-- n = 2 ^ zoom
-- lon_deg = xtile / n * 360.0 - 180.0
-- lat_rad = arctan(sinh(π * (1 - 2 * ytile / n)))
-- lat_deg = lat_rad * 180.0 / π
tile2point :: TileID -> Zoom -> Point
tile2point (TID (fromIntegral -> x, fromIntegral -> y)) zoom =
  let n = 2^zoom
      lon_deg = x / n * 360 - 180
      lat_rad = atan ( sinh ( pi * (1 - 2 * y / n)))
  in pt (lat_rad * 180/pi) lon_deg Nothing Nothing

-- A frame is a point of view including the number of pixels
-- (width,height), center, and zoom.  All pixel positions with respect
-- to a frame are from the lower left corner of the lower left tile of
-- the grid that displays the frame.
data Frame = Frame { width,height :: Int
                   , center       :: Point
                   , frameZoom    :: Zoom
                   } deriving (Eq, Ord, Show, Read)

-- |Given a width, height and center, compute the tiles needed to fill
-- the display.
--
-- THIS ASSUMES tiles are 256x256 pixels!
selectTilesForFrame :: Frame  -> [[TileID]]
selectTilesForFrame (Frame w h center z) =
  let (x,y) = point2tileRaw (pntLat center) (pntLon center) z
      nrColumns2, nrRows2 :: Int
      nrColumns2 = 1 + ceiling (fromIntegral w / 512)
      nrRows2    = 1 + ceiling (fromIntegral h / 512)
      -- FIXME hardcoding the tile server tile pixel width
  in [ [ TID (xp, yp) | xp <- [truncate x - nrColumns2..truncate x + nrColumns2]]
         | yp <- [truncate y - nrRows2..truncate y + nrRows2] ]
       -- FIXME not handling boundary conditions, such as +/-180 longitude!

-- Get the number of pixels high a grid will be for a given Frame.
getFrameHeight :: Frame -> Int
getFrameHeight f = 256 * length (selectTilesForFrame f)

tileCoordsForFrame :: Frame -> TileCoords
tileCoordsForFrame frm =
  let (xs,ys) = unzip . map unTID . concat . selectTilesForFrame $ frm
  in TileCoords
     { maxX = maximum xs
     , minX = minimum xs
     , maxY = maximum ys
     , minY = minimum ys
     }

-- |Gives the position of the coordinate in the frame with the origin as
-- the lower left (note this is different from the lower level operations!)
point2pixel :: Frame -> Point -> (Int,Int)
point2pixel frm q =
  let coords = tileCoordsForFrame frm
      (x,y') = pixelPosForCoord q coords (frameZoom frm)
  in (x, y')

pixel2point :: Frame -> (Int,Int) -> Point
pixel2point frm q =
  let coords = tileCoordsForFrame frm
  in  coordForPixelPos q coords (frameZoom frm)

-- |Computes the rectangular map region to download based on GPS points and a zoom level
determineTileCoords :: [Point] -> Zoom -> Maybe TileCoords
determineTileCoords [] _ = Nothing
determineTileCoords wpts z =
    let (xs,ys) = unzip $ map (\p -> point2tileRaw (pntLat p) (pntLon p) z) wpts
        xs' = map truncate xs
        ys' = map truncate ys
    in Just $ TileCoords
         { maxX = maximum xs'
         , minX = minimum xs'
         , maxY = maximum ys'
         , minY = minimum ys'
         }

maxNumAutoTiles = 32

-- Computes a reasonable zoom level for the given tile coordinates
-- Resulting zoom levles are always <= 16!
--
-- Basically, zooms out until there will be less than 'maxNumAutoTiles' tiles.
zoomCalc :: TileCoords -> Zoom
zoomCalc tCoords =
   let numxtiles = maxX tCoords - minX tCoords + 1
       numytiles = maxY tCoords - minY tCoords + 1
       div = getZoomDiv numxtiles numytiles 0
   in 16 - div

getZoomDiv x y i
   | (x+1)*(y+1) > maxNumAutoTiles = getZoomDiv (shiftR x 1) (shiftR y 1) (i+1)
   | otherwise = i

-- | Takes the boundaries of the OSM tiles, and generates
-- a list of the encompassed OSM tiles.
selectedTiles :: TileCoords -> [[TileID]]
selectedTiles c = map (\j -> [TID (i,j) | i <- [minX c..maxX c]]) [minY c .. maxY c]

-- | Formats the URL string
urlStr :: String -> TileID -> Zoom -> String
urlStr base (TID (xTile, yTile)) zoom = base ++"/"++show zoom++"/"++show xTile++"/"++show yTile++".png"

-- | Takes the boundaries of the OSM tiles and downloads the tiles,
-- keeping them in proper grid patterns for latter stiching or
-- side-by-side display.
downloadTiles :: String -> Zoom -> [[TileID]] -> IO [[Either Status B.ByteString]]
downloadTiles base zoom ts = runResourceT $ do
  man <- liftIO $ newManager tlsManagerSettings
  mapM (mapM (liftM (fmap snd) . downloadTile' man base zoom)) ts

-- |Download a single tile form a given OSM server URL.
downloadTile :: String -> Zoom -> TileID -> IO (Either Status B.ByteString)
downloadTile base zoom t = runResourceT $ do
  man <- liftIO $ newManager tlsManagerSettings
  liftM (fmap snd) (downloadTile' man base zoom t)

downloadTile' :: Manager -> String -> Zoom -> TileID -> ResourceT IO (Either Status (ResponseHeaders,B.ByteString))
downloadTile' man base zoom t = do
  let packIt = B.concat . L.toChunks
      url = urlStr base t zoom
  url' <- liftBase (parseUrl url)
  rsp <- httpLbs url' man
  if  (responseStatus rsp) == ok200
    then return (Right (responseHeaders rsp, packIt (responseBody rsp)))
    else return (Left $ responseStatus rsp)

-- | Takes a coordinate, the OSM tile boundaries, and a zoom level then
-- generates (x,y) points to be placed on the Image. The origin is
-- the bottom left of the picture.
pixelPosForCoord :: Integral t => Point -> TileCoords -> Zoom -> (t, t)
pixelPosForCoord wpt (TileCoords {..}) zoom =
  let lat' = pntLat wpt
      lon' = pntLon wpt
      tile@(tx,ty) = point2tileRaw lat' lon' zoom
      xoffset = (tx - fromIntegral minX) * 256
      yRange = fromIntegral $ maxY - minY
      yoffset = (ty - fromIntegral minY) * 256 - ( 1 - (ty - fromIntegral (floor ty))) * 256
  in (truncate xoffset, truncate yoffset)

coordForPixelPos :: Integral t => (t,t) -> TileCoords -> Zoom -> Point
coordForPixelPos (fromIntegral -> x,fromIntegral -> y) (TileCoords{..}) zoom =
  let lon = x * (east - west)   / 256 + west
      lat = y * (south - north) / 256 + north
      tx = x/256 + fromIntegral minX
      ty = y/256 + fromIntegral minY
  in pt lat lon Nothing Nothing

-- | The suggested copyright text in accordance with
-- <http://wiki.openstreetmap.org/wiki/Legal_FAQ>
osmCopyrightText :: String
osmCopyrightText = "Tile images © OpenStreetMap (and) contributors, CC-BY-SA"

-- | Takes the tile server base URL,
-- the set of coordinates that must appear within the map boundaries, and users
-- the 'downloadTiles' function to acquire all the necessary tiles.
--
-- The returned files should all be in an approriate grid for row/column display.
-- See the test files of Main.hs and Main2.hs for examples of Repa stiching tiles
-- into a single image or side by side display of individual tiles.
downloadBestFitTiles :: String -> [Point] -> IO [[Either Status B.ByteString]]
downloadBestFitTiles base points = do
  let (coords,zoom) = bestFitCoordinates points
      tids = selectedTiles coords
  downloadTiles base zoom tids

bestFitCoordinates :: [Point] -> (TileCoords, Zoom)
bestFitCoordinates points =
  let tiles = determineTileCoords points 16
      zoom = fmap zoomCalc tiles
      tiles' = join
             . fmap (determineTileCoords points)
             $ zoom
  in case (tiles',zoom) of
      (Just coord, Just z) -> (coord,z)
      _                    -> (TileCoords 0 0 0 0,0)


-- |The cacheing operations run in their own monad that describe the
-- location of the cache, the tile server URL, and the worker threads
-- the retrieve tiles.
data OSMConfig = OSMCfg
                { baseUrl      :: String
                , cache       :: Text                      -- ^ Path of the tile cache
                , noCacheAction :: Maybe (TileID -> Zoom -> IO (Either Status B.ByteString))
                                                           -- ^ Action to take if the tile is not cached.
                                                           --   Return 'Just' val for a default value.
                                                           --   Return 'Nothing' to wait for a tile server.
                , nrQueuedDownloads     :: Int             -- ^ Max download queue size
                , nrConcurrentDownloads :: Int             -- ^ Number of threads the tile downloading
                                                           --   can concurrently run in.  Tileserver
                                                           --   admins request this be no more than 2.
                , networkEnabled :: Bool                   -- ^ True if we should use the network to
                                                           -- download Tiles
                }

-- |The OSM operations maintain a list of tiles needing refreshed (for
-- local caching), the state of the local cache, and initial
-- configuration options.
data OSMState = OSMSt
                { neededTiles :: TBChan (TileID,Zoom)
                , dbPool      :: ConnectionPool
                , cfg         :: OSMConfig }

-- |A Monad transformer allowing you acquire OSM maps
newtype OSM a = OSM { runOSM :: StateT OSMState (LoggingT (ResourceT IO)) a }
         deriving (Monad, Applicative, Functor, MonadState OSMState)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TileEntry
    tileID TileID
    zoom   Zoom
    tileExpiration UTCTime
    tileData B.ByteString
    TileCacheID tileID zoom
|]

instance MonadIO OSM where
  liftIO = OSM . liftIO

runDB :: SqlPersistT (LoggingT (ResourceT IO)) a -> OSM a
runDB f = do
  p <- gets dbPool
  v <- liftIO (X.catch (liftM Right $ runResourceT $ runStderrLoggingT $ runSqlPool f p) hdl)
  case v of
   Left e -> runDB f
   Right x -> return x
 where
  -- hdl :: Error -> IO (Either Error a)
  hdl = return . Left . (id :: X.SomeException -> X.SomeException)

-- |evalOSM allows you to query an OSM server and the local cache.
-- Take note - the 'OSMConfig' thread limit is enforced per-evalOSM.
-- Running many evalOSM processes can result in a violation of the
-- limit and incur admin wrath.
evalOSM :: OSM a -> OSMConfig -> IO a
evalOSM m cfg = runStderrLoggingT $ withSqlitePool (cache cfg) (2 * (nrConcurrentDownloads cfg + 1))
  $ \conn -> LoggingT $ \_ -> do
  runSqlPool (runMigration migrateAll) conn
  tc <- liftIO $ newTBChanIO (nrQueuedDownloads cfg)
  liftIO $ mapM_ forkIO $ replicate (nrConcurrentDownloads cfg) (monitorTileQueue cfg tc conn)
  let s = OSMSt tc conn cfg
  runResourceT $ runStderrLoggingT $ evalStateT (runOSM m) s

-- Pulls requested tiles off the queue, downloads them, and adds them
-- to the cache.  We need to re-check the cache to make sure someone
-- hasn't already inserted it while the item was queued.  We leave the
-- possibility that it is being downloaded in parallel by another
-- 'monitorTileQueue' as acceptable duplication of work.
monitorTileQueue :: OSMConfig -> TBChan (TileID, Zoom) -> ConnectionPool -> IO ()
monitorTileQueue cfg tc p = forever (X.catch go hdl)
 where
 hdl :: X.SomeException {- Error -} -> IO ()
 hdl err = return () -- Silently ignore SQL errors (usually ErrorBusy)
 go :: IO ()
 go = do
  (t,z) <- atomically $ readTBChan tc
  b     <- runResourceT $ runStderrLoggingT $ runSqlPool (getBy (TileCacheID t z)) p
  case b of
    Nothing -> doDownload t z
    Just (Entity _ (TileEntry _ _ exp _)) -> do
      now <- liftIO getCurrentTime
      when (exp < now) (doDownload t z)
 doDownload :: TileID -> Zoom -> IO ()
 doDownload t z = do
     tileE <- downloadTileAndExprTime (baseUrl cfg) z t
     case tileE of
       Left err        -> return ()
       Right (exp,bs)  -> runResourceT $ runStderrLoggingT $ runSqlPool (insertBy (TileEntry t z exp bs) >> return ()) p

-- |A default configuration using the main OSM server as a tile server
-- and a cabal-generated directory for the cache directory
defaultOSMConfig :: IO OSMConfig
defaultOSMConfig = do
  getDataDir >>= createDirectoryIfMissing True
  cache <- getDataFileName "TileCache"
  return $ OSMCfg osmTileURL (T.pack cache) Nothing 64 2 True

buildUrl :: OSMConfig -> TileID -> Zoom -> String
buildUrl cfg t z = urlStr (baseUrl cfg) t z

-- |Like 'downloadBestFitTiles' but uses the cached copies when available.
getBestFitTiles :: [Point] -> OSM [[Either Status B.ByteString]]
getBestFitTiles cs = do
  let (coords,zoom) = bestFitCoordinates cs
      tids = selectedTiles coords
  getTiles tids zoom

-- |Like 'downloadTiles' but uses the cached copies when available
getTiles :: [[TileID]]
         -> Zoom
         -> OSM [[Either Status B.ByteString]]
getTiles ts z = mapM (mapM (\t -> getTile t z)) ts

downloadTileAndExprTime ::    String
                           -> Zoom
                           -> TileID
                           -> IO (Either Status (UTCTime,B.ByteString))
downloadTileAndExprTime base z t = do
  res <- runResourceT $ liftIO (newManager tlsManagerSettings) >>= \m -> downloadTile' m base z t
  case res of
    Right (hdrs,bs) -> do
      now <- getCurrentTime
      let maxSec = cacheLength hdrs
          delTime = addUTCTime (fromIntegral maxSec) now
      return $ Right (delTime,bs)
    Left e -> return (Left e)

-- |Like 'downloadTile' but uses a cached copy when available.
-- Downloaded copies are added to the cache.
--
-- When the cached copy is out of date it will still be returned but a
-- new copy will be downloaded and added to the cache concurrently.
getTile :: TileID -> Zoom -> OSM (Either Status B.ByteString)
getTile t zoom = do
  ch  <- gets neededTiles
  p   <- gets dbPool
  nca <- gets (noCacheAction . cfg)
  b   <- runDB $ getBy (TileCacheID t zoom)
  case b of
    Nothing -> do
      case nca of
        Nothing  -> blockingTileDownloadUpdateCache
        Just act -> do
          liftIO $ atomically $ tryWriteTBChan ch (t,zoom)
          liftIO $ act t zoom
    Just (Entity _ (TileEntry _ _ expTime x))  -> do
      now <- liftIO getCurrentTime
      let exp = expTime < now
      when exp (liftIO $ atomically (tryWriteTBChan ch (t,zoom)) >> return ())
      return (Right x)
  where
    blockingTileDownloadUpdateCache :: OSM (Either Status B.ByteString)
    blockingTileDownloadUpdateCache = do
      net  <- gets (networkEnabled . cfg)
      base <- gets (baseUrl . cfg)
      p    <- gets dbPool
      if net
       then do
        res <- liftIO $ downloadTileAndExprTime base zoom t
        case res of
           Right (delTime,bs) -> do
             runDB (insertBy (TileEntry t zoom delTime bs) >> return ())
             return (Right bs)
           Left err -> return (Left err)
       else return (Left serviceUnavailable503)

-- | Determine the lenth of time to cache an HTTP response (in seconds)
cacheLength :: ResponseHeaders -> Int
cacheLength hdrs =
  let v = lookup "Cache-Control" hdrs
      c = fmap parseSimpleQuery v
      age = join . fmap (lookup "max-age") $ c
  in fromMaybe (7 * 24 * 60 * 60) (fmap (read . filter isDigit . ('0' :) . BC.unpack) $ age)
