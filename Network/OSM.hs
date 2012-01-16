{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, OverloadedStrings 
    , GeneralizedNewtypeDeriving #-}
module Network.OSM
  (  -- * Basic Types
    TileID(..)
  , TileCoords(..)
  , Zoom
    -- * Types for tile cacheing
  , OSMConfig(..)
  , OSMState
  , OSM
    -- * High-level (cacheing) Operations
  , evalOSM
  , getBestFitTiles
  , getTiles
  , getTile
  , defaultConfig
    -- * Network Operations
  , downloadBestFitTiles
  , downloadTiles
  , downloadTile
  , osmTileURL
  -- * Helper Functions
  , pixelPosForCoord
  , selectTilesWithFixedDimensions
  , determineTileCoords
  , selectedTiles
  -- * Legal
  , copyrightText
  )where

import Control.Monad
import Control.Monad.Base (liftBase)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.GPS
import Data.Maybe
import Data.Word
import Network.HTTP.Conduit
import Network.HTTP.Types ( Status, statusOK, ResponseHeaders
                          , parseSimpleQuery, statusServiceUnavailable)

-- For the cacheing
import Control.Concurrent.MonadIO (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ask)
import Control.Monad.State
import Data.Acid
import Data.Char (isDigit)
import Data.Conduit
import Data.Data
import Data.SafeCopy
import Data.Time
import Data.Typeable
import Paths_osm_download
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

type Zoom = Int

-- | The official OSM tile server.
osmTileURL :: String
osmTileURL = "http://tile.openstreetmap.org"

-- |The coordinates associated with any particular GPS location
-- can be computed using 'determineTileCoords' and converted into tile ids
-- using 'selectedTiles' before final download with 'downloadTiles'.
data TileCoords = TileCoords
                     { minX :: Int
                     , maxX :: Int  
                     , minY :: Int  
                     , maxY :: Int 
                     } deriving (Eq, Ord, Show)

-- |A TileID, along with a zoom level, uniquely identifies a single
-- OSM map tile.  The standard size is 256x256 pixels for such a tile.
data TileID = TID { unTID :: (Int, Int) } deriving (Eq, Ord, Show, Data, Typeable)

-- |An on-disk cache of tiles.  Failure to use a local cache results in
-- excessive requests to the tile server.  OSM admins might ban such users.
newtype TileCache = TC (M.Map (TileID,Zoom) (UTCTime,B.ByteString))
  deriving (Data, Typeable)

updateTC :: UTCTime -> (TileID,Zoom) -> B.ByteString -> Update TileCache ()
updateTC expire tid bs = do
  TC tc <- get
  let tc' = M.insert tid (expire,bs) tc
  put (TC tc')

queryTC :: (TileID,Zoom) -> Query TileCache (Maybe (UTCTime,B.ByteString))
queryTC tid = do
  TC st <- ask
  return $ M.lookup tid st

$(deriveSafeCopy 1 'base ''TileID)
$(deriveSafeCopy 1 'base ''TileCache)
$(makeAcidic ''TileCache ['updateTC, 'queryTC])

tileNumbers :: Integral t => Double -> Double -> Zoom -> [(t, t)]
tileNumbers t g z =
  let (a,b) = tileNumbers' t g z
      bounds x = [ceiling x, floor x]
  in [(xt,yt) | xt <- bounds a, yt <- bounds b]

-- |OSM defined method of converting a coordinate and zoom level to a list of tiles
tileNumbers' :: Double -> Double -> Zoom -> (Double,Double)
tileNumbers' latitude longitude zoom = 
             let xtile = ((longitude+180) / 360) * fromInteger (shift (1::Integer) zoom)
                 tmp = log (tan (latitude*pi / 180) + secant (latitude * pi / 180))
                 ytile = ((1-tmp / pi) / 2.0) * fromInteger (shift (1::Integer) zoom)
             in (xtile,ytile)

secant :: Floating a => a -> a
secant a = 1 / cos a

-- |Given a width, height and center, compute the tiles needed to fill the display
selectTilesWithFixedDimensions :: (Coordinate a) => (Int,Int) -> a -> Zoom -> [[TileID]]
selectTilesWithFixedDimensions (w,h) center z =
  let (x,y) = tileNumbers' (lat center) (lon center) z
      nrColumns = ceiling $ fromIntegral w / 256
      nrRows    = ceiling $ fromIntegral h / 256
      -- FIXME hardcoding the tile server tile pixel width
  in take nrRows $ map (take nrColumns) $ 
       [ [ TID (truncate xp, truncate yp) | xp <- [x..]] 
         | yp <- [y..] ] 
       -- FIXME not handling boundary conditions, such as +/-180 longitude!

-- |Computes the rectangular map region to download based on GPS points and a zoom level
determineTileCoords :: (Coordinate a) => [a] -> Zoom -> Maybe TileCoords
determineTileCoords [] _ = Nothing
determineTileCoords wpts z =
    let (xs,ys) = unzip $ concatMap (\w -> tileNumbers (lat w) (lon w) z) wpts
    in Just $ TileCoords
         { maxX = maximum xs
         , minX = minimum xs
         , maxY = maximum ys
         , minY = minimum ys
         }

maxNumAutoTiles = 32

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
urlStr :: String -> Int -> Int -> Zoom -> String
urlStr base xTile yTile zoom = base ++"/"++show zoom++"/"++show xTile++"/"++show yTile++".png"

-- | Takes the boundaries of the OSM tiles and downloads the tiles,
-- keeping them in proper grid patterns for latter stiching or
-- side-by-side display.
downloadTiles :: String -> Zoom -> [[TileID]] -> IO [[Either Status B.ByteString]]
downloadTiles base zoom ts = runResourceT $ do
  man <- newManager
  mapM (mapM (liftM (fmap snd) . downloadTile' man base zoom)) ts
  
-- |Download a single tile form a given OSM server URL.
downloadTile :: String -> Zoom -> TileID -> IO (Either Status B.ByteString)
downloadTile base zoom t = runResourceT $ do
  man <- newManager
  liftM (fmap snd) (downloadTile' man base zoom t)

downloadTile' :: Manager -> String -> Zoom -> TileID -> ResourceT IO (Either Status (ResponseHeaders,B.ByteString))
downloadTile' man base zoom t@(TID (x, y)) = do
  let packIt = B.concat . L.toChunks
  url' <- liftBase (parseUrl (urlStr base x y zoom))
  rsp <- httpLbs url' man
  if statusCode rsp == statusOK
    then return (Right (responseHeaders rsp, packIt (responseBody rsp)))
    else return (Left $ statusCode rsp)

projectMercToLat :: Floating a => a -> a
projectMercToLat rely = (180 / pi) * atan (sinh rely)

-- | Used by @pixelPosForCoord@ for N,S,E,W coordinates for (x,y) values
project :: Double -> Double -> Zoom -> (Double,Double,Double,Double)
project x y zoom = 
  let unit = 1.0 / (2.0 ** fromIntegral zoom)
      rely1 = y * unit
      rely2 = rely1 + unit
      limity = pi
      rangey = 2.0 * limity
      rely1' = limity - rangey * rely1
      rely2' = limity - rangey * rely2
      lat1 = projectMercToLat rely1'
      lat2 = projectMercToLat rely2'
      unit' = 360.0 / (2.0 ** fromIntegral zoom)
      long1 = (-180.0) + x * unit'
  in (lat2,long1,lat1,long1+unit') -- S,W,N,E
  
-- | Takes a coordinate, the OSM tile boundaries, and a zoom level then
-- generates (x,y) points to be placed on the Image.
pixelPosForCoord :: (Coordinate a, Integral t) => a -> TileCoords -> Zoom -> (t, t)
pixelPosForCoord wpt tCoord zoom =
             let lat' = lat wpt
                 lon' = lon wpt
                 tile = tileNumbers' lat' lon' zoom
                 xoffset = (fst tile - fromIntegral (minX tCoord)) * 256
                 yoffset = (snd tile - fromIntegral (minY tCoord)) * 256
                 (south,west,north,east) = (uncurry project tile zoom)
                 x = round $ (lon' - west) * 256.0 / (east - west) + xoffset
                 y = round $ (lat' - north) * 256.0 / (south - north) + yoffset
             in (x,y)

-- | The suggested copyright text in accordance with
-- <http://wiki.openstreetmap.org/wiki/Legal_FAQ>
copyrightText :: String
copyrightText = "Tile images © OpenStreetMap (and) contributors, CC-BY-SA"

-- | Takes the tile server base URL,
-- the set of coordinates that must appear within the map boundaries, and users
-- the 'downloadTiles' function to acquire all the necessary tiles.
--
-- The returned files should all be in an approriate grid for row/column display.
-- See the test files of Main.hs and Main2.hs for examples of Repa stiching tiles
-- into a single image or side by side display of individual tiles.
downloadBestFitTiles :: (Coordinate a) => String -> [a] -> IO [[Either Status B.ByteString]]
downloadBestFitTiles base points = do
  let (coords,zoom) = bestFitCoordinates points
      tids = selectedTiles coords
  downloadTiles base zoom tids

bestFitCoordinates :: (Coordinate a) => [a] -> (TileCoords, Zoom)
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
                { buildUrl    :: TileID -> Zoom -> String  -- ^ The download URL for a given tile
                , cache       :: FilePath                  -- ^ Path of the tile cache
                , noCacheAction :: Maybe (TileID -> Zoom -> IO B.ByteString)
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
                { acid        :: AcidState TileCache
                , neededTiles :: TBChan (TileID,Zoom)
                , cfg         :: OSMConfig }

-- |A Monad transformer allowing you acquire OSM maps
newtype OSM m a = OSM { runOSM :: StateT OSMState m a }
         deriving (Monad, MonadState OSMState, MonadTrans)

instance (MonadIO m) => MonadIO (OSM m) where
  liftIO = lift . liftIO

-- |evalOSM allows you to query an OSM server and the local cache.
-- Take note - the 'OSMConfig' thread limit is enforced per-evalOSM.
-- Running many evalOSM processes can result in a violation of the
-- limit and incur admin wrath.
evalOSM :: MonadIO m => OSMConfig -> OSM m a -> m a
evalOSM cfg m = do
  tc <- liftIO $ newTBChanIO (nrQueuedDownloads cfg)
  acid <- liftIO $ openLocalStateFrom (cache cfg) (TC M.empty)
  liftIO $ mapM_ forkIO $ replicate (nrConcurrentDownloads cfg) (monitorTileQueue cfg acid tc)
  let s = OSMSt acid tc cfg
  evalStateT (runOSM m) s

-- Pulls requested tiles off the queue, downloads them, and adds them to the cache.
monitorTileQueue :: OSMConfig -> AcidState TileCache -> TBChan (TileID, Zoom) -> IO ()
monitorTileQueue cfg acid tc = forever $ do
  (t,z) <- atomically $ readTBChan tc
  let addr = buildUrl cfg t z
  tileE <- downloadTileAndExprTime addr z t
  case tileE of
    Left err -> return ()
    Right (exp,bs)  -> update acid (UpdateTC exp (t,z) bs) >> createCheckpoint acid

-- |A default configuration using the main OSM server as a tile server
-- and a cabal-generated directory for the cache directory
defaultConfig :: IO OSMConfig
defaultConfig = do
  cache <- getDataFileName "TileCache"
  return $ OSMCfg (\(TID (x,y)) z -> urlStr osmTileURL x y z) cache Nothing 1024 2 True

-- |Like 'downloadBestFitTiles' but uses the cached copies when available.
getBestFitTiles :: (Coordinate a, MonadIO m)
                     => FilePath
                     -> String 
                     -> [a] -> OSM m [[Either Status B.ByteString]]
getBestFitTiles f base cs = do
  let (coords,zoom) = bestFitCoordinates cs
      tids = selectedTiles coords
  getTiles f base tids zoom

-- |Like 'downloadTiles' but uses the cached copies when available
getTiles :: MonadIO m => FilePath 
                      -> String 
                      -> [[TileID]] 
                      -> Zoom 
                      -> OSM m [[Either Status B.ByteString]]
getTiles f s ts z = mapM (mapM (\t -> getTile f s t z)) ts

downloadTileAndExprTime ::    String 
                           -> Zoom 
                           -> TileID 
                           -> IO (Either Status (UTCTime,B.ByteString))
downloadTileAndExprTime base z t = do
  res <- runResourceT $ newManager >>= \m -> downloadTile' m base z t
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
getTile :: MonadIO m => FilePath -> String -> TileID -> Zoom -> OSM m (Either Status B.ByteString)
getTile fp base t zoom = do
  st  <- gets acid
  ch  <- gets neededTiles
  nca <- gets (noCacheAction . cfg)
  b <- liftIO $ query st (QueryTC (t,zoom))
  case b of
    Nothing -> do
      case nca of
        Nothing  -> blockingTileDownloadUpdateCache
        Just act -> liftIO $ do
          atomically $ writeTBChan ch (t,zoom)
          liftM Right (act t zoom)
    Just (expTime,x)  -> do
      liftIO $ do
         now <- getCurrentTime
         let exp = expTime < now
         when exp (atomically (tryWriteTBChan ch (t,zoom)) >> return ())
      return (Right x)
  where
    blockingTileDownloadUpdateCache = do
      st  <- gets acid
      net <- gets (networkEnabled . cfg)
      if net 
       then do
        res <- liftIO $ downloadTileAndExprTime base zoom t
        case res of
           Right (delTime,bs) -> do
             liftIO $ do
               update st (UpdateTC delTime (t,zoom) bs)
               createCheckpoint st
             return (Right bs)
           Left err -> return (Left err)
       else return (Left statusServiceUnavailable)

-- | Determine the lenth of time to cache an HTTP response (in seconds)
cacheLength :: ResponseHeaders -> Int
cacheLength hdrs =
  let v = lookup "Cache-Control" hdrs
      c = fmap parseSimpleQuery v
      age = join . fmap (lookup "max-age") $ c
  in fromMaybe (7 * 24 * 60 * 60) (fmap (read . filter isDigit . ('0' :) . BC.unpack) $ age)