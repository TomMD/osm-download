{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, FlexibleInstances #-}
module Network.OSM
  (  -- * Types
    TileID
  , TileCoords(..)
  , Zoom
    -- * High-level (cacheing) Operations
  , getBestFitTiles
  , getTiles
  , getTile
    -- * Network Operations
  , downloadBestFitTiles
  , osmTileURL
  , pixelPosForCoord
  -- * Low level and helper functions
  , determineTileCoords
  , selectedTiles
  , downloadTiles
  , downloadTile
  -- * Legal
  , copyrightText
  )where

import Data.GPS
import Network.HTTP.Conduit
import Network.HTTP.Types (Status,statusOK)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Control.Monad.Base (liftBase)
import Data.Bits
import Data.Maybe
import Data.Word

-- For the cacheing
import Data.Data
import Data.Typeable
import Data.Acid
import Data.SafeCopy
import qualified Data.Map as M
import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (ask)
import Data.Conduit

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
                     } 

data TileID = TID { unTID :: (Int, Int) } deriving (Eq, Ord, Show, Data, Typeable)

newtype TileCache = TC (M.Map (TileID,Zoom) B.ByteString)
  deriving (Data, Typeable)
           
updateTC :: ((TileID,Zoom),B.ByteString) -> Update TileCache ()
updateTC (tid,bs) = do
  TC tc <- get
  let tc' = M.insert tid bs tc
  put (TC tc')
  
queryTC :: (TileID,Zoom) -> Query TileCache (Maybe B.ByteString)
queryTC tid = do
  TC st <- ask
  case M.lookup tid st of
    Nothing -> return Nothing
    Just t  -> return (Just t)

$(deriveSafeCopy 1 'base ''TileID)
$(deriveSafeCopy 1 'base ''TileCache)
$(makeAcidic ''TileCache ['updateTC, 'queryTC])
                                            
-- OSM defined a of converting a coordinate and zoom level to a list of tiles
tileNumbers :: Double -> Double -> Zoom -> [(Int,Int)]
tileNumbers latitude longitude zoom = 
             let xtile = ((longitude+180) / 360) * fromInteger (shift (1::Integer) zoom)
                 tmp = log (tan (latitude*pi / 180) + secant (latitude * pi / 180))
                 ytile = ((1-tmp / pi) / 2.0) * fromInteger (shift (1::Integer) zoom)
                 bounds x = [ceiling x, floor x]
             in [(xt,yt) | xt <- bounds xtile, yt <- bounds ytile]

secant :: Floating a => a -> a
secant a = 1 / cos a

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
  mapM (mapM (downloadTile' man base zoom)) ts
  
downloadTile :: String -> Zoom -> TileID -> IO (Either Status B.ByteString)
downloadTile base zoom t = runResourceT $ do
  man <- newManager
  downloadTile' man base zoom t

downloadTile' :: Manager -> String -> Zoom -> TileID -> ResourceT IO (Either Status B.ByteString)
downloadTile' man base zoom t@(TID (x, y)) = do
  let packIt = B.concat . L.toChunks
  url' <- liftBase (parseUrl (urlStr base x y zoom))
  rsp <- httpLbs url' man
  if statusCode rsp == statusOK
    then return (Right $ packIt (responseBody rsp))
    else return (Left $ statusCode rsp)

projectMercToLat :: Floating a => a -> a
projectMercToLat rely = (180 / pi) * atan (sinh rely)

-- | Used by @pixelPosForCoord@ for N,S,E,W coordinates for (x,y) values
project :: Int -> Int -> Zoom -> (Double,Double,Double,Double)
project x y zoom = 
  let unit = 1.0 / (2.0 ** fromIntegral zoom)
      rely1 = fromIntegral y * unit
      rely2 = rely1 + unit
      limity = pi
      rangey = 2.0 * limity
      rely1' = limity - rangey * rely1
      rely2' = limity - rangey * rely2
      lat1 = projectMercToLat rely1'
      lat2 = projectMercToLat rely2'
      unit' = 360.0 / (2.0 ** fromIntegral zoom)
      long1 = (-180.0) + fromIntegral x * unit'
  in (lat2,long1,lat1,long1+unit') -- S,W,N,E
  
-- | Takes a WptType, the OSM tile boundaries, and a zoom level then
-- generates (x,y) points to be placed on the Image.
pixelPosForCoord :: (Coordinate a, Integral t) => [a] -> TileCoords -> Zoom -> (t, t)
pixelPosForCoord [] _ _ = (0,0)
pixelPosForCoord [wpt] tCoord zoom =
             let lat' = lat wpt
                 lon' = lon wpt
                 tile = maximum $ tileNumbers lat' lon' zoom
                 xoffset = (fst tile - minX tCoord) * 256
                 yoffset = (snd tile - minY tCoord) * 256
                 (south,west,north,east) = (uncurry project tile zoom)
                 x = round $ (lon' - west) * 256.0 / (east - west) + fromIntegral xoffset
                 y = round $ (lat' - north) * 256.0 / (south - north) + fromIntegral yoffset
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

getBestFitTiles :: (Coordinate a) => FilePath -> String -> [a] -> IO [[Either Status B.ByteString]]
getBestFitTiles f base cs = do
  let (coords,zoom) = bestFitCoordinates cs
      tids = selectedTiles coords
  getTiles f base tids zoom

getTiles :: FilePath -> String -> [[TileID]] -> Zoom -> IO [[Either Status B.ByteString]]
getTiles f s ts z = mapM (mapM (\t -> getTile f s t z)) ts

-- FIXME constantly opening the acid state is probably really dumb.
getTile :: FilePath -> String -> TileID -> Zoom -> IO (Either Status B.ByteString)
getTile fp base t zoom = do
  st <- openLocalStateFrom fp (TC M.empty)
  b <- query st (QueryTC (t,zoom))
  case b of
    Nothing -> do
               res <- downloadTile base zoom t
               case res of
                 Right bs -> do update st (UpdateTC ((t,zoom),bs))
                                createCheckpoint st
                                closeAcidState st
                                return (Right bs)
                 Left err -> closeAcidState st >> return (Left err)
    Just x  -> return (Right x)

