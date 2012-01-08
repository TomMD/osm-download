module Network.OSM
  (  -- * Types
    TileID
     -- * High Level Operations
  , downloadBestFitTiles
  , osmTileURL
  , pixelPosForCoord
  -- * Low level and helper functions
  , determineTileCoords
  , selectedTiles
  , downloadTiles
  -- * Legal
  , copyrightText
  )where

import Data.GPS
import Network.HTTP.Conduit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Data.Bits
import Data.Maybe
import Data.Word

-- | The official OSM tile server.
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

data TileID = TID { unTID :: (Int, Int) } deriving (Eq, Ord, Show)
       
-- OSM defined a of converting a coordinate and zoom level to a list of tiles
tileNumbers :: Double -> Double -> Int -> [(Int,Int)]
tileNumbers latitude longitude zoom = 
             let xtile = ((longitude+180) / 360) * fromInteger (shift (1::Integer) zoom)
                 tmp = log (tan (latitude*pi / 180) + secant (latitude * pi / 180))
                 ytile = ((1-tmp / pi) / 2.0) * fromInteger (shift (1::Integer) zoom)
                 bounds x = [ceiling x, floor x]
             in [(xt,yt) | xt <- bounds xtile, yt <- bounds ytile]

secant :: Floating a => a -> a
secant a = 1 / cos a

initCoords :: TileCoords
initCoords = TileCoords {minX = 100000, maxX = -100000, minY = 100000, maxY = -100000}

determineTileCoords :: (Lat a, Lon a) => [a] -> Int -> Maybe TileCoords
determineTileCoords [] _ = Nothing
determineTileCoords wpts z =
    let (xs,ys) = unzip $ concatMap (\w -> tileNumbers (realToFrac $ lat w) (realToFrac $ lon w) z) wpts
    in Just $ TileCoords
         { maxX = maximum xs
         , minX = minimum xs
         , maxY = maximum ys
         , minY = minimum ys
         }

maxNumAutoTiles = 32

zoomCalc :: TileCoords -> Int
zoomCalc tCoords = 
   let numxtiles = maxX tCoords - minX tCoords + 1
       numytiles = maxY tCoords - minY tCoords + 1
       div = getZoomDiv numxtiles numytiles 0
   in 16 - div

getZoomDiv x y i
   | (x+1)*(y+1) > maxNumAutoTiles = getZoomDiv (shiftR x 1) (shiftR y 1) (i+1)
   | otherwise = i

-- | Takes the boundaries of the OSM tiles, and generates
-- [(Int,Int)] containing a list of all OSM tiles that
-- need downloading
selectedTiles :: TileCoords -> [[TileID]]
selectedTiles c = map (\j -> [TID (i,j) | i <- [minX c..maxX c]]) [minY c .. maxY c]

-- | Formats the URL string
urlStr :: String -> Int -> Int -> Int -> String
urlStr base xTile yTile zoom = base ++"/"++show zoom++"/"++show xTile++"/"++show yTile++".png"

-- | Takes the boundaries of the OSM tiles and downloads the tiles,
-- keeping them in proper grid patterns for latter stiching or
-- side-by-side display.
downloadTiles :: String -> [[TileID]] -> Int -> IO [[B.ByteString]]
downloadTiles base ts zoom = do
  let packIt = B.concat . L.toChunks
  mapM (mapM (\(x,y) -> liftM packIt $ simpleHttp (urlStr base x y zoom))) (map (map unTID) ts)

projectMercToLat :: Floating a => a -> a
projectMercToLat rely = (180 / pi) * atan (sinh rely)

-- | Used by @pixelPosForCoord@ for N,S,E,W coordinates for (x,y) values
project :: Int -> Int -> Int -> (Double,Double,Double,Double)
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
  
-- | Takes a WptType, and the OSM tile boundaries
-- and generates (x,y) points to be placed on the 'Image'
pixelPosForCoord :: (Lon a, Lat a, Integral t) => [a] -> TileCoords -> Int -> (t, t)
pixelPosForCoord [] _ _ = (0,0)
pixelPosForCoord [wpt] tCoord zoom =
             let lat' = value $ lat wpt
                 lon' = value $ lon wpt
                 tile = maximum $ tileNumbers lat' lon' zoom
                 xoffset = (fst tile - minX tCoord) * 256
                 yoffset = (snd tile - minY tCoord) * 256
                 (south,west,north,east) = (uncurry project tile zoom)
                 x = round $ (lon' - west) * 256.0 / (east - west) + fromIntegral xoffset
                 y = round $ (lat' - north) * 256.0 / (south - north) + fromIntegral yoffset
             in (x,y)

-- | The suggested copyright text in accordance with
-- http://wiki.openstreetmap.org/wiki/Legal_FAQ
copyrightText = "Tile images © OpenStreetMap (and) contributors, CC-BY-SA"

-- | Takes the destination directory for the web content,
-- the (Trail PtType), and uses the DrawOsm functions
-- to generate an `osm.png' file showing the trail.
downloadBestFitTiles :: String -> (Lat a, Lon a) => [a] -> IO [[B.ByteString]]
downloadBestFitTiles base points = do
  let tiles = determineTileCoords points 16
      zoom = fmap zoomCalc tiles
      tiles' = join 
             . fmap (determineTileCoords points)
             $ zoom
  case tiles' of
     Nothing -> return []
     Just coord -> 
         let tids = selectedTiles coord
         in concatMapM (downloadTiles base tids) (maybeToList zoom)

concatMapM f = liftM concat . mapM f
