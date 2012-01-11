{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss
import Graphics.Gloss.DevIL
import System.IO.Temp
import System.Environment
import qualified Data.ByteString as B
import Network.OSM
import Data.GPS

import System.IO
import Data.Array.Repa.IO.DevIL
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM3, Z(..), (:.)(..), extent, reshape)
import Data.Word
import qualified Data.List

main = do
  args    <- getArgs
  case args of
    [latS,lonS] -> 
      let lat = realToFrac (read latS :: Double)
          lon = realToFrac (read lonS :: Double)
      in run [ptType lat lon Nothing Nothing]
    _ -> putStr
           $  unlines [ "usage: bitmap <file.png>"
                      , "  file.png should be a PNG file (32-bit RGBA)"]

run pts
 = do files <- downloadBestFitTiles osmTileURL pts
      arrs <- mapM (mapM stupidConversion) files
      let pics = map (map (repaToPicture True)) arrs
          (x,y) = ...  pics
          pic = gridToPicture x y (map (map third) pics)
      display (FullScreen (1280,1024)) white pic
 where
 third (_,_,x) = x

gridToPicture :: Int -> Int -> [[Picture]] -> Picture
gridToPicture x y arrs =
  let (Z :. x :. y :. _) = extent (head $ head arrs)
      rows = map (\(r,a) -> Translate 0 (r*fromIntegral y) (adjustColumns a)) (zip [1..] arrs)
      yF = fromIntegral y
      adjustColumns :: [Picture] -> Picture
      adjustColumns = Pictures
                    . map (\(c,a) -> Translate (c*yF) 0.0 a)
                    . zip [1..]
  in Pictures rows

-- We don't have a library to read in-memory buffers of PNG (colormap!) images.
stupidConversion :: B.ByteString -> IO (Array DIM3 Word8)
stupidConversion bs = do
  withSystemTempDirectory "testTiles" $ \path -> do
  hdl <- openFile (path ++ "/blah") WriteMode
  B.hPutStr hdl bs
  hClose hdl
  !pic <- readRepaImage (path ++ "/blah")
  return pic
