{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss
import System.IO.Temp
import System.Environment
import qualified Data.ByteString as B
import Network.OSM
import Data.GPS

import Control.Monad
import System.IO
import Codec.Picture.Repa
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM3, Z(..), (:.)(..), extent, reshape)
import qualified Data.Array.Repa.ByteString as RB
import Data.Word
import qualified Data.List

main = do
  args    <- getArgs
  case args of
    [latS,lonS] -> 
      let lat = realToFrac (read latS :: Double)
          lon = realToFrac (read lonS :: Double)
      in run [pt lat lon Nothing Nothing]
    _ -> putStr
           $  unlines [ "usage: bitmap <file.png>"
                      , "  file.png should be a PNG file (32-bit RGBA)"]

run pts
 = do files <- liftM (map $ map $ either (error . show) id) (evalOSM (getBestFitTiles pts) =<< defaultOSMConfig)
      let arrs = map (map $ imgData . either (error .show) id . decodeImageRGBA) files
      let arr = map (map $ (\(_,_,p) -> p) . repaToPicture True) arrs
          picture = gridToPicture arr
      display (FullScreen (1280,1024)) white picture

repaToPicture :: Bool -> Array DIM3 Word8 -> (Int, Int, Picture)
repaToPicture b arr = (col, row, bitmapOfByteString row col (toBS arr) b)
  where
  e@(Z :. col :. row :. chan) = extent arr
  order (Z :. oldCol :. oldRow :. oldChan) = Z :. oldCol :. oldRow :. oldChan
  toBS = RB.toByteString -- . backpermute e order

gridToPicture :: [[Picture]] -> Picture
gridToPicture arrs =
  let rows = map (\(r,a) -> Translate (r*(-256)) 0 (adjustColumns a)) (zip [0..] arrs)
      adjustColumns :: [Picture] -> Picture
      adjustColumns = Pictures
                    . map (\(c,a) -> Translate 0 (c*(256)) a)
                    . zip [0..]
  in Pictures rows
