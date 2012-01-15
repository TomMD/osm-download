{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss
import Graphics.Gloss.DevIL
import System.IO.Temp
import System.Environment
import qualified Data.ByteString as B
import Network.OSM
import Data.GPS

import Control.Monad
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
      in run [pt lat lon Nothing Nothing]
    _ -> putStr
           $  unlines [ "usage: bitmap <file.png>"
                      , "  file.png should be a PNG file (32-bit RGBA)"]

run pts
 = do files <- liftM (map $ map $ either (error . show) id)(downloadBestFitTiles osmTileURL pts)
      arrs <- (mapM (mapM stupidConversion) files)
      let arr = joinRows arrs
          (c,r,picture) = repaToPicture True arr
      display (FullScreen (1280,1024)) white picture

joinRows :: [[Array DIM3 Word8]] -> (Array DIM3 Word8)
joinRows fs2D =
  let rows = map joinColumns fs2D
  in case rows of
      (x:xs) -> foldl trav x xs
      [] -> error "No rows!"
  where
  trav b a = R.traverse a ext lkup
    where ext (Z :. rowA :. colA :. chanA) = Z :. rowA + rowB :. colA :. chanA
          (Z :. rowB  :. _ :. _) = extent b
          (Z :. rowA' :. _ :. _) = extent a
          lkup f idx@(Z :. r :. c :. n)
             | r >= rowA' = b R.! (Z :. r - rowA' :. c :. n)
             | otherwise = a R.! idx

joinColumns :: [Array DIM3 Word8] -> Array DIM3 Word8
joinColumns tiles =
  case tiles of
    (x:xs) -> foldl trav x xs
    []     -> error "No Tiles!"
  where
  trav a b = R.traverse a ext lkup
    where ext (Z :. rowA :. colA :. chanA) = Z :. colA :. rowA + rowB :. chanA
          (Z :. rowB  :. _ :. _) = extent b
          (Z :. _ :. colA' :. _) = extent a
          lkup f idx@(Z :. r :. c :. n)
             | c >= colA' = b R.! (Z :.  r :. c - colA' :. n)
             | otherwise = a R.! idx

-- We don't have a library to read in-memory buffers of PNG (colormap!) images.
stupidConversion :: B.ByteString -> IO (Array DIM3 Word8)
stupidConversion bs = do
  withSystemTempDirectory "testTiles" $ \path -> do
  hdl <- openFile (path ++ "/blah") WriteMode
  B.hPutStr hdl bs
  hClose hdl
  !pic <- readRepaImage (path ++ "/blah")
  return pic
