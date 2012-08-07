{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss
import System.IO.Temp
import System.Environment
import qualified Data.ByteString as B
import Network.OSM
import Geo.Computations

import Control.Monad
import System.IO
import Codec.Picture.Repa
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM3, Z(..), (:.)(..), extent, reshape)
import qualified Data.Array.Repa.Repr.ByteString as RB
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
      mapM_ (\(bs,n,m) -> B.writeFile (show n ++ "-" ++ show m ++ ".png") bs)
            (concat $ zipWith (\fs n -> zipWith (\f m -> (f,n,m)) fs [0..]) files [0..])
