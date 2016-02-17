{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.OSM.Types where

import Database.Persist
import Database.Persist.Sqlite hiding (get)

newtype TileID  = TID { unTID :: (Int,Int) }
  deriving (Eq, Ord,  Show, Read, PersistField, PersistFieldSql)
data TileCoords = TileCoords { minX,minY,maxX,maxY :: Int }
  deriving (Eq, Ord,  Show, Read)
