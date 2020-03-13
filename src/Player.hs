{-# LANGUAGE TemplateHaskell #-}

module Player (
  Player(..), -- Character Storage [Room]
  plch,rooms,

  remOptOnRoom, -- Location -> Room -> Room
  addOptOnRoom, -- RoomOption -> Room -> Room
  replOptOnRoom, -- RoomOption -> Room -> Room
  remOpt, -- Location -> Player -> Player
  addOpt, -- RoomOption -> Player -> Player
  replOpt -- RoomOption -> Player -> Player

) where

import CharacUtil
import SelectSystem
import Battle
import Rooms

import Control.Lens
import Control.Monad

import Data.List



data Player = P {_plch :: Character
                ,_rooms :: [Room]} deriving (Show, Read)

makeLenses ''Player

instance Mortal Player where
  name = plch . characterName
  hp = plch . characterHp
  xp = plch . characterXp
  belt = plch . characterBelt

remOptOnRoom :: Location -> Room -> Room
remOptOnRoom location [] = []
remOptOnRoom location (o:os) | location == view loc o = os
                             | otherwise = o : remOptOnRoom location os
addOptOnRoom :: RoomOption -> Room -> Room
addOptOnRoom option room = sortOn (view loc) $ option : room
replOptOnRoom :: RoomOption -> Room -> Room
replOptOnRoom option = addOptOnRoom option . remOptOnRoom (option ^. loc)

remOpt :: Location -> Player -> Player
remOpt = over rooms . (inject . remOptOnRoom <*> view roomNum)
addOpt :: RoomOption -> Player -> Player
addOpt = over rooms . (inject . addOptOnRoom <*> view (loc . roomNum))
replOpt :: RoomOption -> Player -> Player
replOpt option = addOpt option . remOpt (option ^. loc)
