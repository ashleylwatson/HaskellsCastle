{-# LANGUAGE TemplateHaskell #-}

module Rooms (
  Object(..), Location(..), RoomOption(..), Room,
  roomNum,objNum,loc,obj,
  pullSel,
  roomNames, storageLocs, gameRooms
) where

import CharacUtil
import SelectSystem (SelectOpt, mkSelOpt)

import Control.Lens



data Object =
  HP | -- HealPool
  R Int String | FR Int String String | -- Room, Fake Room
  -- Enemy: Normal, Single, Blocker, Precede
  EN String | ES String | EB String Object | EP String Object |
  -- Chest: Normal, Storage, Single Normal, Single Precede
  CN String | CS Utility | CSN String String | CSP String String Object |
  GD String | -- Golden Door
  RO Object RoomOption | -- Repl Object (entering room)
  AO Object RoomOption deriving (Show, Read) -- Add Object (on player)


-- Object Location (Room Number, Object Number)
data Location = Location {_roomNum :: Int
                         ,_objNum :: Int} deriving (Eq, Ord, Show, Read)
data RoomOption = Option {_loc :: Location
                         ,_obj :: Object} deriving (Show, Read)
makeLenses ''Location
makeLenses ''RoomOption
type Room = [RoomOption]




pullSel :: Object -> SelectOpt
pullSel object =
  let proceed object = case drop 4 $ pullSel object "" of
        ('G':'o':' ':'T':'o':' ':'T':'h':'e':' ':name) -> name -- "Go To The "
        ('T':'a':'k':'e':' ':name) -> name                     -- "Take "
        name -> name
  in case object of
  HP -> mkSelOpt "HealPool"
  (R int name) -> mkSelOpt $ "Go To The " ++ name
  (FR int roomName enemyName) -> mkSelOpt $ "Go To The " ++ roomName
  (EN name) -> mkSelOpt name
  (ES name) -> mkSelOpt name
  (EB name object) -> (\s -> sp4 ++ s ++ name ++ " Blocks " ++ proceed object)
  (EP name object) -> (\s -> sp4 ++ s ++ name ++ " Protects " ++ proceed object)
  (CN utilName) -> mkSelOpt $ "Take " ++ utilName
  (CS util) -> mkSelOpt $ "Take " ++ showUtilName util
  (CSN utilName enemyName) -> mkSelOpt $ "Take " ++ utilName
  (CSP utilName enemyName _) -> mkSelOpt $ "Take " ++ utilName
  (GD name) -> mkSelOpt "Golden Door"
  (object1 `RO` _) -> pullSel object1
  (object1 `AO` _) -> pullSel object1



roomNames = ["House"
            ,"Yard"
            ,"Entrance Of The Ruins"
            ,"Ruins Level 1"
            ,"Ruins Level 2"
            ,"Ruins Level 3"
            ,"Ruins Level 4 Chamber"
            ,"Ruins Level 4 Cave"
            ,"Ruins Gold Room"
            ,"Ruins Level 5"
            ,"Ruins Level 6 Right Sector"
            ,"Ruins Level 6 Left Sector"
            ,"Den"
            ,"Passageway"
            ,"Long Staircase"
            ,"Town"]

-- This is only used once in GameRules, but I felt it was better to place here
storageLocs :: [(String, Location)]
storageLocs = zip utilityNamesList $ do x <- [17..21]
                                        y' <- [4,5,5,6,5]
                                        y <- [1..y']
                                        [Location x y]

gameRooms :: [Room]
gameRooms =
  let assignLocations rooms = do
        (roomNum, room) <- zip [1..] rooms
        return $ do (objNum, option) <- zip [1..] room
                    return $ Option (Location roomNum objNum) option
  in assignLocations $
           -- 1 House
           [CN "Long Bow"
           ,R 2 "Yard"]
           -- 2 Yard
          :[R 1 "House"
           ,R 3 "Entrance Of The Ruins"]
           -- 3 Entrance Of The Ruins
          :[R 2 "Yard"
           ,EP "Slime" $ R 4 "Ruins Level 1"]
           -- 4 Ruins Level 1
          :[R 3 "Entrance Of The Ruins"
           ,HP
           ,CSN "Wooden Shield" "Slime"
           ,EP "Goblin" $ R 5 "Ruins Level 2"]
           -- 5 Ruins Level 2
          :[R 4 "Ruins Level 1"
           ,EP "Goblin" $ CN "Longsword"
           ,EN "Slime"
           ,EN "Orc"
           ,EB "Skeleton" $ R 6 "Ruins Level 3"]
           -- 6 Ruins Level 3
          :[R 5 "Ruins Level 2" `RO` Option (Location 5 5) (R 6 "Ruins Level 3")
           ,EN "Goblin"
           ,EB "Skeleton" $ CSP "Thunder" "Mummy" $ EN "Skeleton"
           ,EP "Golem" $ R 7 "Ruins Level 4 Chamber"]
           -- 7 Ruins Level 4 Chamber
          :[R 6 "Ruins Level 3"
           ,HP
           ,R 8 "Ruins Level 4 Cave"
           ,R 10 "Ruins Level 5"]
           -- 8 Ruins Level 4 Cave
          :[R 7 "Ruins Level 4 Chamber"
           ,EN "Mummy"
           ,EN "Zombie"
           ,CN "Broadsword"
           ,EN "Zombie"
           ,R 9 "Ruins Gold Room"]
           -- 9 Ruins Gold Room
          :[R 8 "Ruins Level 4 Cave"
           ,EP "What Is This?!" $ CN "Mythril Sword"]
           -- 10 Ruins Level 5
          :[R 7 "Ruins Level 4 Chamber"
           ,EN "Mummy"
           ,EN "Ghost"
           ,CN "Wing Guard"
           ,EN "Zombie"
           ,EN "Ghost"
           ,EN "Harpy"
           ,R 11 "Ruins Level 6 Right Sector"
           ,R 12 "Ruins Level 6 Left Sector"]
           -- 11 Ruins Level 6 Right Sector
          :[R 10 "Ruins Level 5"
           ,EN "Harpy"
           ,EN "Ghost"
           ,GD "Golden Door"
           ,R 13 "Ruins Den" `RO` Option (Location 13 1)
                                         (R 11 "Ruins Level 6 Right Sector")]
           -- 12 Ruins Level 6 Left Sector
          :[R 10 "Ruins Level 5"
           ,EN "Zombie"
           ,EN "Harpy"
           ,GD "Golden Door"
           ,R 13 "Ruins Den" `RO` Option (Location 13 1)
                                         (R 12 "Ruins Level 6 Left Sector")]
           -- 13 Den
          :[R 11 "Ruins Level 6 Right Sector"
           ,FR 14 "Passageway" "Griffon"]
           -- 14 Passageway
          :[R 13 "Den"
           ,R 15 "Long Staircase"
           ,R 22 "Town"]
           -- 15 Long Staircase
          :[R 14 "Passageway"
           ,R 16 "Storage Room"
           ,R 2 "Yard" `AO` Option (Location 2 3) (R 15 "Long Staircase")]
           -- 16 Storage Room
          :[R 15 "Long Staircase"
           ,R 17 "Floor 5 Equipment"
           ,R 18 "Floor 4 Equipment"
           ,R 19 "Floor 3 Equipment"
           ,R 20 "Floor 2 Equipment"
           ,R 21 "Floor 1 Equipment"]
           -- 17 Floor 5 Equipment
          :[R 16 "Storage Room"]
           -- 18 Floor 4 Equipment
          :[R 16 "Storage Room"]
           -- 19 Floor 3 Equipment
          :[R 16 "Storage Room"]
           -- 20 Floor 2 Equipment
          :[R 16 "Storage Room"]
           -- 21 Floor 1 Equipment
          :[R 16 "Storage Room"]
           -- 22 Town
          :[R 14 "Passageway"
           ,HP]
           -- 23
          {-:[
           ]
           -- 24
          :[
           ]-}
          :[]
