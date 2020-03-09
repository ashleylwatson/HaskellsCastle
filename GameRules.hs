{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module GameRules (
  Game(..)
 ,play
 ,getSaveData
 ,selectSave
) where


import CharacUtil
import SelectSystem
import Battle
import Rooms
import Player

import Control.Concurrent
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Trans

import Data.Char
import Data.List (delete, find)
import Data.Functor ((<&>))
import Data.Function (fix, on, (&))
import Data.Maybe

import System.IO
import System.Random



data Game = Game {_player :: Player
                 ,_room :: Room} deriving (Show, Read)
makeLenses ''Game

play :: StateT Game IO ()
play = do
  player <- gets $ view player
  room <- gets $ view room
  select <- lift $ selSyOption 1. length <*> map (pullSel . view obj) $ room
  case select of
    0 -> case length $ view belt player of -- go to menu
           0 -> play -- you need at least 1 utility to go to the menu
           _ -> do menu 1
                   play
    _ -> let option = room !!! select
         in case option of
              -- You need to grab the bow in the house before fighting
              (Option _ (EP _ _)) | length (view belt player) == 0 -> play
              _ -> do useOpt option
                      play







menu :: Int -> StateT Game IO ()
menu menuSelect = do
  gameState <- get
  let viewPlayer attribute = view (player . attribute) gameState
      pre playerXp = lines23 $ "    EXP: " ++ strictSpacing 3 (show playerXp)
                     ++ mkSp 39 ++ "2 -> SAVE" ++ ln ++ newLns 13 ++
                     sp4 ++ strictSpacing 16 (viewPlayer name)
      showUtil = showUtilFrom $ viewPlayer belt
      showScreen int utils = pre (viewPlayer xp) ++ utils ++ mkSp 20 ++
                             showUtilStats (viewPlayer belt !!! int)
                             (beltBonuses $ viewPlayer belt) ++ newLns 3
      beltLength = length $ viewPlayer belt
  -- Select A Utility
  utilSelect <- lift $ selSyMenu showScreen menuSelect beltLength
    [\s -> showUtil 1 s,                           \s -> showUtil 2 s ++ ln
    ,\s -> showHP (viewPlayer hp) ++ showUtil 3 s, \s -> showUtil 4 s ++ ln
    ,\s -> mkSp 20 ++ showUtil 5 s,                \s -> showUtil 6 s ++ ln
    ,\s -> mkSp 20 ++ showUtil 7 s,                \s -> showUtil 8 s ++ ln]
  if utilSelect == 0 then return () -- exit menu
  else if utilSelect == -1
  then do saveSelect <- lift $ selectSave =<< getSaveData -- go to save menu
          if saveSelect == 0 then return () -- exit menu
          else lift . writeFile ("Sv" ++ show saveSelect ++ ".txt") . show=<<get
    -- After selecting a utility, choose what to do with it
  else flip fix 1 $ \selectOption select -> do
  gameState <- get
  let viewPlayer attribute = view (player . attribute) gameState
      selectedUtil = viewPlayer belt !!! utilSelect
      showUtilPair = on (++) $ showUtilFrom (viewPlayer belt)
                           <*> \n -> if n == utilSelect then "> " else "  "
      newPre = pre (viewPlayer xp) ++ showUtilPair 1 2 ++ ln ++
               showHP (viewPlayer hp) ++ showUtilPair 3 4 ++ ln
      showlLVupCost = case view nextLevels selectedUtil of
                           (x:_) -> strictSpacing 4 (show $ fst x)
                           [] -> sp4
      utilTrait = view trait selectedUtil
      beltLength = length $ viewPlayer belt
  -- Choose LVLup, Heal, Order, or Drop
  optionSelect <- lift $ case utilTrait of
    -- If the utility is Heal, give an option to Heal
    Recovery -> selSyOption select 4 [(\s -> newPre ++
       mkSelOpt ("LVLup - " ++ showlLVupCost ++ "  " ++ showUtilPair 5 6) s)
      ,mkSelOpt $ strictSpacing 16 "Heal - 1" ++ showUtilPair 7 8
      ,mkSelOpt $ strictSpacing 16 "Order" ++ showUtilStats selectedUtil (0,0,0)
      ,mkSelOpt $ "Drop" ++ ln]
    -- Otherwise, don't give that option
    _ -> selSyOption select 3 [(\s -> newPre ++
       mkSelOpt ("LVLup - " ++ showlLVupCost ++ "  " ++ showUtilPair 5 6) s)
      ,mkSelOpt $ strictSpacing 14 "Order" ++ showUtilPair 7 8
      ,mkSelOpt $ strictSpacing 14 "Drop" ++ showUtilStats selectedUtil (0,0,0)
       ++ newLns 2]
  -- Left is for if the selected utility is Heal, Right is for anything else
  case (if utilTrait == Recovery then Left else Right) optionSelect of
    x | elem x [Left 0, Right 0] -> menu utilSelect -- go back to menu
    -- LVLup
    x | elem x [Left 1, Right 1] -> case view nextLevels selectedUtil of
      ((cost,bonus):lvls) | viewPlayer xp >= cost -> do
        let ((_,(effUp,defUp,waitUp)):restLevels) = view nextLevels selectedUtil
            levelUp = over eff (effUp +) .
                      over def (defUp +) .
                      over waitVal (waitUp +) .
                      set nextLevels restLevels
            levelUpUtil int = over belt (inject levelUp utilSelect) .
                              over xp (subtract $ (fst . head) $
                                       view nextLevels selectedUtil)
        modify $ over player $ levelUpUtil utilSelect
        selectOption optionSelect
      _ -> selectOption optionSelect
    -- Heal
    (Left 2) -> let lvlsNum = length $ view nextLevels selectedUtil
      in if lvlsNum == 10 then selectOption optionSelect
      else do let updatedName = case reverse $ showUtilName selectedUtil of
                    ('1':'+':eman) -> reverse eman
                    (int:'+':eman) -> reverse eman ++ "+" ++
                                      show (read [int] - 1)
                    ('P':'S':eman) -> reverse eman ++ "+9"
                  healLevels = view nextLevels $ listUtilities !!!
                               -- Heal and Quick Heal are 2 different utilities
                               case showUtilName selectedUtil of
                                    ('H':xs) -> 11
                                    _        -> 19
                  lowerLvl = over belt $ flip inject utilSelect $
                             set utilName updatedName .
                             over nextLevels (healLevels !!! (10 - lvlsNum) :)
                  heal (x,y) = let newHp = x + view eff selectedUtil
                               in if newHp > y then (y, y) else (newHp, y)
              modify $ over player $ over hp heal . lowerLvl
              selectOption optionSelect
    -- Order
    x | elem x [Left 3, Right 2]
      -- you need at least 2 utilities to change the order
      -> if beltLength == 1 then selectOption optionSelect else do
      let showStar n m = showUtil n $ if m == "* " then m
                           else if n == utilSelect then "> " else "  "
          showScreen int utils = pre (viewPlayer xp) ++ utils ++ mkSp 20 ++
                                 showUtilStats (viewPlayer belt !!! int)
                                 (beltBonuses $ viewPlayer belt) ++ newLns 3
      utilSelect2 <- lift $ selSyUtility showScreen utilSelect beltLength
        [\s-> showStar 1 s,                           \s-> showStar 2 s ++ ln
        ,\s-> showHP (viewPlayer hp) ++ showStar 3 s, \s-> showStar 4 s ++ ln
        ,\s-> mkSp 20 ++ showStar 5 s,                \s-> showStar 6 s ++ ln
        ,\s-> mkSp 20 ++ showStar 7 s,                \s-> showStar 8 s ++ ln]
      if utilSelect == utilSelect2 then selectOption select
      else let util1 = viewPlayer belt !!! utilSelect
               util2 = viewPlayer belt !!! utilSelect2
               replaceItem 1 y (x:xs) = y:xs
               replaceItem n y (x:xs) = x : replaceItem (n - 1) y xs
               switch n m = replaceItem n util2 . replaceItem m util1
               reorder = over belt $ switch utilSelect utilSelect2
           in do modify $ over player reorder
                 menu utilSelect2
    -- Drop
    x | elem x [Left 4, Right 3]
      -- you need at least 1 utility in your inventory
      -> if beltLength == 1 then selectOption optionSelect else do
      confirmDrop <- lift $ selSyOption select 2
        [\s -> newPre ++ sp4 ++ s ++ "Drop" ++ mkSp 12 ++ showUtilPair 5 6 ++ ln
        ,\s -> sp4 ++ s ++ "Cancel" ++ mkSp 11 ++ showUtilPair 7 8 ++ ln ++
         mkSp 20 ++ showUtilStats selectedUtil (0,0,0) ++ newLns 3]
      if elem confirmDrop [0,2] then selectOption optionSelect else do
      let util = viewPlayer belt !!! utilSelect
          mkOption = Option $ fromJust $ lookup (view utilName util) storageLocs
      modify $ over player $ over belt (delete util) .
                             addOpt (mkOption $ CS util)
      menu $ if utilSelect > beltLength - 1 then beltLength - 1
                                            else utilSelect



getSaveData :: IO [Maybe Game]
getSaveData = let pullSave fileName = withFile fileName ReadWriteMode $
                    \handle -> do saveData <- hGetContents handle
                                  evaluate $ length saveData
                                  return $ case saveData of
                                    "" -> Nothing
                                    _ -> Just (read saveData :: Game)
              in sequence $ map pullSave ["Sv1.txt", "Sv2.txt", "Sv3.txt"]


selectSave :: [Maybe Game] -> IO Int
selectSave saves = selSyOption 1 3 $ saves <&> \save -> mkSelOpt $ case save of
  (Just (Game _ room)) -> roomNames !!! (head room ^. loc . roomNum)
  _                -> "----"


nameToUtil :: String -> Utility
nameToUtil name = fromJust $ find ((==) name . view utilName) listUtilities


useOpt :: RoomOption -> StateT Game IO ()
useOpt option =
  let mkOption = flip (set obj) option
  in option ^. loc & case option ^. obj of
  -- completely heals player
  HP -> const $ modify $ over (player . hp) $ \(_,y) -> (y, y)
  -- Move to the nth room
  R n _ -> const $ modify . set room =<< gets ((!!! n) . view (player . rooms))
  FR int roomName enemyName -> const $ do
      notice $ sp4 ++ "A " ++ enemyName ++ " Has Appeared"
      useOpt $ mkOption $ EP enemyName $ R int roomName
  -- after enemy is defeated, it is removed from the active room,
  -- but will return apon reentering the room
  EN enemyName -> enemy enemyName $ return ()
  -- after enemy is defeated, it is completely removed from the game
  ES enemyName -> enemy enemyName . modify . over player =<< remOpt
  -- after enemy is defeated, it is only replaced in the active room,
  -- but will return apon reentering the room
  EB enemyName object -> enemy enemyName $
                         modify . over room $ addOptOnRoom $ mkOption object
  -- after enemy is defeated, it is completely replaced
  EP enemyName object -> enemy enemyName $
                         modify $ over player (replOpt $ mkOption object)
                                . over room (addOptOnRoom $ mkOption object)
  -- After taking the Utility from a Normal Chest, the chest disappears
  CN utilName -> chest (nameToUtil utilName) "" .
                 modify . over player =<< remOpt
  CS util -> chest util "" . modify . over player =<< remOpt
  CSN utilName enemyName -> chest (nameToUtil utilName) enemyName .
                            modify . over player =<< remOpt
  CSP utilName enemyName object -> chest (nameToUtil utilName) enemyName $
                                modify $ over player $ replOpt $ mkOption object
  GD name -> case name of "Golden Door" -> goldenDoor
                          _ -> chest (nameToUtil name) "" $
                               modify $ over player $ remOpt (Location 12 4)
                                                    . remOpt (Location 11 4)
  RO object roomOption -> const $ do
       useOpt $ mkOption object
       modify $ over room $ replOptOnRoom roomOption

  AO object roomOption -> const $ do
       let newOption = mkOption object
       modify $ over player $ addOpt roomOption
       useOpt newOption
       modify $ over player $ replOpt newOption





enemy = flip enemy' $ return ()
-- enemy' exists to have a lossUpdate and is only used in chest funtion
-- the lossUpdate in enemy' is only used for CSN and CSP objects
enemy' :: String -> StateT Game IO () -> StateT Game IO ()
       -> Location -> StateT Game IO ()
enemy' enemyName lossUpdate winUpdate loc = do
  let monster = fromJust $ find ((==) enemyName . view name) listMonsters
      chToFitr charac = let (effB, defB, waitB) = beltBonuses $ charac ^. belt
                        in F charac Nothing 0 effB defB waitB
  Game p _ <- get
  (result,_) <- lift $ runStateT battle .
    (Battle (chToFitr $ p ^. plch) (chToFitr monster) 0 1) =<< getStdGen
  if result == 0
    -- if the player loses the battle
    then do modify $ over player (over hp (\ (_,y) -> (y + 1, y + 1)))
                   . (set room =<< head . view (player . rooms))
            lossUpdate
    -- if the player wins the battle
    else do modify $ over player (over hp (set _1 result) .
                                  over xp (+ view characterXp monster))
                   . over room (remOptOnRoom loc)
            winUpdate







chest :: Utility -> String -> StateT Game IO () -> Location -> StateT Game IO ()
chest util enemyName playerUpdate loc = do
  let utilName = showUtilName util
      spacing s = sp4 ++ s ++ ln
  beltLength <- gets $ length . view (player . belt)
  case beltLength of
    8 -> notice (spacing "Your Inventory Is Full." ++
                                "You Cannot Pick Up " ++ utilName ++ ".")
    _ -> do select <- lift $
              selSySimple 2 [\ s -> spacing $ s ++ "Take " ++ utilName
                            ,\ s -> spacing $ s ++ "Leave It"]
            if select == 2 then return ()
            else case enemyName of
                   "" -> do modify $ over player (over belt (++ [util]))
                                . over room (remOptOnRoom loc)
                            playerUpdate
                   _ -> do notice $ sp4 ++ "A " ++ enemyName ++ " Has Appeared"
                           modify $ over (player . belt) (++ [util])
                           enemy' enemyName
                                  (modify $ over (player . belt) $ delete util)
                                  playerUpdate
                                  loc




goldenDoor :: Location -> StateT Game IO ()
goldenDoor loc = do
  let spacing s = sp4 ++ s ++ newLns 10
  notice $ spacing   "You must choose..."
  notice $ spacing   "Behind each golden door lies a wonderful treasure."
  notice $ spacing $ "But when one chest is opened," ++ ln ++
              sp4 ++ "the other will be locked for all eternity!"
  notice $ spacing   "Chose wisely."
  let otherUtil n = if n == 11 then "Power Bangle" else "Trusty Dagger"
      placeChest f n utilName = f $ Option (Location n 4) (GD utilName)
  modify $ over player (placeChest replOpt 11 "Power Bangle"
                      . placeChest replOpt 12 "Trusty Dagger")
         . over room (placeChest replOptOnRoom <*> otherUtil $ loc ^. roomNum)
