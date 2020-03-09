{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Battle (
  Fighter(..)
 ,Battle(..)
 ,battle -- Fighter -> Fighter
) where

import SelectSystem
import CharacUtil

import Control.Concurrent
import Control.Monad.State.Lazy
import Control.Monad.Trans

import Data.Function (on)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Tuple

import System.IO
import System.Random

import Control.Lens








data Fighter = F { _ftch :: Character
                 , _heldU :: Maybe Utility
                 , _wait :: Int
                 , _effBonus :: Int
                 , _defBonus :: Int
                 , _waitBonus :: Int} -- Passive bonuses

makeLenses ''Fighter

instance Mortal Fighter where
  name = ftch . characterName
  hp = ftch . characterHp
  xp = ftch . characterXp -- we dont actually need this
  belt = ftch . characterBelt

bonuses fighter = (view effBonus fighter
                  ,view defBonus fighter
                  ,view waitBonus fighter)










data Battle = Battle {_player :: Fighter
                     ,_enemy :: Fighter
                     ,_timePassed :: Int
                     ,_select :: Int
                     ,_gen :: StdGen}
makeLenses ''Battle


battle :: StateT Battle IO Int -- returns player's health at the end of battle
battle = do
  battleState <- get
  let view2 attribute fighter = battleState ^. fighter . attribute
  -- if the player loses
  if fst (view2 hp player) == 0 then return 0
  -- if the player wins
  else if fst (view2 hp enemy) == 0 then return $ fst $ view2 hp player
  -- if both fighters are still waiting, decrease both fighters wait by 1
  else if view2 wait player /= 0 && view2 wait enemy /= 0
  then do lift $ threadDelay 100000 -- wait 1 tenth of a real second
          printBattle
          let click fighter = over fighter $ over wait $ subtract 1
          modify $ click player
                 . click enemy
                 . over timePassed (+1)
          battle
  -- use utility or select then equip utility
  else do
  newSelect <- printBattle -- select might not change
  if view2 wait player == 0 -- player goes before the enemy
  then case view2 heldU player of
    (Just _) -> do player -|==> enemy -- player uses their utility
                   battle
    _ -> do modify $ over player (equip newSelect) -- equip new utility
                   . (blaze =<< view timePassed) -- update Burning Agony's eff
                   . set select newSelect -- update select
            battle
  else case view2 heldU enemy of
    (Just _) -> do player <==|- enemy -- enemy uses their utility
                   battle
    _ -> do let (enemyNum, newGen) = (randomR (1, length $ view2 belt enemy) $
                                     view gen battleState)  :: (Int, StdGen)
            modify $ over enemy (equip enemyNum) -- equip new utility
                   . (blaze =<< view timePassed)
                   . set gen newGen -- update Random Number
            battle




blaze :: Int -> Battle -> Battle
blaze x = over player $ over belt $ \belt ->
  case findIndex ((==) "Burning Agony" . view utilName) belt of
       (Just n) -> inject (set eff $ div x 4 + 25) n belt
       _ -> belt

equip :: Int -> Fighter -> Fighter
equip int fighter = let util = view belt fighter !!! int
  in fighter & set wait (view waitVal util + view waitBonus fighter)
             . set heldU (Just $ util)







printBattle :: StateT Battle IO Int
printBattle = do
  (Battle player enemy _ select _) <- get
  let (actual, cap) = view hp enemy
      numStars = round $ 20 * fromIntegral actual / fromIntegral cap
      stars = if numStars == 0 then "*" else replicate numStars '*'

      pre = newLns 2 ++ sp4 ++ strictSpacing 18 (view name enemy)
        ++ "|" ++ strictSpacing 20 stars ++ "|" ++ mkSp 2 ++
        (concatMap (\x -> "  " ++ view utilName x) $ view heldU enemy) ++ ln ++
        sp4 ++ maybe "" (\util -> showStats util (view wait enemy) $
        bonuses enemy) (view heldU enemy) ++ newLns 13 ++
        sp4 ++ strictSpacing 16 (view name player)

  lift $ case view heldU player of
   Just onlyPlayerUtil -> do
    putStr $ pre ++ sp4 ++ showUtilName onlyPlayerUtil ++ ln ++ showHP
               (view hp player) ++ showStats onlyPlayerUtil (view wait player)
               (bonuses player) ++ ln ++ newLns 5
    return select
   _ -> do
    let showUtil = showUtilFrom $ view belt player
        showScreen int utils = pre ++ utils ++ mkSp 20 ++ showUtilStats
         (view belt player !!! int) (beltBonuses $ view belt player) ++ newLns 3
    flip fix select $
      \recursion select -> do
      newSelect <- selSyUtility showScreen select (length $ view belt player)
       [\s -> showUtil 1 s ,                           \s -> showUtil 2 s ++ ln
       ,\s -> showHP (view hp player) ++ showUtil 3 s ,\s -> showUtil 4 s ++ ln
       ,\s -> mkSp 20 ++ showUtil 5 s ,                \s -> showUtil 6 s ++ ln
       ,\s -> mkSp 20 ++ showUtil 7 s ,                \s -> showUtil 8 s ++ ln]
      if view trait (view belt player !!! newSelect) == Passive
           then recursion newSelect
            else return newSelect






-- pronounced as attacks
(-|==>) :: Lens' Battle Fighter -> Lens' Battle Fighter -> StateT Battle IO ()
attacker -|==> defender = do
  attUtil <- gets $ fromJust . view (attacker . heldU)
  defUtil <- gets $ fromJust . view (defender . heldU)
  defHP <- gets $ view (defender . hp)
  defDefBonus <- gets $ view (defender . defBonus)
  let heal h (x,y) = if x + h > y then (y, y) else (x + h, y)
      damage d (x,y) = if x - d < 0 then (0, y) else (x - d, y)
      reflDamage health = if view trait defUtil == Reflection
        then flip damage health (if defUtil ^. def <= attUtil ^. eff
        then defUtil ^. def else attUtil ^. eff) else health
  modify $ over attacker
         $ set heldU Nothing
         . over effBonus (+ if attUtil ^. trait == EffectUp then 3 else 0)
         . over waitBonus (+ if attUtil ^. trait == Haste then 1 else 0)
         . over hp (case attUtil ^. trait of
             Recovery -> heal $ attUtil ^. eff
             (Absorb x) -> reflDamage . heal (if fst defHP < x
                                              then fst defHP else x)
             RequiresHP -> reflDamage . damage 6
             _ -> reflDamage)
  modify $ over defender
         $ over wait (\defenderWT -> case attUtil ^. trait of
               (Stun x) -> defenderWT + x
               _ -> defenderWT)
         . over hp (case attUtil ^. trait of
               Piercing -> damage (attUtil ^. eff)
               _ -> damage $ (\x d -> if x - d < 0 then 0 else x - d)
                     (attUtil ^. eff) (defUtil ^. def + defDefBonus))

(<==|-) :: Lens' Battle Fighter -> Lens' Battle Fighter -> StateT Battle IO ()
defender <==|- attacker = attacker -|==> defender
