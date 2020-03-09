{-# LANGUAGE TemplateHaskell #-}

module CharacUtil (
   sp4 -- String
  ,ln -- String
  ,strictSpacing -- Int -> String -> String
  ,mkSp -- Int -> String
  ,inject -- (a -> a) -> Int -> [a] -> [a]
  ,(!!!) -- [a] -> Int -> a

  ,Trait(..)
  ,showTrait -- Trait -> String
  ,Utility(..)  -- String Int Int Int Trait [(Int,(Int, Int, Int))]
  ,utilName, eff, def, waitVal, trait, nextLevels
  ,Character(..) -- String Int Int Belt
  ,characterName, characterHp, characterXp, characterBelt

  ,showUtilName -- Util -> String
  ,Belt -- [Utility]
  ,beltBonuses -- Belt -> (Int, Int, Int)
  ,showUtilFrom -- Belt -> Int -> String -> String
  ,showStats -- Utility -> Int -> (Int, Int, Int) -> String
  ,showUtilStats -- Utility -> (Int, Int, Int) -> String

  ,showHP -- (Int, Int)
  ,Mortal(..)-- name, hp, xp, belt

  ,listMonsters -- [Character]
  ,utilityNamesList -- [[String]]
  ,listUtilities --[[Utility]]
) where

import Control.Lens
import Data.Maybe


--------------------------------------------------------------------------------
-- VeryTrivialFunctions

sp4 = "    "
ln = ['\n']
strictSpacing int string = string ++ mkSp (int - length string)

mkSp :: Int -> String
mkSp int = replicate int ' '

-- apply this function on this part of this list (starting at 1)
inject :: (a -> a) -> Int -> [a] -> [a]
inject _ _ [] = []
inject f 1 (x:xs) = f x : xs
inject f int (x:xs) = x : inject f (int - 1) xs

-- starting at 1
(!!!) :: [a] -> Int -> a
x !!! y = x !! (y - 1)

--------------------------------------------------------------------------------


data Trait = TL | Defense | Piercing | Passive | Stun Int | -- TL == Traitless
             Recovery | EffectUp | Absorb Int | Haste |
             RequiresHP | Reflection deriving (Eq, Show, Read)

showTrait x = case x of TL       -> ""
                        EffectUp -> "Effect Up"
                        _        -> show x


type Belt = [Utility]
data Utility = U {_utilName :: String
                 ,_eff :: Int
                 ,_def :: Int
                 ,_waitVal :: Int
                 ,_trait :: Trait
                 -- Pair of xpCost (effChange, defChange, waitChange)
                 ,_nextLevels :: [(Int,(Int, Int, Int))]}
                 deriving (Eq, Show, Read)
makeLenses ''Utility


data Character = C {_characterName :: String
                   ,_characterHp :: (Int, Int) -- (actual hp, max hp)
                   ,_characterXp :: Int
                   ,_characterBelt :: Belt}
                   deriving (Eq, Show, Read)
makeLenses ''Character


-- show the name of a utility
showUtilName util = let name = view utilName util in
  name ++ case (length $ view nextLevels util, name) of
  (0, "Magic Robe") -> "SS"
  (1,"Magic Robe" ) -> []
  (2,"Power Bangle") -> []
  (0,_) | elem name ["Speed Shoes","Demon Bow","Metronome","Burning Agony"]-> []
  (0,_) -> "SP"
  (10,_) -> []
  (int,_) -> ("+" ++) . show . subtract int $ case name of "Power Bangle" -> 2
                                                           _              -> 10


beltBonuses :: Belt -> (Int,Int,Int)
beltBonuses = foldr (addTriple . mkBonuses) (0, 0, 0)
  where mkBonuses (U _ e d w Passive _) = (e, d, w)
        mkBonuses _ = (0, 0, 0)
        addTriple (e1, d1, w1) (e2, d2, w2) = (e1 + e2, d1 + d2, w1 + w2)


showUtilFrom belt int star = strictSpacing 20 $ mkSp 2 ++ star ++ name
  where name = if int > length belt then "" else showUtilName $ belt !!! int


showStats util waitTime (effB, defB, waitB) =
  case view trait util of
       Passive -> ""
       _ -> "Eff: "  ++ strictSpacing 3 (show $ view eff util + effB)  ++
          "  Def: "  ++ strictSpacing 3 (show $ view def util + defB)  ++
          "  Wait: " ++ strictSpacing 3 (show $ abs  waitTime + waitB) ++
             showTrait (view trait util)


showUtilStats :: Utility -> (Int,Int,Int) -> String
showUtilStats = showStats <*> view waitVal




showHP (actual, cap) = "    HP " ++ strictSpacing 13 (show actual ++ " / " ++
                                                      show cap)


class Mortal m where
  name :: Lens' m String
  hp   :: Lens' m (Int,Int)
  xp   :: Lens' m Int
  belt :: Lens' m Belt


instance Mortal Character where
  name = characterName
  hp = characterHp
  xp = characterXp
  belt = characterBelt


listMonsters :: [Character]
listMonsters =
  let mkC name int = C name (int,int) in
  [mkC "Slime" 18 2 [U "Body Blow" 14 4 12 TL []]

  ,mkC "Goblin" 24 3 [U "Slam" 17 8 10 TL [],
                      U "Great Swing" 30 0 20 TL []]

  ,mkC "Orc" 15 3 [U "Slash" 26 7 25 TL []]

  ,mkC "Skeleton" 20 3 [U "Longsword" 16 8 10 TL []]

  ,mkC "Mummy" 40 5 [U "Deliberate" 0 15 3 TL [],
                     U "Swing" 28 10 22 TL []]

  ,mkC "Golem" 74 20 [U "Bulldoze" 28 15 24 TL [],
                      U "Guard" 0 50 8 Defense []]

  ,mkC "Zombie" 40 4 [U "Claw" 20 0 9 TL [],
                      U "Acid" 10 0 16 Piercing []]

  ,mkC "What Is This?!" 81 14 [U "Mythril Sword" 24 12 8 TL [],
                               U "Wing Guard" 0 25 6 Defense [],
                               U "Starlight" 20 0 20 Piercing []]

  ,mkC "Ghost" 28 4 [U "Shade" 15 0 15 Piercing [],
                     U "Menece" 11 5 7 (Stun 5) []]

  ,mkC "Harpy" 32 6 [U "Great Swing" 24 2 10 TL [],
                     U "Swing" 12 0 4 TL []]

  ,mkC "Griffon" 99 30 [U "Guard" 0 34 17 Defense [],
                        U "Wind" 20 0 12 Piercing [],
                        U "Claw" 22 9 7 TL []]
  ]


utilityNamesList = map (view utilName) listUtilities :: [String]
listUtilities :: [Utility]
listUtilities = [U "Long Bow" 10 2 5 TL [(1,(1,0,0)),
                                         (2,(1,0,0)),
                                         (3,(1,0,0)),
                                         (5,(1,0,0)),
                                         (6,(1,0,0)),
                                         (9,(1,0,0)),
                                         (12,(1,0,0)),
                                         (15,(1,0,0)),
                                         (18,(1,0,0)),
                                         (99,(2,0,0))]
                ,U "Wooden Shield" 0 18 8 Defense [(2,(0,2,0)),
                                                   (4,(0,2,0)),
                                                   (7,(0,2,0)),
                                                   (10,(0,1,0)),
                                                   (13,(0,2,0)),
                                                   (16,(0,1,0)),
                                                   (20,(0,1,0)),
                                                   (25,(0,2,0)),
                                                   (30,(0,3,0)),
                                                   (99,(0,2,0))]
                ,U "Longsword" 15 7 10 TL [(3,(1,1,0)),
                                           (6,(2,1,0)),
                                           (11,(1,1,0)),
                                           (14,(1,2,0)),
                                           (17,(1,1,0)),
                                           (21,(2,2,0)),
                                           (27,(2,1,0)),
                                           (35,(0,1,0)),
                                           (45,(1,2,0)),
                                           (99,(34,9,0))]
                ,U "Thunder" 35 0 20 Piercing [(5,(0,0,-1)),
                                               (7,(0,0,-1)),
                                               (10,(0,0,-1)),
                                               (14,(0,0,-1)),
                                               (19,(0,0,-1)),
                                               (25,(0,0,-1)),
                                               (32,(0,0,-1)),
                                               (40,(0,0,-1)),
                                               (55,(0,0,-1)),
                                               (99,(0,0,-1))]


                ,U "Broadsword" 22 16 14 TL [(2,(1,1,0)),
                                             (4,(1,1,0)),
                                             (7,(2,1,0)),
                                             (10,(2,1,0)),
                                             (13,(2,1,0)),
                                             (16,(2,2,0)),
                                             (20,(2,2,0)),
                                             (25,(3,1,0)),
                                             (30,(3,2,0)),
                                             (99,(2,2,0))]
                ,U "Mythril Sword" 19 11 8 TL [(5,(1,1,0)),
                                               (7,(2,1,0)),
                                               (10,(2,1,0)),
                                               (14,(1,1,0)),
                                               (19,(2,0,0)),
                                               (25,(1,1,0)),
                                               (32,(1,1,0)),
                                               (40,(1,1,0)),
                                               (55,(2,2,0)),
                                               (99,(3,2,0))]
                ,U "Wing Guard" 0 16 6 Defense [(2,(0,1,0)),
                                                (4,(0,2,0)),
                                                (7,(0,2,0)),
                                                (10,(0,2,0)),
                                                (13,(0,1,0)),
                                                (16,(0,2,0)),
                                                (20,(0,2,0)),
                                                (25,(0,2,0)),
                                                (30,(0,3,0)),
                                                (99,(0,1,-1))]
                ,U "Trusty Dagger" 2 0 6 Piercing [(6,(1,0,0)),
                                                   (9,(1,0,0)),
                                                   (14,(1,0,0)),
                                                   (19,(1,0,0)),
                                                   (25,(1,0,0)),
                                                   (37,(1,0,0)),
                                                   (50,(1,0,0)),
                                                   (60,(1,0,0)),
                                                   (70,(1,0,0)),
                                                   (99,(1,0,0))]
                ,U "Power Bangle" 0 0 0 Passive [(99,(1,0,0)),
                                                 (99,(2,0,0))]


                ,U "Crossbow" 15 0 6 TL [(3,(2,0,0)),
                                         (4,(1,0,0)),
                                         (6,(1,0,0)),
                                         (8,(1,0,0)),
                                         (13,(1,0,0)),
                                         (17,(1,0,0)),
                                         (22,(1,0,0)),
                                         (26,(1,0,0)),
                                         (31,(1,0,0)),
                                         (99,(1,0,0))]
                ,U "Heal" 24 0 15 Recovery [(1,(2,0,0)),
                                            (2,(1,0,0)),
                                            (3,(2,0,0)),
                                            (5,(3,0,0)),
                                            (6,(3,0,0)),
                                            (9,(6,0,0)),
                                            (12,(6,0,0)),
                                            (15,(6,0,0)),
                                            (18,(7,0,0)),
                                            (99,(10,0,0))]
                ,U "Invigorate" 0 0 20 EffectUp [(6,(0,0,-1)), -- increases eff by 3
                                                 (9,(0,0,-1)),
                                                 (14,(0,0,-1)),
                                                 (19,(0,0,-1)),
                                                 (25,(0,0,-1)),
                                                 (37,(0,0,-1)),
                                                 (50,(0,0,-1)),
                                                 (60,(0,0,-1)),
                                                 (70,(0,0,-1)),
                                                 (99,(0,0,-1))]
                ,U "Magic Robe" 0 2 0 Passive [(99,(0,4,0))] -- increases Defense by 2 -> 6
                ,U "Speed Shoes" 0 0 (-1) Passive [] -- drops wait by 1

                ,U "Flamberge" 40 24 15 TL [(6,(2,1,0)),
                                            (9,(2,1,0)),
                                            (14,(2,1,0)),
                                            (19,(2,1,0)),
                                            (25,(2,1,0)),
                                            (37,(2,1,0)),
                                            (50,(2,0,0)),
                                            (60,(2,1,0)),
                                            (70,(2,0,0)),
                                            (99,(4,1,0))]
                ,U "Large Bowgun" 28 0 8 TL [(6,(1,0,0)),
                                             (9,(1,0,0)),
                                             (14,(1,0,0)),
                                             (19,(1,0,0)),
                                             (25,(1,0,0)),
                                             (37,(1,0,0)),
                                             (50,(1,0,0)),
                                             (60,(1,0,0)),
                                             (70,(2,0,0)),
                                             (99,(2,0,0))]
                ,U "Kite Shield" 0 34 10 Defense [(5,(0,2,0)),
                                                  (7,(0,1,0)),
                                                  (10,(0,2,0)),
                                                  (14,(0,2,0)),
                                                  (19,(0,2,0)),
                                                  (25,(0,2,0)),
                                                  (32,(0,2,0)),
                                                  (40,(0,3,0)),
                                                  (55,(0,4,0)),
                                                  (99,(0,6,0))]
                ,U "Wrath of Zeus" 75 0 26 Piercing [(5,(0,0,-1)),
                                                     (7,(0,0,-1)),
                                                     (10,(0,0,-1)),
                                                     (14,(0,0,-1)),
                                                     (19,(0,0,-1)),
                                                     (25,(0,0,-1)),
                                                     (32,(0,0,-1)),
                                                     (40,(0,0,-1)),
                                                     (55,(0,0,-1)),
                                                     (99,(0,0,-1))]
                ,U "Quick Heal" 18 0 10 Recovery [(1,(1,0,0)),
                                                  (2,(1,0,0)),
                                                  (3,(1,0,0)),
                                                  (5,(2,0,0)),
                                                  (6,(1,0,0)),
                                                  (9,(1,0,0)),
                                                  (12,(2,0,0)),
                                                  (15,(2,0,0)),
                                                  (18,(3,0,0)),
                                                  (99,(3,0,0))]
                ,U "Quick Move" 0 0 27 Haste [(9,(0,0,0)),
                                              (13,(0,0,-1)),
                                              (21,(0,0,0)),
                                              (28,(0,0,-1)),
                                              (35,(0,0,-1)),
                                              (45,(0,0,-1)),
                                              (60,(0,0,0)),
                                              (75,(0,0,-1)),
                                              (99,(0,0,-1)),
                                              (99,(0,0,-1))]



               ,U "Demonbow" 36 2 5 RequiresHP []
               ,U "Metronome" 0 0 1 TL []
               ,U "Burning Agony" 25 10 9 TL []
               ,U "Dragon Blade" 50 22 12 TL [(9,(1,0,0)),
                                              (13,(1,1,0)),
                                              (21,(1,0,0)),
                                              (28,(2,1,0)),
                                              (35,(1,0,-1)),
                                              (45,(1,1,0)),
                                              (60,(2,0,0)),
                                              (75,(2,0,0)),
                                              (99,(2,1,0)),
                                              (99,(2,0,0))]
               ,U "Spiked Guard" 0 25 12 Reflection [(5,(0,1,0)),
                                                     (7,(0,1,0)),
                                                     (10,(0,1,0)),
                                                     (14,(0,1,0)),
                                                     (19,(0,1,0)),
                                                     (25,(0,1,0)),
                                                     (32,(0,1,0)),
                                                     (40,(0,1,0)),
                                                     (55,(0,1,0)),
                                                     (99,(0,1,0))]
               ]
