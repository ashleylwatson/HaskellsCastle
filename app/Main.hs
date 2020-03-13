module Main where

import CharacUtil
import SelectSystem
import Rooms
import Player
import GameRules

import Control.Monad.State.Lazy
import Control.Monad.Trans.Identity

import Data.Function (fix)
import Data.Functor (void)

import System.IO


main :: IO ()
main = do
  select <- selSySimple 3 (map mkSelOpt ["New Game"
                                        ,"Continue"
                                        ,"Controls"
                                        ,"Quit Game"])
  case select of
       1 -> void $ runStateT play $
              Game . P (C "Haskell" (28,28) 0 []) <*> head $ gameRooms
       2 -> do saveData <- getSaveData
               flip fix 1 $ \load select -> do
                saveSelect <- selectSave saveData
                case saveSelect of
                     -1 -> load select
                     0 -> main
                     _ -> case saveData !!! saveSelect of
                               Nothing -> load select
                               (Just save) -> void $ runStateT play save
       3 -> do let spacing s = sp4 ++ s ++ ln
               runIdentityT $
                 notice (spacing "Enter  -> Select Option" ++
                         spacing "wasd   -> Move Selection Star" ++
                         spacing "1      -> Go To Menu Or Go Back" ++
                         spacing "2      -> Save While In Menu")
               main

       _ -> return ()
