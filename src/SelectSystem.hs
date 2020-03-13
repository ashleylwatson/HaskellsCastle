module SelectSystem (
  mkSelOpt     -- String -> String -> String
 ,lines23      -- String ->           String
 ,newLns

 ,SelectOpt
 ,selSySimple  --                                     Int -> [SelectOpt] -> IO Int
 ,selSyOption  --                              Int -> Int -> [SelectOpt] -> IO Int
 ,selSyMenu    -- (Int -> String -> String) -> Int -> Int -> [SelectOpt] -> IO Int
 ,selSyUtility -- (Int -> String -> String) -> Int -> Int -> [SelectOpt] -> IO Int

 ,notice       -- MonadTrans t => String -> t IO ()
) where

import CharacUtil

import Control.Monad.Trans
import Data.Function (fix)
import Data.List (elemIndex)

import System.IO


getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x



mkSelOpt string star = sp4 ++ star ++ string ++ ln
lines23 string = newLns (24 - length (lines string)) ++ string

newLns int = replicate int '\n'






type SelectOpt = String -> String


selectionSystem extrakeys = univSelSystem 1 extrakeys (\_ -> id)

selSySimple = selectionSystem [] 1
selSyOption = selectionSystem [('1',0)]
selSyUtility = univSelSystem 2 []
selSyMenu = univSelSystem 2 [('2',-1),('1',0)]



univSelSystem :: Int -> [(Char, Int)] -> (Int -> String -> String) ->
                 Int -> Int -> [SelectOpt] -> IO Int
univSelSystem columns extrakeys fitInto select numOfOpt options =
  do let showSelect _ [] = []
         showSelect int (f:fs) = f (star int) ++ showSelect (int + 1) fs
           where star x = if x == select then "* " else "  "
         screen = lines23 $ fitInto select $ showSelect 1 options
     putStr screen
     -- only allow valid inputs
     input <- fix $ \getInput -> do
       input <- getCh
       -- pressing enter or any of the extrakeys are valid inputs
       if elem input $ '\n' : map fst extrakeys then return input
       -- a or d benig valid inputs depends in columns
       else let selType col keys = columns == col && elem input keys in
         if selType 1 "ws" || selType 2 "wsad" then return input else getInput
     case input of
       '\n' -> return select
       _ | elem input $ map fst extrakeys ->
             let (Just int) = elemIndex input $ map fst extrakeys
             in return $ snd $ extrakeys !! int
       _ -> let newSelect = changeSelect columns numOfOpt select input
                 in univSelSystem columns extrakeys fitInto
                                  newSelect numOfOpt options




changeSelect :: Int -> Int -> Int -> Char -> Int
changeSelect columns nOfOpt select direction =
  case columns of
    1 -> case direction of
              'w' -> if select == 1 then select else select - 1
              _ -> if select == nOfOpt then select else select + 1
    2 -> case direction of
              'w' -> if select == 1 || select == 2
                     then select else select - 2
              's' -> if select == nOfOpt || select == (nOfOpt - 1)
                     then select else select + 2
              'd' -> changeSelect 1 nOfOpt select 's'
              _ -> changeSelect 1 nOfOpt select 'w'





notice :: MonadTrans t => String -> t IO ()
notice s = lift $ do
  putStr $ lines23 $ s ++ newLns 2
  getCh
  return ()
