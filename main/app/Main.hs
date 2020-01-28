{-# OPTIONS -Wall #-}

module Main where
import Data.Map as Map hiding (map, drop)
import System.Environment
import qualified MineSweeper as MS

readAction :: MS.Board -> IO MS.Action
readAction b = do
    putStrLn "What's your next move?"
    strAction <- getLine
    checkValidAction (words strAction) b

checkValidAction :: [String] -> MS.Board -> IO MS.Action
checkValidAction [] b = tryAgain b
checkValidAction [_] b = tryAgain b
checkValidAction [_,_] b = tryAgain b
checkValidAction (a:x:y:xs) (MS.Board w h mines myMap) =
    if MS.isValidAction w h a x y
        then if (length xs) > 0
            then tryAgain (MS.Board w h mines myMap)
            else return (setAction a x y)
        else tryAgain (MS.Board w h mines myMap)

tryAgain :: MS.Board -> IO MS.Action
tryAgain myBoard = do
    putStrLn "invalid input, please try again:"
    readAction myBoard

setAction :: String -> String -> String -> MS.Action
setAction action x y =
    if (action == "Flag" || action == "flag")
        then MS.Flag (read x::Int) (read y::Int)
        else MS.Dig (read x::Int) (read y::Int)

gameStep :: MS.Board -> MS.GameState -> IO ()
-- calls the readAction and act to calculate the next state of the game. if the game is over, will return the final state of the game, otherwise calls itself
gameStep (MS.Board w h mines myMap) myGamestate = do
    putStrLn (MS.showGame (MS.Board w h mines myMap) myGamestate)
    action <- readAction (MS.Board w h mines myMap)
    if MS.isGameOn (MS.getNonMines (Map.toList myMap)) (MS.act (MS.Board w h mines myMap) myGamestate action)
        then gameStep (MS.Board w h mines myMap) (MS.act (MS.Board w h mines myMap) myGamestate action)
        else putStrLn (MS.gameOver (MS.Board w h mines myMap) (MS.act (MS.Board w h mines myMap) myGamestate action))
    
main :: IO ()
main = do
    args <- getArgs
    startGame args

startGame :: [String] -> IO ()
startGame [] = putStrLn "no arguments entered! exiting.."
startGame [_] = putStrLn "not enough command line arguments! exiting.."
startGame [_,_] = putStrLn "not enough command line arguments! exiting.."
startGame (mw:mh:mm:xs) = do
    if MS.validateArgs mw mh mm
         then if (length xs) > 0
             then putStrLn "too many command line arguments! exiting.."
             else let myBoard = MS.newGame w h m
                      myGameState = MS.newGameState myBoard
                  in  gameStep myBoard myGameState
         else putStrLn "invalid command line arguments! exiting.."

        where w = read mw::Int
              h = read mh::Int
              m = read mm::Int
