{-# OPTIONS -Wall #-}
-- Vered Harel
-- 205680523
-- Tal Keidan
-- 204917686
-- Raziel Sharabi
-- 204209738

module MineSweeper
    ( newGame
    , showGame
    , newGameState
    , dig
    , toggleFlag
    , act
    , isGameOn
    , gameOver
    , isValidAction
    , validateArgs
    , getNonMines
    , Action(..)
    , Board(..)
    , GameState(..)
    ) where

import Data.Map as Map hiding (take, drop)
import Data.List (sortBy)
import Data.Char
import Data.Maybe
import Safe 
import Data.Function (on)
import System.Random.Shuffle
import System.Random as Rand
data Action = Dig Int Int | Flag Int Int deriving (Read)
data Board = Board Int Int [(Int, Int)] (Map (Int, Int) Char) deriving (Show)
data GameState = GameState (Map (Int, Int) Char) [(Int, Int)] deriving (Show) 

validateArgs :: String -> String -> String -> Bool
-- get values from command line, and make sure they are validateArgs
validateArgs w h m = 
    let mayW = readMay w::Maybe Int
        mayH = readMay h::Maybe Int
        mayM = readMay m::Maybe Int
    in  if isNothing mayW || isNothing mayH || isNothing mayM
            then False
            else if areNumbersGood (fromJust mayW) (fromJust mayH) (fromJust mayM)
                then True
                else False

isValidAction :: Int -> Int -> String -> String -> String -> Bool
isValidAction w h action x y =
    let mayX = readMay x::Maybe Int
        mayY = readMay y::Maybe Int
    in  if isNothing mayX || isNothing mayY
            then False
            else if isActionGood action (fromJust mayX) (fromJust mayY) w h
                then True
                else False

isActionGood :: String -> Int -> Int -> Int -> Int -> Bool
isActionGood action x y w h
    | ((action == "Flag") || (action == "Dig")) && (x <= w) && (x > 0) && (y <= h) && (y > 0) = True
    | otherwise = False

areNumbersGood :: Int -> Int -> Int -> Bool
areNumbersGood w h m
    | m < (w*h) && w <= 20 && w >= 10 && h <= 20 && h >= 10 = True
    | otherwise = False

newGame :: Int -> Int -> Int -> Board
newGame w h m =
    let tups = cp_lc [1..w] [1..h]
        sortTups = sortBySecond tups
        myMap = initMap sortTups
    in  (generateMines m (Board w h [] myMap))

initMap :: [(Int, Int)] -> (Map (Int, Int) Char)
initMap [] = Map.empty
initMap (x:xs) = Map.insert x '0' (initMap xs)

coords :: Int -> Int -> Int -> [(Int, Int)]
coords numMines w h = 
    let xCord = [1..w]
        yCord = [1..h]
        tups = cp_lc xCord yCord
        shuffled = shuffle' tups (w*h) (mkStdGen (1455*numMines*w*h))
     in take numMines shuffled

generateMines :: Int -> Board -> Board
-- will create new board with numbers and mines
generateMines numMines (Board w h _ myMap) =
    let myMines = coords numMines w h
        newMap = adjustMines myMines myMap
    in  (fixBoard myMines (Board w h myMines newMap))

adjustMines :: [(Int, Int)] -> (Map (Int, Int) Char) -> (Map (Int, Int) Char)
adjustMines [] m = m
adjustMines (x:xs) m = adjustMines xs (Map.adjust (\_ -> '*') x m)

fixBoard :: [(Int, Int)] -> Board -> Board
fixBoard [] board = board
fixBoard (m:mines) (Board w h stMines myMap) =
    let x = fst m
        y = snd m
        newMap = adjustNeighbors [(x-1, y-1), (x+1, y+1), (x, y+1), (x+1, y-1), (x+1, y), (x-1, y+1), (x-1, y), (x,y-1)] stMines myMap
    in  fixBoard mines (Board w h stMines newMap)

adjustNeighbors :: [(Int, Int)] -> [(Int, Int)] -> (Map (Int, Int) Char) -> (Map (Int, Int) Char)
adjustNeighbors [] _ m = m
adjustNeighbors (n:neighbors) mines myMap
    | n `elem` mines = adjustNeighbors neighbors mines myMap
    | otherwise = adjustNeighbors neighbors mines (Map.adjust (\y -> chr ((ord y) + 1)) n myMap)

cp_lc :: [Int] -> [Int] -> [(Int, Int)]
cp_lc a b = [ (x,y) | x <- a, y <- b ]

showGame :: Board -> GameState -> String
showGame (Board w h mines myMap) game =
    let myCoords = sortBySecond (Map.keys myMap)
    in  (makeColumns 0 1 w) ++ "\n" ++ (makeRow 1) ++ (convertBoardToString (Board w h mines myMap) False myCoords w 1 game)

makeColumns :: Int -> Int -> Int -> String
makeColumns _ _ 0 = ""
makeColumns 0 1 w = "\n    001 " ++ (makeColumns 0 2 (w-1))
makeColumns i 9 w = "0" ++ (show i) ++ "9" ++ " " ++ (makeColumns (i+1) 0 (w-1))
makeColumns i j w = "0" ++ (show i) ++ (show j) ++ " " ++ (makeColumns i (j+1) (w-1))

makeRow :: Int -> String
makeRow row
     | row < 10 = "00" ++ (show row) ++ " "
     | row > 9 && row < 20 = "01" ++ (show (row `mod` 10)) ++ " "
     | otherwise = "020 "

convertBoardToString :: Board -> Bool -> [(Int, Int)] -> Int -> Int -> GameState -> String
convertBoardToString _ _ [] _ _ _ = ""
convertBoardToString (Board tempWidth h bMines myMap) bool cords 0 i (GameState gameMap mines) = "\n" ++ (makeRow (i+1)) ++ convertBoardToString (Board tempWidth h bMines myMap) bool cords tempWidth (i+1) (GameState gameMap mines)
convertBoardToString (Board tempWidth h bMines myMap) bool (c:cords) w i (GameState gameMap mines)
    | (Map.lookup c gameMap) == Just '!' = "[!] " ++ convertBoardToString (Board tempWidth h bMines myMap) bool cords (w-1) i (GameState gameMap mines)
    | (Map.lookup c myMap) == Just '*' && bool == True = "[*] " ++ convertBoardToString (Board tempWidth h bMines myMap) bool cords (w-1) i (GameState gameMap mines)
    | (Map.lookup c gameMap) == Just 'd' = maybeToVal (Map.lookup c myMap) ++ convertBoardToString (Board tempWidth h bMines myMap) bool cords (w-1) i (GameState gameMap mines) --change from x please
    | (Map.lookup c gameMap) == Nothing = "[ ] " ++ convertBoardToString (Board tempWidth h bMines myMap) bool cords (w-1) i (GameState gameMap mines)
    | otherwise = "error converting to string\n"

maybeToVal :: Maybe Char -> [Char]
maybeToVal Nothing = ""
maybeToVal (Just x) = "[" ++ [x] ++ "] "

sortBySecond :: [(Int, Int)] -> [(Int, Int)]
sortBySecond = sortBy (compare `on` snd)

newGameState :: Board -> GameState
newGameState (Board _ _ mines _) = GameState Map.empty mines

dig :: Board -> GameState -> Int -> Int -> GameState
dig (Board w h boardMines boardMap) (GameState gameMap mines) x y
    | (Map.lookup (x,y) boardMap) == Just '0' = (GameState (digNeighbors (Board w h boardMines boardMap) (GameState (Map.insert (x,y) 'd' gameMap) mines) (getMyNeighbors (x,y))) mines)
    | (Map.lookup (x,y) gameMap) == Just '!' = GameState gameMap mines
    | otherwise = GameState (Map.insert (x,y) 'd' gameMap) mines

getMyNeighbors :: (Int, Int) -> [(Int, Int)]
getMyNeighbors (x, y) = [(x-1, y-1), (x+1, y+1), (x, y+1), (x+1, y-1), (x+1, y), (x-1, y+1), (x-1, y), (x, y-1)]

digNeighbors :: Board -> GameState -> [(Int, Int)] -> Map (Int, Int) Char
digNeighbors _ (GameState myMap _) [] = myMap
digNeighbors (Board w h boardMines boardMap) (GameState myMap mines) (n:neighbors)
    | n `elem` mines || (Map.lookup n myMap) == Just 'd' = digNeighbors (Board w h boardMines boardMap) (GameState myMap mines) neighbors
    | (Map.lookup n boardMap) /= Just '0' = digNeighbors (Board w h boardMines boardMap) (GameState (Map.insert n 'd' myMap) mines) neighbors
    | (Map.lookup n boardMap) == Just '0' = digNeighbors (Board w h boardMines boardMap) (GameState (Map.insert n 'd' myMap) mines) ((getMyNeighbors n) ++ neighbors)
    | otherwise = digNeighbors (Board w h boardMines boardMap) (GameState myMap mines) neighbors

toggleFlag :: GameState -> Int -> Int -> GameState
-- if a flag already exists in the flag location, remove it
toggleFlag (GameState gameMap mines) x y
    | Map.lookup (x,y) gameMap == Just '!' = GameState (Map.delete (x,y) gameMap) mines--flagoff
    | Map.lookup (x,y) gameMap == Just 'd' = GameState gameMap mines--do nothing
    | otherwise = GameState (Map.insert (x,y) '!' gameMap) mines --flagon

act :: Board -> GameState -> Action -> GameState
-- gets the action from readAction, converts it to Action and calls dig or toggleFlag
act _ gState (Flag x y) = toggleFlag gState x y
act myBoard gState (Dig x y) = dig myBoard gState x y

isGameOn :: [(Int, Int)] -> GameState -> Bool
-- gets the gameState, returns false only when the game is over (winning or losing)
isGameOn [] _ = False --win
isGameOn (x:xs) (GameState gameMap mines)
    | Map.lookup x gameMap == Just 'd' = isGameOn xs (GameState gameMap mines)
    | isMineDug (toList gameMap) mines = False --lose
    | otherwise = True

isMineDug :: [((Int, Int), Char)] -> [(Int, Int)] -> Bool
isMineDug [] _ = False
isMineDug (x:xs) mines
    | (fst x) `elem` mines && (snd x) == 'd' = True
    | otherwise = isMineDug xs mines

getNonMines :: [((Int, Int), Char)] -> [(Int, Int)]
getNonMines [] = []
getNonMines (x:xs)
    | (snd x) == '*' = getNonMines xs
    | otherwise = [fst x] ++ getNonMines xs

gameOver :: Board -> GameState -> String
gameOver (Board w h bMines myMap) (GameState gameMap mines)
    | isMineDug (toList gameMap) mines = (makeColumns 0 1 w) ++ "\n" ++ (makeRow 1) ++ (convertBoardToString (Board w h bMines myMap) True (sortBySecond (Map.keys myMap)) w 1 (GameState gameMap mines)) ++ "\nBOOM! Game is over" --lost
    | otherwise = (makeColumns 0 1 w) ++ "\n" ++ (makeRow 1) ++ (convertBoardToString (Board w h bMines myMap) False (sortBySecond (Map.keys myMap)) w 1 (GameState gameMap mines)) ++ "\nyou win! all mines cleared"
