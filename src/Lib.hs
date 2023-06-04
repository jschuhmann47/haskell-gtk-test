module Lib where
import Prelude
import Data.Bifoldable

chain :: [Statement] -> Statement
chain instructions board = foldl (>>=) (return board) instructions

data Board = Board
    { cells :: [Cell]
    , currentCell :: Int
    , dimensions :: Int
    } deriving (Eq, Show)

data Cell = Cell
    { red :: Int
    , blue :: Int
    , green :: Int
    , black :: Int
    } deriving (Eq, Show)

data Colour = Red | Blue | Green | Black deriving (Eq, Show)
data Direction = North | South | East | West deriving (Eq, Show)

type Statement = Board -> Either String Board

type BoolExpr = Board -> Bool
type IntExpr = Board -> Int
type Program = Board -> Either String Board

width :: Board -> Int
width = dimensions
height :: Board -> Int
height = dimensions

emptyCell :: Cell
emptyCell = Cell 0 0 0 0

newBoard :: Int -> Board
newBoard n = Board (replicate (n ^ 2) emptyCell) 0 n

checkDir :: Direction -> BoolExpr
checkDir East (Board _ currentCell dimensions) = mod currentCell dimensions /= dimensions - 1
checkDir West (Board _ currentCell dimensions) = mod currentCell dimensions /= 0
checkDir North (Board _ currentCell dimensions) = currentCell + dimensions < dimensions ^ 2
checkDir South (Board _ currentCell dimensions) = currentCell - dimensions >= 0

moveTowards :: Direction -> Board -> Board
moveTowards South (Board cells currentCell dimensions) = Board cells (currentCell - dimensions) dimensions
moveTowards North (Board cells currentCell dimensions) = Board cells (currentCell + dimensions) dimensions
moveTowards East (Board cells currentCell dimensions) = Board cells (currentCell + 1) dimensions
moveTowards West (Board cells currentCell dimensions) = Board cells (currentCell - 1) dimensions

moveStatement :: Direction -> Statement
moveStatement direction board
    | checkDir direction board = Right $ moveTowards direction board
    | otherwise = Left "error: current cell is outside of the board"

addInCell :: Colour -> Cell -> Cell
addInCell Red cell = cell { red = red cell + 1 }
addInCell Blue cell = cell { blue = blue cell + 1 }
addInCell Green cell = cell { green = green cell + 1 }
addInCell Black cell = cell { black = black cell + 1 }

removeInCell :: Colour -> Cell -> Cell
removeInCell Red cell = cell { red = red cell - 1 }
removeInCell Blue cell = cell { blue = blue cell - 1 }
removeInCell Green cell = cell { green = green cell - 1 }
removeInCell Black cell = cell { black = black cell - 1 }

modifyIndexedCell :: Int -> (Cell -> Cell) -> [Cell] -> [Cell]
modifyIndexedCell n f = map (\(x, c) -> if x == n then f c else c) . zip [0..]

addInBoard :: Colour -> Statement
addInBoard colour (Board cells currentCell dimensions) = Right $ Board (modifyIndexedCell currentCell (addInCell colour) cells) currentCell dimensions

removeInBoard :: Colour -> Statement
removeInBoard colour board@(Board cells currentCell dimensions)
    | numberOfBalls colour board > 0 = Right $ Board (modifyIndexedCell currentCell (removeInCell colour) cells) currentCell dimensions
    | otherwise = Left $ "error: no balls coloured " ++ show colour ++ " to take from current cell"

repeat :: IntExpr -> Statement -> Statement
repeat expr instruction board = chain (replicate (expr board) instruction) board

doIfElse :: BoolExpr -> Statement -> Statement -> Statement
doIfElse condition ifTrue ifFalse board
    | condition board = ifTrue board
    | otherwise = ifFalse board

doIf :: BoolExpr -> Statement -> Statement
doIf condition ifTrue = doIfElse condition ifTrue Right

doUnless :: BoolExpr -> Statement -> Statement
doUnless condition ifFalse = doIfElse condition Right ifFalse

while :: BoolExpr -> Statement -> Statement
while condition ifTrue = doIf condition (chain [ifTrue, while condition ifTrue])

moveToBorder :: Direction -> Statement
moveToBorder dir = while (checkDir dir) (moveStatement dir)

hasBall :: Colour -> BoolExpr
hasBall colour = (> 0) . numberOfBalls colour

numberOfBalls :: Colour -> IntExpr
numberOfBalls colour = numberOfBallsOfColorInCell colour . getCurrentCell

getCurrentCell :: Board -> Cell
getCurrentCell board = cells board !! currentCell board

numberOfBallsOfColorInCell :: Colour -> Cell -> Int
numberOfBallsOfColorInCell Red = red
numberOfBallsOfColorInCell Blue = blue
numberOfBallsOfColorInCell Green = green
numberOfBallsOfColorInCell Black = black

program :: Statement -> Program
program instruction = instruction

aProgram :: Program
aProgram = program $ chain [ moveStatement North
    , addInBoard Black
    , addInBoard Black
    , addInBoard Blue
    , moveStatement North
    , Lib.repeat (const 15)
        (chain [ addInBoard Red
        , addInBoard Blue
        ])
    , doIfElse (hasBall Green)
        (chain [ moveStatement East
        , addInBoard Black
        ])
        (chain [ moveStatement South
        , moveStatement East
        , addInBoard Blue
        ])
    , moveStatement East
    , while ((<= 9) . numberOfBalls Green)
        (addInBoard Green)
    , addInBoard Blue
    ]
