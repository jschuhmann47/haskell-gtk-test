module Lib where
import Prelude

chain :: [Statement] -> Statement
chain instructions board = foldl (>>=) (return board) instructions

data Board = Board
    { cells :: [Cell]
    , currentCell :: Int
    , dimensions :: Int
    , previous :: Maybe Board
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
type Program = Statement
type BoolOperand = Int -> Int -> Bool

width :: Board -> Int
width = dimensions
height :: Board -> Int
height = dimensions

emptyCell :: Cell
emptyCell = Cell 0 0 0 0

newBoard :: Int -> Board
newBoard n = Board (replicate (n ^ 2) emptyCell) 0 n Nothing

allBoardStates :: Board -> [Board]
allBoardStates board = case previous board of
    Nothing -> []
    Just previousBoard -> allBoardStates previousBoard ++ [board]

-- checkDir :: Direction -> BoolExpr
-- checkDir East (Board _ currentCell dimensions) = mod currentCell dimensions /= dimensions - 1
-- checkDir West (Board _ currentCell dimensions) = mod currentCell dimensions /= 0
-- checkDir North (Board _ currentCell dimensions) = currentCell + dimensions < dimensions ^ 2
-- checkDir South (Board _ currentCell dimensions) = currentCell - dimensions >= 0

currentCoordinate :: Board -> (Int, Int)
currentCoordinate board = currentCell board `quotRem` dimensions board

isValidPosition :: BoolExpr
isValidPosition board =
    fst (currentCoordinate board) <= width board &&
    snd (currentCoordinate board) <= height board

checkDir :: Direction -> BoolExpr
checkDir direction = isValidPosition . moveTowards direction

moveTowards :: Direction -> Board -> Board
moveTowards South board = board { currentCell = currentCell board - width board }
moveTowards North board = board { currentCell = currentCell board + width board }
moveTowards East board = board { currentCell = currentCell board + 1 }
moveTowards West board = board { currentCell = currentCell board - 1 }

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

modifyIndexedCell :: Int -> (Cell -> Cell) -> Board -> Board
modifyIndexedCell idx f board =
    board { cells = modifyIndexed idx f (cells board) }

modifyCurrentCell :: (Cell -> Cell) -> Board -> Board
modifyCurrentCell f board =
    modifyIndexedCell (currentCell board) f board

modifyIndexed :: Int -> (a -> a) -> [a] -> [a]
modifyIndexed expectedIdx f =
    map (\(idx, value) -> if idx == expectedIdx then f value else value) . zip [0..]

-- newFrom :: Board -> Board

moveStatement :: Direction -> Statement
moveStatement direction board
    | isValidPosition (moveTowards direction board) = Right $ (moveTowards direction board) { previous = Just board }
    | otherwise = Left "error: current cell is outside of the board"

addInBoard :: Colour -> Statement
addInBoard colour board =
    Right $ (modifyCurrentCell (addInCell colour) board) { previous = Just board }

removeInBoard :: Colour -> Statement
removeInBoard colour board
    | numberOfBalls colour board > 0 = Right $ (modifyCurrentCell (removeInCell colour) board) { previous = Just board }
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

comparison :: IntExpr -> BoolOperand -> IntExpr -> BoolExpr
comparison intExpr1 compareF intExpr2 board = compareF (intExpr1 board) (intExpr2 board)

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
    , while (comparison (numberOfBalls Green) (<=) (const 9))
        (addInBoard Green)
    , addInBoard Blue
    ]
