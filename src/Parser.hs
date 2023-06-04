module Parser where
import Control.Applicative
import Data.Char
import Lib
import Control.Monad
import Data.Either (fromRight)

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
    fmap function parser = do
        value <- parser
        pure $ function value

instance Applicative Parser where
    pure x = Parser $ \input -> return (input, x)
    p1 <*> p2 = do
        resultFunction <- p1
        resultValue <- p2
        pure $ resultFunction resultValue

instance Monad Parser where
    parser >>= function = Parser $
        runParser parser >=>
            \(rest, obtained) -> runParser (function obtained) rest

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

charP :: Char -> Parser Char
charP char = conditionP (== char)

strP :: String -> Parser String
strP = traverse charP

digitP :: Parser Int
digitP = subtract 48 . ord <$> conditionP isDigit

spaceP :: Parser Char
spaceP = conditionP isSpace

manySpacesP :: Parser String
manySpacesP = many spaceP

conditionP :: (Char -> Bool) -> Parser Char
conditionP condition = Parser $ \input -> case input of
    (x : xs) -> if condition x then Just (xs, x) else Nothing
    [] -> Nothing

withParentheses :: Parser a -> Parser a
withParentheses p = manySpacesP *> charP '(' *> manySpacesP *> p <* manySpacesP <* charP ')' <* manySpacesP

withCurlyBrackets :: Parser a -> Parser a
withCurlyBrackets p = manySpacesP *> charP '{' *> manySpacesP *> p <* manySpacesP <* charP '}' <* manySpacesP

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = (:) <$> p <*> many (sep *> p) <|> pure []

directionP :: Parser Direction
directionP = northP <|> southP <|> eastP <|> westP

northP :: Parser Direction
northP = North <$ strP "Norte"

southP :: Parser Direction
southP = South <$ strP "Sur"

eastP :: Parser Direction
eastP = East <$ strP "Este"

westP :: Parser Direction
westP = West <$ strP "Oeste"

moveP :: Parser Statement
-- moveP = (moveStatement <$ strP "Mover") <*> withParentheses directionP
moveP = do
    _ <- strP "Mover"
    direction <- withParentheses directionP
    pure $ moveStatement direction

colourP :: Parser Colour
colourP = redP <|> blueP <|> greenP <|> blackP

redP :: Parser Colour
redP = Red <$ strP "Rojo"

blueP :: Parser Colour
blueP = Blue <$ strP "Azul"

greenP :: Parser Colour
greenP = Green <$ strP "Verde"

blackP :: Parser Colour
blackP = Black <$ strP "Negro"

addP :: Parser Statement
addP = (addInBoard <$ strP "Poner") <*> withParentheses colourP

removeP :: Parser Statement
removeP = (removeInBoard <$ strP "Sacar") <*> withParentheses colourP

statementP :: Parser Statement
statementP = moveP <|> addP <|> removeP <|> compoundStatementP <|> ifElseP <|> ifP <|> whileP <|> repeatP

compoundStatementP :: Parser Statement
compoundStatementP = chain <$> withCurlyBrackets (sepBy manySpacesP statementP)

ifP :: Parser Statement
ifP = (doIf <$ strP "if") <*> withParentheses boolExprP <*> statementP

ifElseP :: Parser Statement
ifElseP = (doIfElse <$ strP "if") <*> withParentheses boolExprP <*> statementP <* manySpacesP <* strP "else" <* manySpacesP <*> statementP

boolExprP :: Parser BoolExpr
boolExprP = hasBallP <|> checkDirP

hasBallP :: Parser BoolExpr
hasBallP = fmap hasBall $ strP "hayBolitas" *> withParentheses colourP

checkDirP :: Parser BoolExpr
checkDirP = fmap checkDir $ strP "puedeMover" *> withParentheses directionP

whileP :: Parser Statement
whileP = (while <$ strP "while") <*> withParentheses boolExprP <*> statementP

repeatP :: Parser Statement
repeatP = (Lib.repeat <$ strP "repeat") <*> withParentheses intExprP <*> statementP

intExprP :: Parser IntExpr
intExprP = numberOfBallsP <|> fmap const intLiteralP

numberOfBallsP :: Parser IntExpr
numberOfBallsP = (numberOfBalls <$ strP "nroBolitas") <*> withParentheses colourP

intLiteralP :: Parser Int
intLiteralP = foldl (\s x -> s * 10 + x) 0 <$> some digitP

moveToBorderP :: Parser Statement
moveToBorderP = (moveToBorder <$ strP "IrAlBorde") <*> withParentheses directionP

programP :: Parser Program
programP = (program <$ strP "program") <*> statementP

parsearPrograma :: String -> Maybe Program
parsearPrograma codigoFuente =
    snd <$> runParser programP codigoFuente

parsearProgramaEn :: FilePath -> IO Program
parsearProgramaEn path = do
    gobstonesSourceCode <- readFile path
    case runParser programP gobstonesSourceCode of
        Nothing -> error "No se pudo parsear el programa"
        Just (_rest, program) -> pure program

ejecutarProgramaEn :: FilePath -> Int -> IO String
ejecutarProgramaEn path tamanioTablero = do
    programa <- parsearProgramaEn path
    pure . show . fromRight (newBoard tamanioTablero) . programa . newBoard $ tamanioTablero