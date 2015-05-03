module WordProblem (answer) where

import qualified Data.Map.Strict as M
import Data.Char (isSpace)
import Data.List (foldl', find, isPrefixOf)
import Data.Maybe (fromMaybe)

type UnaryFn = Integer -> Integer
type BinaryFn = Integer -> Integer -> Integer

data Operation = UnaryOp UnaryFn | BinaryOp BinaryFn

ops :: M.Map String Operation
ops = M.fromList
    [ ("plus", BinaryOp (+))
    , ("minus", BinaryOp (-))
    , ("divided by", BinaryOp div)
    , ("multiplied by", BinaryOp (*))
    , ("times", BinaryOp (*))
    , ("squared", UnaryOp (pow 2))]
    -- , (" cubed", UnaryOp (pow 3))] Whoops, not actually needed

prefixOps :: M.Map String UnaryFn
prefixOps = M.fromList
    [ ("twice", (* 2))
    , ("half", (`div` 2))
    , ("the square of", (pow 2))
    , ("the cube of", (pow 3))
    , ("negative", negate)]

pow :: BinaryFn
pow = (foldl' (*) 1 .) . replicate . fromIntegral

nums :: M.Map String Integer
nums = M.fromList $ zip
        ["zero", "one", "two", "three", "four", "five", "six",
        "seven", "eight", "nine", "ten", "eleven", "twelve"]
        [0 ..]

answer :: String -> Maybe Integer
answer input = do
    question <- consume "What is" input
    (num, eq) <- readNum question
    if eq == "?"
        then return num
        else answerEquation num eq

answerEquation :: Integer -> String -> Maybe Integer
answerEquation num eq = do
    (op, rest) <- readOperation eq
    (res, rest') <- applyOperation num op rest
    term <- consumeAny isSpace rest'
    if term == "?"
        then return res
        else answerEquation res rest'

readOperation :: String -> Maybe (Operation, String)
readOperation = readFromMap ops

readEnglishNum :: String -> Maybe (Integer, String)
readEnglishNum = readFromMap nums

applyOperation :: Integer -> Operation -> String -> Maybe (Integer, String)
applyOperation num op rest = case op of
    UnaryOp f -> Just (f num, rest)
    BinaryOp f -> do
        (num', rest') <- readNum rest
        return (f num num', rest')

consume :: String -> String -> Maybe String
consume target input = do
    let (t', rest) = splitAt (length target) input
    if t' == target then return rest else fail "Not found"

consumeAny :: (Char -> Bool) -> String -> Maybe String
consumeAny f input = do
    let (_, rest) = span f input
    return rest

readNum :: String -> Maybe (Integer, String)
readNum input = do
    let (f, input') = readFromMapWithDefault id prefixOps input
    (num, rest) <- readInteger input'
    return (f num, rest)

readInteger :: String -> Maybe (Integer, String)
readInteger input = case reads input of
    (found:_) -> Just found
    _ -> readEnglishNum input

readFromMap :: M.Map String a -> String -> Maybe (a, String)
readFromMap mapping input = do
    trimmed <- consumeAny isSpace input
    key <- find (`isPrefixOf` trimmed) (M.keys mapping)
    val <- M.lookup key mapping
    let rest = drop (length key) trimmed
    return (val, rest)

readFromMapWithDefault :: a -> M.Map String a -> String -> (a, String)
readFromMapWithDefault orElse mapping input =
    fromMaybe (orElse, input) $ readFromMap mapping input
