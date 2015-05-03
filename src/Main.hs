import qualified WordProblem as W

import Data.Char (toLower)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    putStr ">> "
    getLine >>= handleLine
    where handleLine line
            | elem lc ["q", "quit"] = putStrLn "Goodbye!"
            | elem lc ["hi", "hey", "hello", "yo"] = putStrLn "Hi" >> main
            | lc == "how are you?" = putStrLn "fine" >> main
            | lc == "who are you?" = putStrLn "Not sure" >> main
            | lc == "what are you?" = putStrLn "A deterministic Turing machine" >> main
            | elem lc ["h", "help"] = putStrLn "Ask a question" >> main
            | "you are " `isPrefixOf` lc = putStrLn "No I'm not!" >> main
            | last line /= '?' = putStrLn "That isn't a question!" >> main
            | otherwise = putStrLn (maybe "Dunno" show (W.answer line)) >> main
            where lc = map toLower line
