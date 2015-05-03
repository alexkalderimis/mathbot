import Data.Char (toLower)
import Data.List (isPrefixOf)
import System.IO (hFlush, stdout)
import Data.Time (getCurrentTime)

import WordProblem (answer)

main :: IO ()
main = do
    putStr ">> "
    hFlush stdout
    getLine >>= handleLine
    where handleLine line
            | null line = main
            | elem lc ["q", "quit"] = putStrLn "Goodbye!"
            | elem lc ["hi", "hey", "hello", "yo"] = putStrLn "Hi" >> main
            | lc == "how are you?" = putStrLn "fine" >> main
            | lc == "who are you?" = putStrLn "Mathbot!" >> main
            | lc == "what are you?" = putStrLn "A deterministic Turing machine" >> main
            | lc == "what time is it?" = getCurrentTime >>= print >> main
            | elem lc ["h", "help"] = putStrLn "Ask a question" >> main
            | "you are " `isPrefixOf` lc = putStrLn "No I'm not!" >> main
            | last line /= '?' = putStrLn "That isn't a question!" >> main
            | otherwise = putStrLn (maybe "Dunno" show (answer line)) >> main
            where lc = map toLower line
