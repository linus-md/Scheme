module Main where
import System.Environment ( getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg1, arg2] -> putStrLn (arg1 ++ " says hello to " ++ arg2 ++ "!")
        _ -> putStrLn "Please provide exactly two arguments."
