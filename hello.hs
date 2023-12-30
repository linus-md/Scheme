module Main where
import System.Environment ( getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [name1, name2, age1, age2] -> do
            putStrLn (name1 ++ " says hello to " ++ name2 ++ "!")
            putStrLn (name1 ++ " is " ++ age1 ++ " years old.")
            putStrLn (name2 ++ " is " ++ age2 ++ " years old.")
            putStrLn ("Together, they are " ++ show (read age1 + read age2) ++ " years old.")
        _ -> putStrLn "Please provide exactly four arguments (Two names, followed by two ages.)"
