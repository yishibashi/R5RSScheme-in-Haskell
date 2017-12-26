module Main where

import           Lib
import           Prelude
import           System.Environment


main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0)

    {-- Exercises --}

    let args1 = args !! 0
    let args2 = args !! 1
    let val1 = read args1
    let val2 = read args2

    putStrLn ("args: " ++ args1 ++ " " ++ args2)

    putStrLn (args1 ++ " + " ++ args2 ++ " = " ++ show (val1 + val2 ))

    putStrLn "Tell me your name."
    name <- getLine
    putStrLn ("Hi, " ++ name ++ "!")

