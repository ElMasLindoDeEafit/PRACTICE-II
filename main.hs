module Main where

-- Define la función que imprime la frase
printAssignment :: IO ()
printAssignment = putStrLn "Assignment 3 by Jose Alejandro Jimenez Vasquez"

-- Método principal que llama a la función printAssignment
main :: IO ()
main = do
    printAssignment
