module Main where

import Data.Maybe

import System.Environment
import System.IO

import Parser
import Whatsapp
import Html


process :: String -> String -> String
process log phonebook = generateHTML $ map snd $ catMaybes $ map (runParser parseMessage) $ lines log

main :: IO ()
main = do
    [logPath, resultPath, phonebookPath] <- getArgs
    log <- readFile logPath
    phonebook <- readFile phonebookPath
    writeFile resultPath $ process log phonebook


