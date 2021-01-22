module Main where

import Data.Maybe

import System.Environment
import System.IO

import Parser
import Whatsapp
import Html

specialChars = ['\160', '\8234', '\8236', '\8206']

removeSpecialChars :: String -> String
removeSpecialChars = filter $ not . (flip elem) specialChars

process :: String -> String -> String
process log phonebook
  = generateHTML (parsePhonebook phonebook)
  $ map snd
  $ catMaybes
  $ map (runParser parseMessage)
  $ lines
  $ removeSpecialChars
  $ log

main :: IO ()
main = do
    [logPath, resultPath, phonebookPath] <- getArgs
    log <- readFile logPath
    phonebook <- readFile phonebookPath
    writeFile resultPath $ process log phonebook


