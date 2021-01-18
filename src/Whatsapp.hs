module Whatsapp where

import Control.Applicative

import qualified Data.Map.Strict as Map
import Data.Map.Strict (fromList)
import Data.Maybe

import Parser

type Date = String
type Time = String
type PhoneNumber = String
type Name = String

data FileType
    = Video
    | Audio
    | Image
    deriving (Show, Eq)

type File = (String, FileType)

type Content = Either String File

type Message = (Date, Time, PhoneNumber, Content)

type PhonebookLine = (PhoneNumber, Name)

type Phonebook = Map.Map PhoneNumber Name

-- parsers

parseDate :: Parser Date
parseDate = takeP 7

parseTime :: Parser Time
parseTime = takeP 8

parseDateTime :: Parser (Date, Time)
parseDateTime = do
    charP '['
    date <- parseDate
    stringP ", "
    time <- parseTime
    charP ']'
    return (date, time)

parsePhoneNumber :: Parser PhoneNumber
parsePhoneNumber = takeP 16

parseFile :: Parser File
parseFile = Parser $ \input -> Nothing

parseContent :: Parser Content
parseContent = (fmap Right parseFile) <|> (fmap Left allP)

-- names

parsePhonebookLine :: String -> Maybe PhonebookLine
parsePhonebookLine line = fmap snd $ runParser p line
    where
        p :: Parser PhonebookLine
        p = do
        phoneNumber <- parsePhoneNumber
        stringP ","
        name <- allP
        return (phoneNumber, name)

parsePhonebook :: String -> Phonebook
parsePhonebook = fromList . catMaybes . (map parsePhonebookLine) . lines

getName:: Phonebook -> PhoneNumber -> Maybe Name
getName = flip Map.lookup

parseMessage :: Parser Message
parseMessage = do
    (date, time) <- parseDateTime
    charP ' '
    phone <- parsePhoneNumber
    stringP ": "
    content <- parseContent
    return (date, time, phone, content)


