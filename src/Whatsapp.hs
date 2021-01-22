module Whatsapp where

import Control.Applicative

import Data.Char
import qualified Data.Map.Strict as Map
import Data.Map.Strict (fromList)
import Data.Maybe

import Parser

type Date = String
type Time = String
type PhoneNumber = String
type Name = String

data FileType
    = Video VideoFormat
    | Audio AudioFormat
    | Image
    deriving (Show, Eq)

data VideoFormat
    = Mp4
    deriving (Show, Eq)

videoFormat :: String -> Maybe VideoFormat
videoFormat ".mp4" = Just Mp4

data AudioFormat
    = Opus
    | Mp3
    deriving (Show, Eq)

audioFormat :: String -> Maybe AudioFormat
audioFormat ".opus" = Just Opus
audioFormat ".mp3" = Just Mp3


type File = (String, FileType)

data Content
    = ContentFile File
    | ContentMessage String
    deriving (Show, Eq)

data Message = Message
    {getDate :: Date,
     getTime :: Time,
     getPhoneNumber :: PhoneNumber,
     getContent:: Content
    } deriving (Show, Eq)

type PhonebookLine = (PhoneNumber, Name)

type Phonebook = Map.Map PhoneNumber Name

-- parsers

parseDate :: Parser Date
parseDate = do
    day   <- spanToP 2 isNumber
    charP '/'
    month <- spanToP 2 isNumber
    charP '/'
    year  <- spanToP 2 isNumber
    return (day <> "/" <> month <> "/" <> year)

parseTime :: Parser Time
parseTime = do
    hours <- takePredP 2 isNumber
    charP ':'
    mins  <- takePredP 2 isNumber
    charP ':'
    sec   <- takePredP 2 isNumber
    return (hours <> ":" <> mins <> ":" <> sec)

parseDateTime :: Parser (Date, Time)
parseDateTime = do
    charP '['
    date <- parseDate
    stringP ", "
    time <- parseTime
    charP ']'
    return (date, time)

parsePhoneNumber :: Parser PhoneNumber
parsePhoneNumber = takeP 12

parseVideo :: Parser String -> Parser (FileType, String)
parseVideo pr = do
    extension <- pr
    let format = fromJust $ videoFormat extension
    return (Video format, extension)

parseAudio :: Parser String -> Parser (FileType, String)
parseAudio pr = do
    extension <- pr
    let format = fromJust $ audioFormat extension
    return (Audio format, extension)

parseFileType :: Parser (FileType, String)
parseFileType =
    (parseVideo $ parseAny videoExtensions) <|>
    (parseAudio $ parseAny audioExtensions) <|>
    (fmap ((,) Image) $ parseAny imageExtensions)
        where
            videoExtensions = [".mp4"]
            audioExtensions = [".opus", ".mp3"]
            imageExtensions = [".jpg"]

parseFile :: Parser File
parseFile = do
    stringP "<attached: "
    name <- spanP (/= '.')
    (fileType, ext) <- parseFileType
    charP '>'
    return (name ++ ext, fileType)

parseContent :: Parser Content
parseContent = (fmap ContentFile parseFile) <|> (fmap ContentMessage allP)

-- names

parsePhonebookLine :: Parser PhonebookLine
parsePhonebookLine = do
            phoneNumber <- spanP (/= ',')
            stringP ", "
            name <- spanP (/= ',')
            return (filter (not . isSpace) phoneNumber, name)

parsePhonebook :: String -> Phonebook
parsePhonebook = fromList . catMaybes . (map $ fmap snd . runParser parsePhonebookLine) . lines

getName:: Phonebook -> PhoneNumber -> Maybe Name
getName = flip Map.lookup

parseMessage :: Parser Message
parseMessage = do
    (date, time) <- parseDateTime
    charP ' '
    phone <- parsePhoneNumber
    stringP ": "
    content <- parseContent
    return $ Message date time phone content


