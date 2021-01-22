module Html where

import Data.Char
import Data.List
import Data.Maybe

import Whatsapp

type HTML = String

header :: HTML
header =
    "\
\<!DOCTYPE html>\n\
\<html lang=\"es\">\n\
\  <head>\n\
\    <meta charset=\"UTF-8\">\n\
\    <title>Whatsapp</title>\n\
\    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n\
\    <link rel=\"stylesheet\" href=\"styles.css\">\n\
\  </head>\n\
\  <body>\n\
\ "

footer :: HTML
footer =
    "\
\    <script>\n\
\     window.scrollTo(0,document.body.scrollHeight);\n\
\   </script>\n\
\  </body>\n\
\</html>\n\
\ "

addLinks :: String -> String
addLinks = unwords . (map linkify) . words
    where
        linkify wrd
          | or $ isPrefixOf <$> ["http://", "https"] <*> (pure wrd) = "<a href=\"" ++ wrd ++ "\">" ++ wrd ++ "</a>"
          | otherwise = wrd

preprocessString :: String -> String
preprocessString str = foldl (flip ($)) str preprocesses
    where
        preprocesses = [addLinks]

contentToHTML :: Content -> HTML
contentToHTML (ContentMessage str) = preprocessString str
contentToHTML (ContentFile (file, Image)) = "<img src=\"media/" ++ file ++ "\" width=\"128\">"
contentToHTML (ContentFile (file, (Audio format))) =
    "<audio controls><source src=\"media/" ++ file ++ "\" type=\"audio/ogg\"></audio>"
contentToHTML (ContentFile (file, (Video format))) =
    "<video width=\"320\" height=\"240\" controls><source src=\"media/" ++ file ++"\" type=\"video/mp4\"></video>"

getUser :: Phonebook -> PhoneNumber -> HTML
getUser phonebook phone =
    "\
\      <div class=\"user\">\n\
\        <div class=\"phonenumber\">\n\
\          " ++ phone ++ "\n\
\        </div>\n\
\        <div class=\"name\">\n\
\         " ++ name ++ "\n\
\        </div>\n\
\      </div>\n"
    where
        name = fromMaybe "" $ getName phonebook phone

getDateTime :: Date -> Time -> HTML
getDateTime date time =
    "\
\   <div class=\"datetime\">\n\
\     " ++ date ++ " | " ++ time ++ "\n\
\   </div>"

messageToHTML :: Phonebook -> Message -> HTML
messageToHTML phonebook message =
    "\
\    <div class=\"message\">\n"
        ++ (getUser phonebook $ getPhoneNumber message) ++ "\
\      <div class=\"content\">\n"
        ++ contentToHTML (getContent message) ++ "\n\
\      </div>\n"
        ++ getDateTime (getDate message) (getTime message) ++ "\
\    </div>\n\
\ "

generateHTML :: Phonebook -> [Message] -> HTML
generateHTML pb msgs = header ++ concat (map (messageToHTML pb) msgs) ++ footer
