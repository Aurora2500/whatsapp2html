module Html where

import Data.Char

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
\  </body>\n\
\</html>\n\
\ "

contentToHTML :: Content -> HTML
contentToHTML (ContentMessage str) = str
contentToHTML (ContentFile (file, Image)) = "<img src=\"media/" ++ file ++ "\" width=\"128\">"
contentToHTML (ContentFile (file, (Audio format))) =
    "<audio controls><source src=\"media/" ++ file ++ "\" type=\"audio/opus\"></audio>"
contentToHTML (ContentFile (file, (Video format))) =
    "<video width=\"320\" height=\"240\" controls><source src=\"" ++ file ++"\" type=\"video/mp4\"></video>"

messageToHTML :: Message -> HTML
messageToHTML message=
    "\
\    <div class=\"message\">\n\
\      <div class=\"phonenumber\">\n"
        ++ getPhoneNumber message ++ "\n\
\      </div>\n\
\      <div class=\"content\">\n"
        ++ contentToHTML (getContent message) ++ "\n\
\      </div>\n\
\    </div>\n\
\ "

generateHTML :: [Message] -> HTML
generateHTML msgs = header ++ concat (map messageToHTML msgs) ++ footer
