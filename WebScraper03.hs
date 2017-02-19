import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.Regex.Posix
import Text.Regex.Base

type URL = String

{-
webReg url regx
PRE: URL is valid
POST: a list of strings of content on url that matched regx
EXAMPLE: webReg "https://www.webhallen.com/se-sv/info/om_oss/jobba_hos_oss" findEmail
== ["kt-jobb@webhallen.com","kt-jobb@webhallen.com","it-jobb@webhallen.com","it-jobb@webhallen.com","erik.estberg@webhallen.com"]
-}
webReg :: URL -> (String -> [[String]]) -> IO [String]
webReg url regx = do
    httpRequest <- parseRequest url
    httpResp <- httpLBS httpRequest
    return $ concat (regx (subWebReg httpResp))

{-
subWebReg httpResp
PRE: True
POST: content of the Response in a string
-}
subWebReg :: (Response L8.ByteString) -> String
subWebReg httpResp =
    let
        htmlBody = getResponseBody httpResp
        htmlString = L8.unpack htmlBody
    in
        htmlString

-- Matrins arbete:

-- finds first date
findDate :: String -> String -> String
findDate [] _ = []
findDate s c = let (_,concat,_,[y, m, d]) = s =~ "([0-9]+)/([0-9]*)/([0-9]*)":: (String,String,String,[String]) 
    in
        case c of
            "year" -> y 
            "month" -> m 
            "day"-> d 
            "concat" -> concat 
            otherWise -> error "please enter year, month or day."

-- find Phone NUmner
findPN :: String -> [[String]]
findPN s = s =~ "[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9][0-9]"

-- find Email
findEmail ::String -> [[String]]
findEmail s = s =~ "[a-zA-z0-9]*.[a-zA-z0-9]*@[a-zA-z0-9]*.[a-zA-z0-9]*"


-- Gustav L egna regex funktioner

findTags :: String -> [[String]]
findTags [] = []
findTags [_] = []
findTags [_,_] = []
findTags (a:b:c:d) | a:[b] == "</" = [c:">"]:(findTags d)
findTags (a:b:d)   | a == '<'      = [a:[b]]:(findTags d)
                   | otherwise     = findTags (b:d)

-- Better than findEmail ;)
superMail :: String -> [[String]]
superMail [] = []
superMail str = findAt str 0 0
   where findAt :: String -> Int -> Int -> [[String]]
         findAt [] _ _ = []
         findAt (s:as) i l | s == '@'                    = [mejl] : findAt as (i+1) 0 where mejl = entireM str i l
         findAt (s:as) i l | s==' ' || s=='"' || s=='>' || s==':' = findAt as (i+1) 0
         findAt (s:as) i l | otherwise                            = findAt as (i+1) (l+1)

entireM :: String -> Int -> Int -> String
entireM str i l | length str <= (i-l) = ""
entireM str i l = if s=='<' || s==' ' || s=='"' || s=='\n'
                  then ""
                  else s : entireM str i (l-1)
                  where s = str!!(i-l)


findLinks :: String -> [[String]]
findLinks [] = []
findLinks str = findL str 6
   where findL :: String -> Int -> [[String]]
         findL [] _ = []
         findL [_] _ = []
         findL [_,_] _ = []
         findL [_,_,_] _ = []
         findL [_,_,_,_] _ = []
         findL [_,_,_,_,_] _ = []
         findL (a:b:c:d:e:f:g) i | a:b:c:d:e:[f] == "href=\"" = [l] : findL g (i+6) where l = entireL str i
         findL (a:b) i                                              = findL b (i+1)

entireL :: String -> Int -> String
entireL str i = if s == '"' || s == '\''
                then ""
                else s : entireL str (i+1)
                where s = str!!i