import Network.HTTP
import Data.List

data Browser = Browser { getUrl :: String, getHtml :: String, getLinks :: [String]} deriving Show
data Page = Page {pageTitle :: String, pageLinks :: [String]} deriving Show
type SiteMap = [Page]

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

getCode :: String -> IO ResponseCode
getCode url = simpleHTTP req >>= getResponseCode
    where req = getRequest url

-- helper function to find the index of a string in a string
indexOfString ::String -> String -> Int -> Int
indexOfString haystack needle pos
    | isInfixOf needle haystack == True && haystack /= "" =
      let len = length needle in
      if take len haystack == needle 
        then pos
        else let s = tail haystack in 
                indexOfString s needle pos+1
    | otherwise = -1


domainFromURL :: String -> String
domainFromURL u = if take 7 u == "http://" 
    then take 7 u ++ (takeWhile (/='/') $ drop 7 u)
    else if take 8 u == "https://"
        then take 8 u ++ (takeWhile (/='/') $ drop 8 u)
        else u

relPgFromURL :: String -> String
relPgFromURL u = reverse . dropWhile (/='/') $ reverse u

formatURL :: String -> String -> String
formatURL u link = case link of
    ('.':'/':x) -> (relPgFromURL u) ++ (drop 2 link)
    ('/':x) -> (domainFromURL u) ++ link
    ('.':'.':'/':x) -> formatURL (relPgFromURL $ init (relPgFromURL u)) x
    ('h':'t':'t':'p':x) -> if indexOfString link (domainFromURL u) 0 > 0 then x else "invalid"
    x -> (relPgFromURL u) ++ link

extractLinks ::  String -> String -> [String] -> [String]
extractLinks u html l = 
    if take 4 html == "href"
        then let link = takeWhile (/='"') $ drop 6 html
                 newHtml = drop ((indexOfString (drop 6 html) "href" 0)+6) html 
                 fLink = formatURL u link in
                    if fLink /= "invalid"
                        then extractLinks u newHtml l ++ [fLink]
                        else extractLinks u newHtml l
        else let i = (indexOfString html "href" 0) in
             if i == -1
                then l 
                else extractLinks u (drop i html) l 


extractTitle :: String -> String
extractTitle html = 
    if take 7 html == "<title>"
        then let title = takeWhile (/='<') $ drop 7 html in
                    title
        else let i = (indexOfString html "<title>" 0) in
            if i == -1
                then "" 
                else extractTitle $ drop i html


goto :: String -> IO Browser
goto url = do
             html <- get url 
             let links = extractLinks url html []
             return (Browser url html links)

main = do  
    putStrLn  "Please enter the url : "
    iURL <- getLine
    b1 <- goto iURL
    let p1 = Page (extractTitle (getHtml b1)) (getLinks b1) in
       putStrLn $ "Content : " ++ (show p1)
