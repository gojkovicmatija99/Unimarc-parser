import Text.Parsec
import System.Environment
import Text.Show.Unicode

newtype Unimarc = Unimarc [Field] deriving Show
data Field = Field FieldNum Indicators SubFields deriving Show
newtype FieldNum = FieldNum String deriving Show
newtype Indicators = Indicators String deriving Show
data SubFields = Simple [(String, String)] | Recurent [(Char, Field)] deriving Show

libraryParse :: Parsec String () [Unimarc]
libraryParse = sepEndBy unimarcParse $ char '\n'

unimarcParse :: Parsec String () Unimarc
unimarcParse =  do
                unimarc <- sepEndBy fieldParse $ char '\n'
                return $ Unimarc unimarc


fieldParse :: Parsec String () Field
fieldParse =    do
                fieldNum <- fieldNumParse
                spaces
                indicators <- indicatorsParse
                spaces
                char '['
                subfields <- subfieldsParse
                return $ Field fieldNum indicators (Simple subfields)

fieldNumParse :: Parsec String () FieldNum
fieldNumParse = do
                fieldNum <- count 3 digit
                return $ FieldNum fieldNum

indicatorsParse :: Parsec String () Indicators
indicatorsParse =   do
                    indicator1 <- oneOf "#0123456789"
                    indicator2 <- oneOf "#0123456789"
                    return $ Indicators [indicator1, indicator2]

subfieldsParse :: Parsec String () [(String, String)]
subfieldsParse = sepEndBy labelContentParse $ char '['

labelContentParse :: Parsec String () (String, String)
labelContentParse = do
                    label <- labelParse
                    content <- many1 (noneOf "[\n")
                    return (label, content)

labelParse :: Parsec String () String
labelParse =    do
                label <- alphaNum
                char ']'
                return [label]

------------------------------------------------------------------------------------------

instance Eq FieldNum where
        FieldNum a == FieldNum b = a == b

searchLibrary:: [Unimarc] -> String -> String -> String
searchLibrary [] fieldNum label = "NOT FOUND"
searchLibrary (x:xs) fieldNum label = if searchUnimarc x fieldNum label == "NOT FOUND" then searchLibrary xs fieldNum label else searchUnimarc x fieldNum label

searchUnimarc:: Unimarc -> String -> String -> String
searchUnimarc (Unimarc []) _ _ = "NOT FOUND"
searchUnimarc (Unimarc (x:xs)) fieldNum label = if searchField x fieldNum label == "NOT FOUND" then searchUnimarc (Unimarc xs) fieldNum label else searchField x fieldNum label

searchField:: Field -> String -> String -> String
searchField (Field (FieldNum fieldNum) indicators subFields) searchFieldNum label = if fieldNum == searchFieldNum then searchSubFields subFields label else "NOT FOUND"

searchSubFields:: SubFields -> String -> String
searchSubFields (Simple []) _ = "NOT FOUND"
searchSubFields (Simple (x:xs)) label =    if checkChar x label == "NOT FOUND" then searchSubFields (Simple xs) label else checkChar x label

checkChar:: (String, String) -> String -> String
checkChar (currLabel, currContent) label = if label == currLabel then currContent else "NOT FOUND"

-----------------------------------------------------------------------------------------

length' :: (Num b) => [a] -> b
length' [] = 0
length' xs = sum [1 | _ <- xs]

toJsonLibrary:: [Unimarc] -> String
toJsonLibrary library = "{ \"library\": [" ++ libraryHelper library ++ "]}"

libraryHelper:: [Unimarc] -> String
libraryHelper [] = ""
libraryHelper (x:xs)
        | length' xs > 0 = toJsonUnimarc x ++ "," ++ libraryHelper xs
        | otherwise = toJsonUnimarc x ++ libraryHelper xs

toJsonUnimarc:: Unimarc -> String
toJsonUnimarc unimarc = "{\"unimarc\": [" ++ unimarcHelper unimarc ++ "]}"

unimarcHelper:: Unimarc -> String
unimarcHelper (Unimarc []) = ""
unimarcHelper (Unimarc (x:xs))
        | length' xs > 0 = toJsonField x ++ "," ++ unimarcHelper (Unimarc xs)
        | otherwise = toJsonField x ++ unimarcHelper (Unimarc xs)

toJsonField:: Field -> String
toJsonField (Field fieldNum indicators subFields) = "{" ++ toJsonFieldNum fieldNum ++ "," ++ toJsonIndicators indicators ++ "," ++ toJsonSubFields subFields ++ "}"

toJsonFieldNum::FieldNum -> String
toJsonFieldNum (FieldNum fieldNum) = "\"fieldNum\": " ++ "\"" ++ fieldNum ++ "\""

toJsonIndicators:: Indicators -> String
toJsonIndicators (Indicators ind) = "\"indicators\": " ++ "\"" ++ ind ++ "\""

toJsonSubFields:: SubFields -> String
toJsonSubFields subFields = "\"subFields\": [" ++ subFieldHelper subFields ++ "]"

subFieldHelper:: SubFields -> String
subFieldHelper (Simple []) = ""
subFieldHelper (Simple (x:xs))
        | length' xs > 0 = showSubFields x ++ "," ++ subFieldHelper (Simple xs)
        | otherwise = showSubFields x ++ subFieldHelper (Simple xs)

showSubFields:: (String, String) -> String
showSubFields (label, content) = "{\"label\":" ++ show label ++ ",\"content\":" ++ ushow content ++ "}"

-----------------------------------------------------------------------------------------

main :: IO ()
main = do [input, output, op] <- getArgs
          cnts <- readFile input
          case runParser libraryParse () input cnts of
                Left err -> putStrLn . show $ err
                Right library -> case op of
                                        "json" -> putStr $ toJsonLibrary library
                                        "search" -> do
                                                        fieldNum <- getLine  
                                                        label <- getLine 
                                                        putStr $ searchLibrary library fieldNum label
                                        otherwise -> putStr "error"                               


