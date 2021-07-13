{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Text.Parsec
import System.Environment
import Text.Show.Unicode

newtype Unimarc = Unimarc [Field] deriving Show
data Field = Field FieldNum Indicators SubFields deriving Show
newtype FieldNum = FieldNum String deriving Show
newtype Indicators = Indicators String deriving Show
data SubFields = Simple [(String, String)] | Recurent [(Char, Field)] deriving Show

instance Eq FieldNum where
        FieldNum a == FieldNum b = a == b

----------------------------------------------------------------------------------------

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
                    content <- choice [many1 (noneOf "[\n"), many1 (noneOf "\n")]
                    return (label, content)

labelParse :: Parsec String () String
labelParse =    do
                label <- alphaNum
                char ']'
                return [label]

------------------------------------------------------------------------------------------

-- type ListZipper a = ([a], [a])

-- goForward :: ListZipper a -> ListZipper a
-- goForward (x:xs, bs) = (xs, x:bs)

-- goBack :: ListZipper a -> ListZipper a
-- goBack (xs, b:bs) = (b:xs, bs)

appendToUnimarc:: Unimarc -> String -> (String, String) -> [Field]
appendToUnimarc (Unimarc []) searchFieldNum (addLabel, addContent) = []
appendToUnimarc (Unimarc (x:xs)) searchFieldNum (addLabel, addContent) = [appendToField x searchFieldNum (addLabel, addContent)] ++ appendToUnimarc (Unimarc xs) searchFieldNum (addLabel, addContent)

appendToField:: Field -> String -> (String, String) -> Field
appendToField (Field (FieldNum fieldNum) indicators subFields) searchFieldNum (addLabel, addContent) =     if searchFieldNum == fieldNum
                                                                                                            then (Field (FieldNum fieldNum) indicators $ appendToSubFields subFields (addLabel, addContent))
                                                                                                            else (Field (FieldNum fieldNum) indicators subFields)

appendToSubFields:: SubFields -> (String, String) -> SubFields
appendToSubFields (Simple xs) x = Simple (xs ++ [x])

----------------------------------------------------------------------------------------

searchLibrary:: [Unimarc] -> String -> String -> String
searchLibrary [] fieldNum label = "Not Found"
searchLibrary (x:xs) fieldNum label =     if searchUnimarc x fieldNum label == ""
                                          then searchLibrary xs fieldNum label
                                          else searchUnimarc x fieldNum label

searchUnimarc:: Unimarc -> String -> String -> String
searchUnimarc (Unimarc []) _ _ = ""
searchUnimarc (Unimarc (x:xs)) fieldNum label =       if searchField x fieldNum label == ""
                                                      then searchUnimarc (Unimarc xs) fieldNum label
                                                      else searchField x fieldNum label

searchField:: Field -> String -> String -> String
searchField (Field (FieldNum fieldNum) indicators subFields) searchFieldNum label =       if fieldNum == searchFieldNum
                                                                                          then searchSubFields subFields label
                                                                                          else ""

searchSubFields:: SubFields -> String -> String
searchSubFields (Simple []) _ = ""
searchSubFields (Simple (x:xs)) label =   if checkChar x label == ""
                                          then searchSubFields (Simple xs) label
                                          else checkChar x label

checkChar:: (String, String) -> String -> String
checkChar (currLabel, currContent) label = if label == currLabel
                                          then currContent
                                          else ""

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
main = do [input, op] <- getArgs
          cnts <- readFile input
          case runParser libraryParse () input cnts of
                Left err -> putStrLn . show $ err
                Right library -> operationPicker library op

operationPicker:: [Unimarc] -> String -> IO ()
operationPicker library op = case op of
                                    "json" -> putStr $ toJsonLibrary library
                                    "search" -> do
                                                putStr "FieldNum: "
                                                fieldNum <- getLine
                                                putStr "Label: "
                                                label <- getLine
                                                putStr "Unimarc index: "
                                                unimarcIdx <- getLine
                                                putStr $ searchUnimarc (library !! read unimarcIdx) fieldNum label

                                    "save" ->   do
                                                putStr "File name: "
                                                output <- getLine
                                                writeFile output $ show library

                                    "append" -> do
                                                putStr "FieldNum: "
                                                fieldNum <- getLine
                                                putStr "Label: "
                                                label <- getLine
                                                putStr "Content: "
                                                content <- getLine
                                                putStr "Unimarc index: "
                                                unimarcIdx <- getLine
                                                writeFile "append.txt" $ show $ appendToUnimarc (library !! read unimarcIdx) fieldNum (label, content)
                                    otherwise -> putStr "error"