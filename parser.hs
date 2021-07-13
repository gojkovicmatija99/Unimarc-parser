import Text.Parsec
import System.Environment
import Text.Show.Unicode

newtype Unimarc = Unimarc [Field]
data Field = Field FieldNum Indicators SubFields
newtype FieldNum = FieldNum String
newtype Indicators = Indicators String
data SubFields = Simple [(String, String)] | Recurent [(String, Field)]

showLibrary:: [Unimarc] -> String
showLibrary [] = ""
showLibrary (x:xs) = show x ++ "\n" ++ showLibrary xs

instance Show Unimarc where
      show (Unimarc []) = ""
      show (Unimarc (x:xs)) = show x ++ "\n" ++ show (Unimarc xs)

instance Show Field where
      show (Field fieldNum indicators subFields) = show fieldNum ++ " " ++ show indicators ++ " " ++ show subFields

instance Show FieldNum where
      show (FieldNum fieldNum) = fieldNum 

instance Show Indicators where
      show (Indicators indicators) = indicators

instance Show SubFields where
      show (Simple []) = ""
      show (Simple (x:xs)) = showSubField x ++ show (Simple xs)

showSubField:: (String, String) -> String
showSubField (label, content) = "[" ++ label ++ "]" ++ content

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
                    content <- many1 (noneOf "[\n") <|> bracketException
                    return (label, content)

bracketException :: Parsec String () String
bracketException =      do
                        char '['
                        content <- many1 (noneOf "[\n")
                        return $ "[" ++ content


labelParse :: Parsec String () String
labelParse =    do
                label <- alphaNum
                char ']'
                return [label]

------------------------------------------------------------------------------------------

appendToLibrary:: [Unimarc] -> String -> (String, String) -> Int -> [Unimarc]
appendToLibrary (x:xs) searchFieldNum (addLabel, addContent) 0 = Unimarc (appendToUnimarc x searchFieldNum (addLabel, addContent)) : xs
appendToLibrary (x:xs) searchFieldNum (addLabel, addContent) unimarcIdx = x : appendToLibrary xs searchFieldNum (addLabel, addContent) (unimarcIdx - 1)

appendToUnimarc:: Unimarc -> String -> (String, String) -> [Field]
appendToUnimarc (Unimarc []) _ _ = []
appendToUnimarc (Unimarc (x:xs)) searchFieldNum (addLabel, addContent) = appendToField x searchFieldNum (addLabel, addContent) : appendToUnimarc (Unimarc xs) searchFieldNum (addLabel, addContent)

appendToField:: Field -> String -> (String, String) -> Field
appendToField (Field (FieldNum fieldNum) indicators subFields) searchFieldNum (addLabel, addContent) =     
      if searchFieldNum == fieldNum
      then Field (FieldNum fieldNum) indicators $ appendToSubFields subFields (addLabel, addContent)
      else Field (FieldNum fieldNum) indicators subFields

appendToSubFields:: SubFields -> (String, String) -> SubFields
appendToSubFields (Simple xs) x = Simple (xs ++ [x])

removeFromLibrary:: [Unimarc] -> String -> String -> Int -> [Unimarc]
removeFromLibrary (x:xs) searchFieldNum removeLabel 0 = Unimarc (removeFromUnimarc x searchFieldNum removeLabel) : xs
removeFromLibrary (x:xs) searchFieldNum removeLabel unimarcIdx = x : removeFromLibrary xs searchFieldNum removeLabel (unimarcIdx - 1)

removeFromUnimarc:: Unimarc -> String -> String -> [Field]
removeFromUnimarc (Unimarc []) _ _ = []
removeFromUnimarc (Unimarc (x:xs)) searchFieldNum removeLabel = 
      removeFromField x searchFieldNum removeLabel : removeFromUnimarc (Unimarc xs) searchFieldNum removeLabel

removeFromField:: Field -> String -> String -> Field
removeFromField (Field (FieldNum fieldNum) indicators subFields) searchFieldNum removeLabel =     
      if searchFieldNum == fieldNum
      then Field (FieldNum fieldNum) indicators $ Simple (removeFromSubFields subFields removeLabel)
      else Field (FieldNum fieldNum) indicators subFields

removeFromSubFields:: SubFields -> String -> [(String, String)]
removeFromSubFields (Simple []) _ = []
removeFromSubFields (Simple (x:xs)) removeLabel = if removeHelper x removeLabel then xs else x : removeFromSubFields (Simple xs) removeLabel

removeHelper:: (String, String) -> String -> Bool
removeHelper (label, content) removeLabel = label == removeLabel

----------------------------------------------------------------------------------------

searchLibrary:: [Unimarc] -> String -> String -> String
searchLibrary [] _ _ = "Not Found"
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
                                          writeFile "zapisi.txt" $ showLibrary $ appendToLibrary library fieldNum (label, content) (read unimarcIdx)

                              "remove" -> do
                                           putStr "FieldNum: "
                                           fieldNum <- getLine
                                           putStr "Label: "
                                           label <- getLine
                                           putStr "Unimarc index: "
                                           unimarcIdx <- getLine
                                           writeFile "zapisi.txt" $ showLibrary $ removeFromLibrary library fieldNum label (read unimarcIdx)
                              otherwise -> putStr "error"