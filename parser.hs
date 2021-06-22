import Text.Parsec
import System.Environment

newtype Unimarc = Unimarc [Field]
data Field = Field FieldNum Indicators SubFields deriving Show
newtype FieldNum = FieldNum String deriving Show
newtype Indicators = Indicators String deriving Show
data SubFields = Simple [(Char, String)] | Recurent [(Char, Field)] deriving Show

libraryParse :: Parsec String () [[Field]]
libraryParse = sepEndBy unimarcParse $ char '\n'

unimarcParse :: Parsec String () [Field]
unimarcParse = sepEndBy fieldParse $ char '\n'


-- fieldParse :: Parsec String () Field
-- fieldParse =    do
--                 fieldNum <- fieldNumParse
--                 spaces
--                 indicators <- indicatorsParse
--                 spaces
--                 char '['
--                 subfields <- simpleParse
--                 return $ Field fieldNum indicators $ Simple subfields

fieldParse :: Parsec String () Field
fieldParse =    do
                fieldNum <- fieldNumParse
                spaces
                indicators <- indicatorsParse
                spaces
                subfields <- manyTill recurentLabelContentParse $ oneOf "}"
                return $ Field fieldNum indicators $ Simple subfields

fieldNumParse :: Parsec String () FieldNum
fieldNumParse = do
                fieldNum <- many digit
                return $ FieldNum fieldNum

indicatorsParse :: Parsec String () Indicators
indicatorsParse =   do
                    indicator1 <- oneOf "#0123456789"
                    indicator2 <- oneOf "#0123456789"
                    return $ Indicators [indicator1, indicator2]

simpleParse :: Parsec String () [(Char, String)]
simpleParse = sepEndBy labelContentParse $ char '['

-- recurentParse :: Parsec String () Field
-- recurentParse =    do
--                           string "{{"
--                           fieldNum <- fieldNumParse
--                           spaces
--                           indicators <- indicatorsParse
--                           spaces
--                           secondarySubFields <- helperParse
--                           return Field $ fieldNum indicators $ Simple secondarySubFields

recurentLabelContentParse :: Parsec String () (Char, String)
recurentLabelContentParse =  do
                          label <- alphaNum 
                          spaces
                          content <- manyTill anyChar (string " a ")
                          return (label, content)


labelContentParse :: Parsec String () (Char, String)
labelContentParse = do
                    label <- labelParse
                    content <- many (noneOf "[\n")
                    return (label, content)

labelParse :: Parsec String () Char
labelParse =    do
                label <- alphaNum
                char ']'
                return label

main :: IO ()
main = do (input:output:[]) <- getArgs
          cnts <- readFile input
          case (runParser libraryParse () input cnts) of
            Left err -> print err
            Right library -> writeFile output . show $ library