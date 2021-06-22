import Text.Parsec
import System.Environment

newtype Unimarc = Unimarc [Field] deriving Show
data Field = Field FieldNum Indicators SubFields deriving Show
newtype FieldNum = FieldNum String deriving Show
newtype Indicators = Indicators String deriving Show
newtype SubFields = SubFields [(Char, String)] deriving Show

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
                return $ Field fieldNum indicators (SubFields subfields)

fieldNumParse :: Parsec String () FieldNum
fieldNumParse = do
                fieldNum <- count 3 digit
                return $ FieldNum fieldNum

indicatorsParse :: Parsec String () Indicators
indicatorsParse =   do
                    indicator1 <- oneOf "#0123456789"
                    indicator2 <- oneOf "#0123456789"
                    return $ Indicators [indicator1, indicator2]

subfieldsParse :: Parsec String () [(Char, String)]
subfieldsParse = sepEndBy labelContentParse $ char '['

labelContentParse :: Parsec String () (Char, String)
labelContentParse = do
                    label <- labelParse
                    content <- many1 (noneOf "[\n")
                    return (label, content)

labelParse :: Parsec String () Char
labelParse =    do
                label <- alphaNum
                char ']'
                return label

main :: IO ()
main = do (input:output:[]) <- getArgs
          cnts <- readFile input
          case (runParser libraryParse () input cnts) of  -- parse
            Left err -> putStrLn . show $ err
            Right rss -> writeFile output . show $ rss  -- putStrLn . show $ rss