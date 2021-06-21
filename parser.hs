import Text.Parsec

data Field = Field FieldNum Indicators SubFields deriving Show
newtype FieldNum = FieldNum String deriving Show
newtype Indicators = Indicators String deriving Show
newtype SubFields = SubFields [(Char, String)] deriving Show

unimarcParse :: Parsec String () Field
unimarcParse =  do
                fieldNum <- fieldNumParse
                spaces
                indicators <- indicatorsParse
                spaces
                char '['
                subfields <- subfieldsParse
                return $ Field fieldNum indicators (SubFields subfields)

fieldNumParse :: Parsec String () FieldNum
fieldNumParse = do
                fieldNum <- many digit
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
                    content <- many (noneOf "[")
                    return (label, content)

labelParse :: Parsec String () Char
labelParse =    do
                label <- alphaNum
                char ']'
                return label