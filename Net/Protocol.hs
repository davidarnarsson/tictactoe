
--http://book.realworldhaskell.org/read/using-parsec.html
module Net.Protocol where 
  import Control.Exception
  import Text.ParserCombinators.Parsec as P
  import Control.Applicative
  import Data.Char
  import Util (digitsToInt)


  data Message = Move (Int, Int) | Hello String | Size Int | Goodbye 
    deriving (Eq, Show)


  parseMove :: GenParser Char st (Int, Int)
  parseMove = do
    string "Move" *> spaces *> char '('
    x <- parseInt
    spaces *> char ',' *> spaces
    y <- parseInt
    char ')'
    return (x,y)

  parseInt :: GenParser Char st Int
  parseInt = do
    x <- many1 alphaNum
    return $ digitsToInt x

  parseSize :: GenParser Char st Int
  parseSize = fmap digitsToInt $ (string "Size" >> space) *> many1 alphaNum

  parseHello :: GenParser Char st String
  parseHello = (string "Hello" >> space) *> many1 anyChar
  
  parseGoodbye :: GenParser Char st Message
  parseGoodbye = string "Goodbye" *> return Goodbye

  parse :: String -> Either ParseError Message
  parse str = P.parse (do {
                            Move    <$> parseMove 
                      P.<|> Hello   <$> parseHello
                      P.<|> Size    <$> parseSize
                      P.<|> parseGoodbye
                      }) "" str  
  
  serialize :: Message -> String 
  serialize = show

