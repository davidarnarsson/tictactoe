
module Util where 
  import Data.Char 
  import Text.ParserCombinators.Parsec
  import Control.Applicative

  digitsToInt :: String -> Int
  digitsToInt str = sum [ digitToInt c * 10^i | (c,i) <- str `zip` [len, len - 1..0]]
    where 
      len = length str -1 


  getMove :: IO (Either ParseError (Int, Int))
  getMove = do
    putStrLn "Enter a move: "
    move <- getLine
    return $ parse parseM "" move 
  
  parseM :: GenParser Char st (Int, Int)
  parseM = do 
    x <- spaces *> many1 alphaNum
    y <- spaces *> many1 alphaNum 

    return (digitsToInt x, digitsToInt y)