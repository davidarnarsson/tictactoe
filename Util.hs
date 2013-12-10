module Util where 
  import Data.Char
  import Text.ParserCombinators.Parsec
  import Control.Applicative hiding (many)

  -- Converts a string of n digits to the corresponding
  -- int of length n
  digitsToInt :: String -> Int
  digitsToInt str = sum [ digitToInt c * 10^i | (c,i) <- str `zip` [len, len - 1..0]]
    where 
      len = length str -1 

  -- Parses a move made by a player
  getMove :: IO (Either ParseError (Int, Int))
  getMove = do
      putStr "Enter a move: "
      move <- getLine
      return $ parse parseM "" move
        

  getString :: IO String
  getString = getLine

  -- parser for the move 
  parseM :: GenParser Char st (Int, Int)
  parseM = do 
    x <- spaces *> many1 digit
    y <- spaces *> many1 digit 

    return (digitsToInt x, digitsToInt y)

  -- gets a single int from the command line
  getInt :: IO Int
  getInt = do
    i <- getLine
    case parse parseGetInt "" i of 
      (Left _) -> do
        putStrLn "Illegal choice, try again"
        getInt
      (Right n) -> do
        return $ digitsToInt n

  parseGetInt :: GenParser Char st String
  parseGetInt = many1 digit
    
