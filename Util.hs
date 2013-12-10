-- http://hackage.haskell.org/package/haskeline-0.7.1.1/docs/System-Console-Haskeline.html
module Util where 
  import Data.Char 
  import System.Console.Haskeline
  import Text.ParserCombinators.Parsec
  import Data.Maybe
  import Control.Applicative hiding (many)

  -- Converts a string of n digits to the corresponding
  -- int of length n
  digitsToInt :: String -> Int
  digitsToInt str = sum [ digitToInt c * 10^i | (c,i) <- str `zip` [len, len - 1..0]]
    where 
      len = length str -1 

  -- Parses a move made by a player
  getMove :: IO (Either ParseError (Int, Int))
  getMove = runInputT defaultSettings loop
    where 
      loop :: InputT IO (Either ParseError (Int, Int))
      loop =  do
        outputStr "Enter a move"
        move <- getInputLine ": "
        case move of 
          Just input -> return $ parse parseM "" (fromJust move) 
          Nothing -> loop

  getString :: IO String
  getString = runInputT defaultSettings $ do { 
    x <- getInputLine ": " ; return $ fromJust x }

  -- parser for the move 
  parseM :: GenParser Char st (Int, Int)
  parseM = do 
    x <- spaces *> many1 digit
    y <- spaces *> many1 digit 

    return (digitsToInt x, digitsToInt y)

  -- gets a single int from the command line
  getInt :: IO Int
  getInt = runInputT defaultSettings loop 
    where 
      loop :: InputT IO Int
      loop = do
        i <- getInputLine ": "

        if isJust i then
          case parse parseGetInt "" (fromJust i) of 
            (Left e) -> rd
              
            (Right n) -> do
              outputStrLn  n
              return $ digitsToInt n
        else 
          rd
        where 
          rd = do
            outputStrLn "Illegal choice, try again"
            loop

  parseGetInt :: GenParser Char st String
  parseGetInt = many1 digit
    
