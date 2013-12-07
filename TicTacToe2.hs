{- 
TO DO:

Nostra adeins vid kodann: Fjarlaegja tvitekningar, laga syntax, gera meira elegant, o.fl.
Seta upp data structures fyrir fleiri hluti (t.d. í stað 'X' og 'O' chars þá data Player = X | O)
Bua til quit function til ad haetta i leiknum
Bua til functions sem lata tolvu spila a moti spilara
Fixa textaframsetningu leiksins
Birta i terminal leyfilega leiki ef spilari gefur ologlegt input (random string, ologlegan leik, ..)
-}

module TicTacToe where
import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
import System.Random

data TicTacToe = TicTacToe { rows :: [[Maybe Char]] }
 deriving (Show, Eq)

type Pos = (Int,Int)

testBoard = TicTacToe [[Nothing,Just 'X',Nothing,Just 'X'],
                       [Just 'O',Just 'X',Just 'X',Just 'X'],
                       [Nothing,Just 'X',Just 'X',Just 'X'],
                       [Nothing,Nothing,Nothing,Just 'X']]

testBoard2 = TicTacToe [[Just 'X',Just 'O',Just 'X'],
                       [Just 'X',Just 'O',Just 'X'],
                       [Just 'X',Just 'O',Just 'X']]

-- Resets the game
emptyBoard :: Int -> TicTacToe
emptyBoard dim = TicTacToe [ [ Nothing | _ <- [1..dim] ] | _ <- [1..dim] ]

-- Prints out the current game position
printGame :: TicTacToe -> IO ()
printGame (TicTacToe rs) =
  putStr $ "\n" ++ concat [ [ char c | c <- r ] ++"\n" | r <- rs]
    where
      char Nothing = '.'
      char (Just c) = c

-- Checks whether a slot is blank or not
isBlank :: TicTacToe -> Pos -> Bool
isBlank gameState pos = if isNothing slot then True else False
  where slot = (rows gameState) !! (fst pos) !! (snd pos)

-- Determines whether the game is drawn or not
isDrawn :: TicTacToe -> Bool
isDrawn gameState =  all (==True) [isJust x | x <- concat $ rows gameState]

-- Determines whether the game has been won by a player or not
win :: TicTacToe -> (Int, Int) -> Char -> Bool
win gameState currentMove currPlayer = row || col || dia1 || dia2
  where
    bsize = length $ rows gameState
    row = bsize == length [a | (Just a) <- 
            filter (==Just currPlayer) (rows gameState !! (fst currentMove))]
    col = bsize == length [a | (Just a) <- 
            filter (==Just currPlayer) (transpose (rows gameState) !! (snd currentMove))]
    dia1 = bsize == length [a | (Just a) <- 
            filter (==Just currPlayer) (diag (rows gameState))]
    dia2 = bsize == length [a | (Just a) <- 
            filter (==Just currPlayer) (diag (reverse (rows gameState)))]

-- Gets the diagonal line
diag :: [[t]] -> [t]
diag xs = [xs!!n!!n | n <- [0..length xs-1]]

-- Updates a list if given a tuple which has: a new value and its index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) list (index, value) = fixList list (index, value) 0
  where
    fixList [] (_, _) _ = []
    fixList list (index, value) current | current == index = [value] ++ fixList (drop 1 list) (index, value) (current+1)
                                        | otherwise = take 1 list ++ fixList (drop 1 list) (index, value) (current+1)

-- Updates a TicTacToe if given a new value and a position for that value
update :: TicTacToe -> Pos -> Maybe Char -> TicTacToe
update (TicTacToe tic) p c = TicTacToe $ rot tic p c
  where 
    rot (r:rr) (0, y) c = (r !!= (y, c)):rr
    rot (r:rr) (x, y) c = r:rot rr (x-1, y) c

-- Checks whether a move is outside of the length of the TicTacToe
isLegal :: TicTacToe -> Pos -> Bool
isLegal game pos = if row > bsize || col > bsize || row < 0 || col < 0
  then False
  else True
    where
      bsize = length (rows game)-1
      row = fst pos
      col = snd pos

-- Gives all the blanks, if given a TicTacToe and its dimensions
blanks :: Int -> TicTacToe -> [Pos]
blanks dim (TicTacToe rr) =  [a | (a, b) <- p, isNothing b ] -- pick out the Nothings
  where 
    -- zip together the cartesian product of x and y and all the cells in the Sudoku
    idxs = [(x,y) | x <- [0..dim-1], y <- [0..dim-1]] 
    rows = concat rr
    p = idxs `zip` rows


--------------------------------------------------------------------------
-- IO Stuff:
--------------------------------------------------------------------------

-- Computer play
compPlay :: Int -> TicTacToe -> IO Pos
compPlay dim gameState = do
  let availMoves = blanks dim gameState
  let len = length availMoves
  randNum <- randomRIO(0,len-1)
  let move = availMoves!!randNum
  return move

-- The Game loop!
gameLoop :: TicTacToe -> Char -> IO()
gameLoop prevMove currPlayer = do
  putStr $ "\nPlayer " ++ (show currPlayer) ++ ": "
  userInput <- getLine
  let currentMove = (digitToInt $ userInput!!0, digitToInt $ userInput!!1)
  if isLegal prevMove currentMove then do
    if isBlank prevMove currentMove then do 
      let gameState = update prevMove currentMove (Just currPlayer)
      printGame gameState
      if isDrawn gameState then gameDrawn (length $ rows gameState)
      else if win gameState currentMove currPlayer
        then do gameWon currPlayer (length $ rows gameState)
        else do
          if currPlayer == 'X' 
          then gameLoop gameState 'O'
          else gameLoop gameState 'X'
    else do 
      putStrLn "\nThat position is already taken! Please select another one."
      gameLoop prevMove currPlayer
  else do
    putStrLn "\nThat move is illegal! Please select another move."
    gameLoop prevMove currPlayer

-- Computer play loop
compLoop prevMove currPlayer = do
  if isDrawn prevMove then gameDrawn (length $ rows prevMove)
  else if currPlayer == 'O'
    then do
      nextMove <- compPlay (length $ rows prevMove) prevMove
      let gameState = update prevMove nextMove (Just currPlayer)
      putStrLn $ "\nThe computer has played its move!"
      printGame gameState
      if win gameState nextMove currPlayer
        then do gameWon currPlayer (length $ rows gameState)
        else do compLoop gameState 'X'
    else do
      putStr $ "\nPlayer " ++ (show currPlayer) ++ ": "
      userInput <- getLine
      let currentMove = (digitToInt $ userInput!!0, digitToInt $ userInput!!1)
      if isLegal prevMove currentMove then do
        if isBlank prevMove currentMove then do 
          let gameState = update prevMove currentMove (Just currPlayer)
          printGame gameState
          if win gameState currentMove currPlayer
            then do gameWon currPlayer (length $ rows gameState)
            else do compLoop gameState 'O'
        else do 
          putStrLn "\nThat position is already taken! Please select another one."
          gameLoop prevMove currPlayer
      else do
      putStrLn "\nThat move is illegal! Please select another move."
      gameLoop prevMove currPlayer


-- Play again function
playAgain dim = do
  putStrLn "\nPlay again? [y]\n"
  yn <- getChar
  if not (toLower yn == 'y') then do
    putStrLn "\n\nThanks for playing!\n"
    return()
     else do
      putStrLn "\n\nHere's a new game, just for you!"
      printGame $ emptyBoard dim
      gameLoop (emptyBoard dim) 'X'

-- Game drawn function
gameDrawn dim = do 
  putStrLn "\nGame drawn."
  playAgain dim

-- Game won function
gameWon currPlayer dim = do
  putStrLn $ "\nPlayer " ++ (show currPlayer) ++ " won!" 
  playAgain dim    

-- Game Initialization
initialize :: IO()
initialize = do
  putStrLn "\nPlease enter a number between 3 and 9\n"
  dimensions <- getChar
  if digitToInt dimensions < 3 
    then do
      putStrLn "Invalid number!" 
      initialize
    else do 
    let newGame = emptyBoard $ digitToInt dimensions
    putStrLn ""
    printGame $ emptyBoard $ digitToInt dimensions
    gameLoop newGame 'X'

-- Main function
main :: IO()
main = initialize



