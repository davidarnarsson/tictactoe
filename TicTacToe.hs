module TicTacToe where
  import Test.QuickCheck
  import Data.Char
  import Data.List
  import Data.Maybe
  import Util
  import System.IO
  
  data TicTacToe = TicTacToe { rows :: [[Maybe Token]] }
   deriving (Show, Eq)

  data Token = X | O 
    deriving (Eq, Show)

  type Pos = (Int,Int)

  -- Resets the game
  emptyBoard :: Int -> TicTacToe
  emptyBoard dim = TicTacToe [ [ Nothing | _ <- [1..dim] ] | _ <- [1..dim] ]

  -- Property for emptyBoard. Running quickCheck(prop_emptyBoard) returns the following:
  -- +++ OK, passed 100 tests.
  prop_emptyBoard n = all (isNothing) [ x | x  <- (concat $ rows $ emptyBoard n')]
    where n' = n `mod` 1000 

  -- Checks whether a slot is blank or not
  isBlank :: TicTacToe -> Pos -> Bool
  isBlank gameState pos = if isNothing slot then True else False
    where slot = (rows gameState) !! (fst pos) !! (snd pos)

  -- Determines whether the game is drawn or not
  isDrawn :: TicTacToe -> Bool
  isDrawn gameState =  all (==True) [isJust x | x <- concat $ rows gameState]

  -- Determines whether the game has been won by a player or not
  win :: TicTacToe -> (Int, Int) -> Token -> Bool
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
  update :: TicTacToe -> Pos -> Maybe Token -> TicTacToe
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
  blanks :: TicTacToe -> [Pos]
  blanks (TicTacToe rr) =  [a | (a, b) <- p, isNothing b ] -- pick out the Nothings
    where 
      -- zip together the cartesian product of x and y and all the cells in the TicTacToe
      idxs = [(x,y) | x <- [0..len], y <- [0..len]] 
      rows = concat rr
      p = idxs `zip` rows
      len = length rr - 1
