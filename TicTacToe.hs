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
  prop_emptyBoard n = all isNothing (concat $ rows $ emptyBoard n')
    where n' = n `mod` 1000 

  -- Checks whether a slot is blank or not
  isBlank :: TicTacToe -> Pos -> Bool
  isBlank gameState pos = isNothing slot
    where slot = rows gameState !! fst pos !! snd pos

  -- Determines whether the game is drawn or not
  isDrawn :: TicTacToe -> Bool
  isDrawn gameState =  and [isJust x | x <- concat $ rows gameState]

  -- Determines whether the game has been won by a player or not
  win :: TicTacToe -> (Int, Int) -> Token -> Bool
  win gameState currentMove currPlayer = row || col || dia1 || dia2
    where
      bsize = length $ rows gameState
      row = bsize == length [a | (Just a) <- 
              filter (==Just currPlayer) (rows gameState !! fst currentMove)]
      col = bsize == length [a | (Just a) <- 
              filter (==Just currPlayer) ((transpose $ rows gameState) !! snd currentMove)]
      dia1 = bsize == length [a | (Just a) <- 
              filter (==Just currPlayer) (diag (rows gameState))]
      dia2 = bsize == length [a | (Just a) <- 
              filter (==Just currPlayer) (diag (reverse (rows gameState)))]
      diag xs = [xs!!n!!n | n <- [0..length xs-1]]


  -- Checks whether a move is outside of the length of the TicTacToe
  isLegal :: TicTacToe -> Pos -> Bool
  isLegal game (col,row) = not (row > bsize || col > bsize || row < 0 || col < 0)
      where
        bsize = length (rows game)-1
        

 ---------------------------------------------------------------------------
 -- The code below this comment was adapted from lab 3 to suit the purposes
 -- of our project.
 ---------------------------------------------------------------------------

  -- Updates a list if given a tuple which has: a new value and its index
  (!!=) :: [a] -> (Int,a) -> [a]
  (!!=) list (index, value) = fixList list (index, value) 0
    where
      fixList [] (_, _) _ = []
      fixList list (index, value) current 
        | current == index = value : fixList (drop 1 list) (index, value) (current + 1)
        | otherwise = take 1 list ++ fixList (drop 1 list) (index, value) (current+1)

  data BoundedTupleArr = BTA { arr :: [Int], tpl :: (Int, Int) }
    deriving (Eq, Show)
 
 -- An instance for generating arbitrary BoundedTupleArrs to use
 -- to quickCheck prop_Operator
  instance Arbitrary BoundedTupleArr where
    arbitrary = do 
      arr <- arbitrary
      idx <- choose (0 , length arr - 1)
      inst <- arbitrary
      return $ BTA arr (idx, inst)
      
  -- The property checks whether the !!= operator successfully inserts value a 
  -- at index i according to the !! operator. Running quickCheck(prop_Operator) returns:
  -- +++ OK, passed 100 tests.
  prop_Operator ::  BoundedTupleArr -> Property
  prop_Operator (BTA xs (i, a)) = 
    xs /= [] ==> applied!!i == a
      where 
        applied = xs !!= (i, a)

  -- Updates a TicTacToe if given a new value and a position for that value
  update :: TicTacToe -> Pos -> Maybe Token -> TicTacToe
  update (TicTacToe tic) p c = TicTacToe $ rot tic p c
    where 
      rot (r:rr) (0, y) c = (r !!= (y, c)):rr
      rot (r:rr) (x, y) c = r:rot rr (x-1, y) c

 -- An instance for generating arbitrary Tokens
  instance Arbitrary Token where
    arbitrary = oneof [return O, return X]

  -- Checks whether the update function actually updates the given cell to the 
  -- given value. Running it gives the following: 
  --  +++ OK, passed 100 tests.
  prop_update :: TicTacToe -> Pos -> Maybe Token -> Bool
  prop_update (TicTacToe tic) (x,y) c = new!!x'!!y' == c
    where 
      (TicTacToe new) = update (TicTacToe tic) (x',y') c
      x' = x `mod` length tic
      y' = y `mod` length tic

  -- Gives all the blanks, if given a TicTacToe and its dimensions.
  blanks :: TicTacToe -> [Pos]
  blanks (TicTacToe rr) =  [a | (a, b) <- p, isNothing b ] -- pick out the Nothings
    where 
      -- zip together the cartesian product of x and y and all the cells in the TicTacToe
      idxs = [(x,y) | x <- [0..len], y <- [0..len]] 
      rows = concat rr
      p = idxs `zip` rows
      len = length rr - 1

  -- Generates an arbitrary cell in a TicTacToe
  cell :: Gen (Maybe Token)
  cell = frequency [ (8, return Nothing), 
                     (1, return (Just X)),
                     (1, return (Just O))
                   ]

  -- An instance for generating arbitrary TicTacToes
  instance Arbitrary TicTacToe where
    arbitrary =
      do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
         return (TicTacToe rows)

  -- Checks that all the positions that blanks returns are in fact blank.
  -- Output: +++ OK, passed 100 tests.
  prop_blanks :: TicTacToe -> Bool
  prop_blanks (TicTacToe rr) = and [ isNothing $ rr!!x!!y | (x,y) <- bs ]
    where 
      bs = blanks $ TicTacToe rr

