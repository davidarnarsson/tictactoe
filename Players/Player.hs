
module Players.Player where 
  import TicTacToe

  class Player a where 
    iToken :: a -> Token
    iName :: a -> String
    iMove :: a -> TicTacToe -> IO (Int, Int)
    iChooseSize :: a -> IO Int
    iReceiveSize :: a -> Int -> IO ()
    iWin :: a -> TicTacToe -> IO () 
    iLose :: a -> TicTacToe -> IO ()     
    iOpponentMove :: a -> TicTacToe -> (Int, Int) -> IO ()


