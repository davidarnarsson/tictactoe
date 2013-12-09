
module Players.Player where 
  import TicTacToe

  class Player a where 
    iToken :: a -> Token
    iName :: a -> String
    iMove :: a -> TicTacToe -> IO Pos
    iChooseSize :: a -> IO Int
    iReceiveSize :: a -> Int -> IO ()
    iWin :: a -> TicTacToe -> IO () 
    iLose :: a -> TicTacToe -> IO ()     


