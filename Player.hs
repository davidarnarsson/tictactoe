
module Player where 
  import TicTacToe 

  data Player {
    iMove :: TicTacToe -> (Int, Int)
    iChooseSize :: Int
  }