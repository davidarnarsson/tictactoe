

module Players.AIPlayer where 
  import Players.Player as P 
  import TicTacToe (TicTacToe(..), Token(..), blanks)
  import System.Random 

  data AIPlayer = AIPlayer Token

  instance Player AIPlayer where 
    iToken (AIPlayer t) = t
    iName (AIPlayer _) = "AI"
    iMove = move
    iChooseSize = chooseSize
    iReceiveSize = receiveSize
    iWin = win
    iLose = lose


  move :: AIPlayer -> TicTacToe -> IO (Int, Int)
  move p gameState = do
    let availMoves = blanks gameState
    let len = length availMoves
    randNum <- randomRIO(0,len-1)
    let move = availMoves!!randNum
    return move

  chooseSize :: AIPlayer -> IO Int
  chooseSize p = randomRIO(3, 6)

  receiveSize :: AIPlayer -> Int -> IO ()
  receiveSize p i = return ()

  win :: AIPlayer -> TicTacToe -> IO ()
  win p t = return ()

  lose :: AIPlayer -> TicTacToe -> IO ()
  lose p t = return ()
