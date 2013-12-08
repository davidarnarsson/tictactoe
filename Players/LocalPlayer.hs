

module Players.LocalPlayer where 
  import Players.Player 
  import TicTacToe hiding (win)
  import System.IO
  import Net.Protocol
  import Net.Communication
  import qualified Players.SinglePlayer as SP 

  -- Local player (name, X or O, Network pipe)
  data LocalPlayer = LocalPlayer String Token Handle 
    deriving (Show)

  instance Player LocalPlayer where 
    iToken (LocalPlayer n t hdl) = t
    iName (LocalPlayer n _ _) = n
    iMove = move
    iChooseSize = chooseSize
    iReceiveSize = receiveSize
    iWin = win
    iLose = lose
    iOpponentMove = opponentMove


  move :: LocalPlayer -> TicTacToe -> IO (Int, Int)
  move lp@(LocalPlayer _ t hdl) ttt = do
    mv <- SP.move (SP.SinglePlayer t) ttt
    (Move mv) `send` hdl
    hFlush hdl
    return mv

  chooseSize :: LocalPlayer -> IO (Int)
  chooseSize (LocalPlayer _ t hdl) = do 
    sz <- SP.chooseSize (SP.SinglePlayer t)
    return sz

  receiveSize :: LocalPlayer -> Int -> IO ()
  receiveSize lp sz = do 
    putStrLn $ "A size of " ++ show sz ++ " has been chosen."

  win :: LocalPlayer -> TicTacToe -> IO ()
  win lp ttt = execSp lp ttt SP.win
    
  lose :: LocalPlayer -> TicTacToe -> IO ()
  lose lp ttt = execSp lp ttt SP.lose
    
  execSp :: LocalPlayer -> TicTacToe -> (SP.SinglePlayer -> TicTacToe -> IO ()) -> IO ()
  execSp (LocalPlayer _ t hdl) ttt fn = do fn (SP.SinglePlayer t) ttt

  opponentMove :: LocalPlayer -> TicTacToe -> (Int, Int) -> IO ()
  opponentMove (LocalPlayer _ t hdl) ttt mv = do 
    SP.opponentMove (SP.SinglePlayer t) ttt mv 
