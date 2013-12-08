

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
    iToken (LocalPlayer _ t _) = t
    iName (LocalPlayer n _ _) = n
    iMove = move
    iChooseSize = chooseSize
    iReceiveSize = receiveSize
    iWin = win
    iLose = lose
    iOpponentMove = opponentMove


  move :: LocalPlayer -> TicTacToe -> IO (Int, Int)
  move lp@(LocalPlayer n t hdl) ttt = do
    mv <- SP.move (SP.SinglePlayer n t) ttt
    (Move mv) `send` hdl
    return mv

  chooseSize :: LocalPlayer -> IO (Int)
  chooseSize (LocalPlayer n t hdl) = do 
    sz <- SP.chooseSize (SP.SinglePlayer n t)
    return sz

  receiveSize :: LocalPlayer -> Int -> IO ()
  receiveSize lp sz = do 
    putStrLn $ "A size of " ++ show sz ++ " has been chosen."

  win :: LocalPlayer -> TicTacToe -> IO ()
  win lp@(LocalPlayer n t hdl) ttt = do 
    SP.win (SP.SinglePlayer n t) ttt
    hClose hdl
    
  lose :: LocalPlayer -> TicTacToe -> IO ()
  lose lp@(LocalPlayer n t hdl) ttt = do 
    SP.lose (SP.SinglePlayer n t) ttt
    hClose hdl
    
  opponentMove :: LocalPlayer -> TicTacToe -> (Int, Int) -> IO ()
  opponentMove (LocalPlayer n t hdl) ttt mv = do 
    SP.opponentMove (SP.SinglePlayer n t) ttt mv 
