

module Players.LocalPlayer where 
  import Players.Player 
  import TicTacToe hiding (win)
  import System.IO
  import Net.Protocol
  import Net.Communication
  import Network.Socket hiding (send)
  import qualified Players.SinglePlayer as SP 

  -- Local player (name, X or O, Network pipe)
  data LocalPlayer = LocalPlayer String Token Handle (Maybe Socket)
    deriving (Show)

  instance Player LocalPlayer where 
    iToken (LocalPlayer _ t _ _) = t
    iName (LocalPlayer n _ _ _) = n
    iMove = move
    iChooseSize = chooseSize
    iReceiveSize = receiveSize
    iWin = win
    iLose = lose

  move :: LocalPlayer -> TicTacToe -> IO Pos
  move (LocalPlayer n t hdl _) ttt = do
    mv <- SP.move (SP.SinglePlayer n t) ttt
    Move mv `send` hdl
    return mv

  chooseSize :: LocalPlayer -> IO Int
  chooseSize (LocalPlayer n t _ _) = SP.chooseSize (SP.SinglePlayer n t)

  receiveSize :: LocalPlayer -> Int -> IO ()
  receiveSize _ sz = putStrLn $ "A size of " ++ show sz ++ " has been chosen."

  win :: LocalPlayer -> TicTacToe -> IO ()
  win (LocalPlayer n t hdl _) ttt = do 
    SP.win (SP.SinglePlayer n t) ttt
    hClose hdl
    
  lose :: LocalPlayer -> TicTacToe -> IO ()
  lose (LocalPlayer n t hdl _) ttt = do 
    SP.lose (SP.SinglePlayer n t) ttt
    hClose hdl
