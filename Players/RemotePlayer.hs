
module Players.RemotePlayer where
  import Players.Player as P (Player(..))
  import TicTacToe (TicTacToe(..), Token(..))
  import Net.Protocol (Message(..))
  import System.IO
  import Net.Communication

  -- Remote player (Name, X or O, Network Pipe)
  data RemotePlayer = RemotePlayer String Token Handle 
    deriving (Show)

  instance Player RemotePlayer where 
    iToken (RemotePlayer _ t _) = t
    iName (RemotePlayer n _ _)  = n
    iMove = move
    iChooseSize = chooseSize
    iReceiveSize = receiveSize
    iWin = win
    iLose = lose
    iOpponentMove = opponentMove


  move :: RemotePlayer -> TicTacToe -> IO (Int, Int)
  move (RemotePlayer n t hdl) _ = do
    (Move oMove) <- receive hdl
    putStrLn $ n ++ " placed " ++ show t ++ " in position " ++ show oMove
    return oMove

  chooseSize :: RemotePlayer -> IO (Int)
  chooseSize (RemotePlayer n t hdl) = do 
    (Size sz) <- receive hdl
    putStrLn $ n ++ " chose size " ++ show sz
    return sz

  receiveSize :: RemotePlayer -> Int -> IO ()
  receiveSize (RemotePlayer n t hdl) sz = do
    (Size sz) `send` hdl

  win :: RemotePlayer -> TicTacToe -> IO ()
  win = undefined

  lose :: RemotePlayer -> TicTacToe -> IO ()
  lose = win

  opponentMove :: RemotePlayer -> TicTacToe -> (Int, Int) -> IO ()
  opponentMove (RemotePlayer n t hdl) ttt mv = undefined
