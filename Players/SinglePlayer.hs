module Players.SinglePlayer where
  import Players.Player 
  import TicTacToe as T hiding (win)
  import Util
  
  data SinglePlayer = SinglePlayer String Token
    deriving Show

  instance Player SinglePlayer where 
    iToken (SinglePlayer _ t) = t
    iName (SinglePlayer n _) = n
    iMove = move
    iChooseSize = chooseSize
    iWin = win
    iLose = lose
    iReceiveSize = receiveSize
    
  move :: SinglePlayer -> TicTacToe -> IO (Int, Int)
  move pp = playerMove 

  playerMove :: TicTacToe -> IO (Int, Int)
  playerMove state = do 
    mv <- getMove
    case mv of 
      Left e -> rc
      Right oMove -> 
                  if T.isLegal state oMove && T.isBlank state oMove 
                    then return oMove
                  else rc
    where 
      rc = do
        putStrLn "Illegal move, please try again!"
        playerMove state 


  receiveSize :: SinglePlayer -> Int -> IO ()
  receiveSize sp sz = putStrLn $ "Opponent chose size " ++ show sz ++ "."

  chooseSize :: SinglePlayer -> IO Int
  chooseSize p = do
    putStrLn "Please choose the size of the board:    "
    n <- getInt 
    if n <= 0 then do
      putStrLn "Illegal number, must be greater than 0"
      chooseSize p
    else
      return n
      
  win :: SinglePlayer -> TicTacToe -> IO ()
  win (SinglePlayer n t) ttt = putStrLn $ n ++ " wins!"

  lose :: SinglePlayer -> TicTacToe -> IO ()
  lose (SinglePlayer n t) ttt = putStrLn $ n ++ " lost!"

  generatePlayer :: Token -> IO SinglePlayer
  generatePlayer t = do 
    putStr $ "Enter the name of player " ++ show t
    n <- getString
    return (SinglePlayer n t)
