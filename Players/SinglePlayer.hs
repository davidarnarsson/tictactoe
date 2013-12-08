module Players.SinglePlayer where
  import Players.Player 
  import TicTacToe as T hiding (win)
  import Util
  
  data SinglePlayer = SinglePlayer Token
    deriving Show

  instance Player SinglePlayer where 
    iToken (SinglePlayer t) = t
    iName _ = "You"
    iMove = move
    iChooseSize = chooseSize
    iWin = win
    iLose = lose
    iReceiveSize = receiveSize
    iOpponentMove = opponentMove

  getPlayer :: Token -> SinglePlayer 
  getPlayer t = SinglePlayer t
    
  move :: SinglePlayer -> TicTacToe -> IO (Int, Int)
  move pp ttt = do
    T.printGame ttt
    playerMove ttt 

  playerMove :: TicTacToe -> IO ((Int, Int))
  playerMove state = do 
    mv <- getMove
    case mv of 
      (Left e) -> do rc
      (Right oMove) -> do
                  if (T.isLegal state oMove) && (T.isBlank state oMove) then do
                    return oMove
                  else rc
    where 
      rc = do
        putStrLn "Illegal move, please try again!"
        playerMove state 


  receiveSize :: SinglePlayer -> Int -> IO ()
  receiveSize sp sz = do 
    putStrLn $ "Opponent chose size " ++ show sz ++ "."

  chooseSize :: SinglePlayer -> IO Int
  chooseSize p = do
    putStrLn "Please choose the size of the board: "
    n <- fmap digitsToInt getLine
    if n <= 0 then do
      putStrLn "Illegal number, must be greater than 0"
      chooseSize p
    else
      return n
      
  win :: SinglePlayer -> TicTacToe -> IO ()
  win lp ttt = do
    putStrLn "Congrats, you won!"

  lose :: SinglePlayer -> TicTacToe -> IO ()
  lose lp ttt = do 
    putStrLn "You lost!"

  opponentMove :: SinglePlayer -> TicTacToe -> (Int, Int) -> IO ()
  opponentMove lp ttt (x, y) = do
    putStrLn $ "The opponent played " ++ show x ++ ", " ++ show y
    T.printGame ttt
    return ()
