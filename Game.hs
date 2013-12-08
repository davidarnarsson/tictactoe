
module Game where 
  import TicTacToe (TicTacToe(..), emptyBoard, update, isDrawn, win)
  import qualified Players.Player as P 
  --import qualified Players.SinglePlayer as SP 
  --import qualified Players.LocalPlayer as LP 
  --import qualified Players.RemotePlayer as RP 
  import Net.Server as NS (create)
  import Net.Client as CS (join)
  import System.IO (hClose)
  u = undefined


  startLocalGame = u

  startAIGame = u

  -- Starts a network game 
  startNetworkGame :: IO ()
  startNetworkGame = do 
    putStrLn "Starting network game..."
    (lp, rp, hdl) <- NS.create
    initGameLoop lp rp
    hClose hdl

  -- joins a network game
  joinNetworkGame :: String -> IO ()
  joinNetworkGame host = do 
    putStrLn "Joining network game..."
    (lp, rp, hdl) <- CS.join host
    initGameLoop rp lp
    hClose hdl


  initGameLoop :: (P.Player a, P.Player b) => a -> b -> IO ()
  initGameLoop p1 p2 = do 
    mv <- P.iChooseSize p1  
    P.iReceiveSize p2 mv
    let ttt = emptyBoard mv

    gameLoop ttt p1 p2
  
  gameLoop :: (P.Player a, P.Player b) => TicTacToe -> a -> b -> IO()
  gameLoop state playerA playerB = do
    putStr $ "\nPlayer " ++ (P.iName playerA) ++ ": "
    
    mv <- P.iMove playerA state
    let newState = update state mv (Just $ P.iToken playerA) 

    if isDrawn newState then do
      putStrLn "The game has drawn!"
    else if win newState mv $ P.iToken playerA then do
      P.iWin playerA newState
      P.iLose playerB newState
    else 
      gameLoop newState playerB playerA
