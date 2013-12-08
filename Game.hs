
module Game where 
  import TicTacToe (TicTacToe(..), emptyBoard, update, isDrawn, win)
  import qualified Players.Player as P 
  import GameTypes.ServerGame as SG (create, cleanUp)
  import GameTypes.ClientGame as CG (create, cleanUp)
  import GameTypes.LocalGame as LG (create, cleanUp)
  import GameTypes.AIGame as AI (create, cleanUp)
  import System.IO (hClose, Handle(..))
  import qualified Control.Exception as E
  u = undefined

  -- starts a local game
  startLocalGame :: IO () 
  startLocalGame = do 
    putStrLn "Starting local game..."
    startGame LG.create LG.cleanUp

  -- starts an AI game
  startAIGame :: IO ()
  startAIGame = do 
    putStrLn "Starting AI game..."
    startGame AI.create AI.cleanUp

  -- Starts a network game 
  startNetworkGame :: IO ()
  startNetworkGame = do 
    putStrLn "Starting network game..."
    startGame SG.create SG.cleanUp
  
  -- joins a network game
  joinNetworkGame :: IO ()
  joinNetworkGame = do 
    putStrLn "Joining network game..."
    startGame CG.create CG.cleanUp

  
  startGame :: (P.Player a, P.Player b) => IO (a, b) -> ((a, b) -> IO()) -> IO ()
  startGame create cleanUp = do 
    players <- create
    E.catch (do { initGameLoop players; cleanUp players })
      (\(E.SomeException e) -> cleanUp players)
  

  initGameLoop :: (P.Player a, P.Player b) => (a, b) -> IO ()
  initGameLoop (p1,p2) = do 
    mv <- P.iChooseSize p1  
    P.iReceiveSize p2 mv
    let ttt = emptyBoard mv

    gameLoop ttt p1 p2
  
  gameLoop :: (P.Player a, P.Player b) => TicTacToe -> a -> b -> IO()
  gameLoop state playerA playerB = do
    putStr $ "\nPlayer " ++ (P.iName playerA) ++ ": "
    
    mv <- P.iMove playerA state
    let newState = update state mv (Just $ P.iToken playerA) 

    if win newState mv $ P.iToken playerA then do
      P.iWin playerA newState
      P.iLose playerB newState
    else if isDrawn newState then do
      putStrLn "The game has drawn!"
    else 
      gameLoop newState playerB playerA
