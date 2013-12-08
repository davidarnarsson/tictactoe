module Main where  
  import Network.Socket (withSocketsDo)
  import Control.Exception 
  import Game (startLocalGame, startNetworkGame, joinNetworkGame, startAIGame)

  menuLoop :: IO ()
  menuLoop = do 
    putStrLn "-------"
    putStrLn "Choose: "
    putStrLn "1. Start a LAN game"
    putStrLn "2. Connect to a LAN game"
    putStrLn "3. Play against AI"
    putStrLn "4. Play 1v1 locally"
    putStrLn "5. Quit"

    opt <- fmap read getLine 

    case opt of 
      1 -> do putStrLn "Starting a LAN game..."
              startNetworkGame
              menuLoop
      2 -> do putStrLn "Joining a LAN game..."
              joinNetworkGame
              menuLoop
      3 -> do startAIGame
              menuLoop
      4 -> do putStrLn "Creating local game"
              startLocalGame
              menuLoop
      5 -> do putStrLn "You quit!"
      _ -> do putStrLn "Invalid choice! Choose again!" 
              menuLoop
--lalala

  main :: IO () 
  main = withSocketsDo $ do 
    putStrLn "Tic Tac Toe v0.01"
    menuLoop
