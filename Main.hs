module Main where  
  import Network.Socket (withSocketsDo)
  import Control.Exception 
  import Game (startLocalGame, startNetworkGame, joinNetworkGame, startAIGame)
  import Util (getInt)
  import Data.Char

  menuLoop :: IO ()
  menuLoop = do 
    putStrLn "-------"
    putStrLn "Choose: "
    putStrLn "1. Start a LAN game"
    putStrLn "2. Connect to a LAN game"
    putStrLn "3. Play against AI"
    putStrLn "4. Play 1v1 locally"
    putStrLn "5. Quit\n"

    opt2 <- getChar
    let opt = digitToInt $ opt2

    case opt of 
      1 -> do putStrLn "\nStarting a LAN game..."
              startNetworkGame
              menuLoop
      2 -> do putStrLn "\nJoining a LAN game..."
              joinNetworkGame
              menuLoop
      3 -> do startAIGame
              menuLoop
      4 -> do putStrLn "\nCreating local game"
              startLocalGame
              menuLoop
      5 -> do putStrLn "\nYou quit!"
      _ -> do putStrLn "\nInvalid choice! Choose again!" 
              menuLoop
--lalala

  main :: IO () 
  main = withSocketsDo $ do 
    putStrLn "\nTic Tac Toe v0.01"
    menuLoop
