module Main where 
  import Communication (open, close, accept, handleConnection, connect)
  import Network.Socket (withSocketsDo)

  menuLoop :: IO ()
  menuLoop = do 
    putStrLn "-------"
    putStrLn "Choose: "
    putStrLn "1. Start a LAN game"
    putStrLn "2. Connect to a LAN game"
    putStrLn "3. Play against AI"
    putStrLn "4. Quit"

    opt <- fmap read getLine 

    case opt of 
      1 -> do putStrLn "Starting a LAN game..."
              startLanGame
              menuLoop
      2 -> do joinLanGame
              menuLoop
      3 -> do putStrLn "You played against AI!"
              menuLoop
      4 -> do putStrLn "You quit!"
      _ -> do putStrLn "Invalid choice! Choose again!" 
              menuLoop
--lalala

  joinLanGame :: IO ()
  joinLanGame = do
    putStrLn "What IP?"
    host <- getLine 
    hdl <- connect host 2222
    putStrLn "Joined lan game"
  
  startLanGame :: IO () 
  startLanGame = do 
    sock <- open 2222 
    accept sock >>= 
      (\ conn@(hdl, h, p) -> do 
        putStrLn $ "Connected: " ++ h ++ ":" ++ show p 
        return conn ) >>= handleConnection >> close sock


  main :: IO () 
  main = withSocketsDo $ do 
    putStrLn "Tic Tac Toe v0.01"
    menuLoop
