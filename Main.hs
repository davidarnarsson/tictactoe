module Main where 
  import Communication (openHandle, closeHandle, write)

  menuLoop :: IO ()
  menuLoop = do 
    putStrLn "-------"
    putStrLn "Choose: "
    putStrLn "1. Play a LAN game"
    putStrLn "2. Play against AI"
    putStrLn "3. Quit"

    opt <- fmap read getLine 

    case opt of 
      1 -> do putStrLn "You played a lan game."
              menuLoop
      2 -> do putStrLn "You played against AI"
              menuLoop
      3 -> putStrLn "You quit!"

  
  runLanGame :: IO () 
  runLanGame = do 
    hdl <- openHandle 2222
    write hdl "hello!"
    closeHandle hdl
    

  main :: IO () 
  main = do 
    putStrLn "Tic Tac Toe v0.01"
    menuLoop
