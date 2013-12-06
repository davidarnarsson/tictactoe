

module Net.Client where 
  import Net.Communication (send, connect, receive, hClose)
  import Net.Protocol (Message(..), parse, serialize)
  import System.IO (Handle(..))
  import Network (HostName)
  import Control.Exception
  import TicTacToe (isLegal, emptyBoard, isBlank, update, win, printGame, isDrawn, playerMove)
  import Util (getMove)

  join :: String -> IO (Handle)
  join host = do 
    putStrLn $ "Joining host " ++ host ++ " ..."
    connect host 2222 
    
  
  startClientGame :: HostName -> IO () 
  startClientGame host = do 
    putStrLn "What's your name?"
    oPlayer <- getLine 
    sHdl <- join host 

    (Hello oPlayer) `send` sHdl
    resp <- receive sHdl

    putStrLn $ "Received: " ++ show resp 
    
    (Size dim) <- receive sHdl

    let newGame = emptyBoard dim
    
    loop newGame sHdl

    return ()
  
  loop state hdl = do 
    printGame state

    (Move move) <- receive hdl    
    putStrLn "Player X moves: "
    let newState = update state move (Just 'X')
    printGame newState
    if isDrawn newState then do
      putStrLn "Game drawn"
    else if win newState move 'X' then do
      putStrLn "You loooost!"
    else do
      oMove <- playerMove newState

      (Move oMove) `send` hdl

      let newerState = update newState oMove (Just 'O')
      printGame newerState
        
      if isDrawn newerState then do
        putStrLn "Game drawn"
      else if win newerState oMove 'O' then do
        putStrLn "You won! Grats!"
      else 
        loop newerState hdl

