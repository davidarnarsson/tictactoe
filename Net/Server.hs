

module Net.Server where 
  import Net.Communication (send, open, accept, receive, close)
  import Net.Protocol (Message(..), parse, serialize)
  import System.IO (Handle(..), hClose)
  import Network (HostName)
  import Control.Exception
  import Util (digitsToInt, getMove)
  import TicTacToe (isLegal, emptyBoard, isBlank, update, win, printGame, isDrawn, playerMove)

  startServerGame :: IO ()
  startServerGame = do 
    putStrLn "What is your name?"
    xPlayer <- getLine

    putStrLn "Enter dimensions: "
    dim <- fmap digitsToInt getLine 

    sock <- open 2222
    accept sock >>= playerJoined xPlayer dim >>= hClose 
    close sock

    startServerGame


  playerJoined xPlayer dim conn@(hdl, _,_) = do
    putStrLn "Player joined!" 

    (Hello oPlayer) <- receive hdl

    putStrLn $ oPlayer ++ " connected!"

    (Hello xPlayer) `send` hdl
    (Size dim) `send` hdl

    let newGame = emptyBoard dim

    loop newGame hdl
    return hdl


  loop state hdl = do 
    printGame state
    move <- playerMove state    
    let newState = update state move (Just 'X')
    printGame newState

    (Move move) `send` hdl

    if isDrawn newState then do
      putStrLn "Game drawn"
    else if win newState move 'X' then do
      putStrLn "You won!"
    else do
      (Move oMove) <- receive hdl
      putStrLn "Player O moves: "
      let newerState =  update newState oMove (Just 'O')
      printGame newerState
      
      if isDrawn newerState then do
        putStrLn "Game drawn"
      else if win newerState oMove 'O' then do
        putStrLn "You loooooost!"
      else 
        loop newerState hdl
  