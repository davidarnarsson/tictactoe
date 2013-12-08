

module Net.Client where 
  import Net.Communication (send, connect, receive)
  import Net.Protocol (Message(..), parse, serialize)
  import System.IO (Handle(..), hClose, hFlush)
  import Network (HostName)
  import Players.LocalPlayer
  import Players.RemotePlayer
  import TicTacToe (TicTacToe(..), Token(..))
  import Util (getMove)

  join :: HostName -> IO (LocalPlayer, RemotePlayer, Handle)
  join host = do 
    hdl <- connect host 2345 
    putStrLn "What's your name?"
    oPlayer <- getLine 

    (Hello oPlayer) `send` hdl
    hFlush hdl
    (Hello xPlayer) <- receive hdl

    return ((LocalPlayer oPlayer O hdl), (RemotePlayer xPlayer X hdl), hdl)
  
  
