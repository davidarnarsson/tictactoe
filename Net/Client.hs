

module Net.Client where 
  import Net.Communication (send, connect, receive)
  import Net.Protocol (Message(..), parse, serialize)
  import System.IO (Handle(..), hClose)
  import Network (HostName)
  import Players.LocalPlayer
  import Players.RemotePlayer
  import TicTacToe (TicTacToe(..), Token(..))
  import Util (getMove)

  join :: HostName -> IO (LocalPlayer, RemotePlayer, Handle)
  join host = do 
    hdl <- connect host 2222 
    putStrLn "What's your name?"
    oPlayer <- getLine 

    (Hello oPlayer) `send` hdl
    (Hello xPlayer) <- receive hdl

    return ((LocalPlayer oPlayer O hdl), (RemotePlayer xPlayer X hdl), hdl)
  
  
