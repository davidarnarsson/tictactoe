

module GameTypes.ClientGame where 
  import Net.Communication (send, connect, receive)
  import Net.Protocol (Message(..), parse, serialize)
  import System.IO (Handle(..), hClose, hFlush)
  import Network (HostName)
  import Players.LocalPlayer
  import Players.RemotePlayer
  import TicTacToe (TicTacToe(..), Token(..))
  import Util (getMove)

  cleanUp :: (RemotePlayer, LocalPlayer) -> IO ()
  cleanUp ((RemotePlayer n t hdl), _) = do
    hClose hdl

  -- Joins a server by connecting to a host and exchanging 
  -- hello's with the host.
  create :: IO (RemotePlayer, LocalPlayer)
  create = do 
    putStrLn "Enter the name of the host"
    host <- getLine
    hdl <- connect host 2345 
    putStrLn "What's your name?"
    oPlayer <- getLine 

    (Hello oPlayer) `send` hdl
    (Hello xPlayer) <- receive hdl

    return ((RemotePlayer xPlayer X hdl), (LocalPlayer oPlayer O hdl))
  
  
