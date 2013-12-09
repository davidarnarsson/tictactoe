

module GameTypes.ServerGame where 
  import Net.Communication (open, accept, receive, send)
  import Net.Protocol (Message(..))
  import Players.RemotePlayer (RemotePlayer(..))
  import Players.LocalPlayer (LocalPlayer(..))
  import Network (HostName)
  import Network.Socket (close, Socket(..))
  import System.IO
  import TicTacToe (Token(..), TicTacToe(..))

  cleanUp :: (LocalPlayer, RemotePlayer) -> IO ()
  cleanUp ((LocalPlayer n t hdl (Just sock)), _) = do
    hClose hdl
    close sock

  -- initiates communication between a server and a
  -- client by opening a server socket, and accepting
  -- a connection. Then, hellos are exchanged.
  create :: IO (LocalPlayer, RemotePlayer)
  create = do 
      putStrLn "What is your name?"
      xPlayer <- getLine

      sock <- open 2345
      dat <- accept sock >>= onJoined xPlayer sock
      return dat

    where    
      onJoined ln sock conn@(hdl, _, _) = do 
        (Hello name) <- receive hdl
        (Hello ln) `send` hdl
        
        let lp = (LocalPlayer ln X hdl (Just sock)) 
        let rm = (RemotePlayer name O hdl)

        return (lp, rm)
    