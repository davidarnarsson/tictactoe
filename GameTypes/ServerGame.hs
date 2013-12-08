

module GameTypes.ServerGame where 
  import Net.Communication (open, accept, receive, send)
  import Net.Protocol (Message(..))
  import Players.RemotePlayer (RemotePlayer(..))
  import Players.LocalPlayer (LocalPlayer(..))
  import Network (HostName)
  import System.IO
  import TicTacToe (Token(..), TicTacToe(..))

  cleanUp :: (LocalPlayer, RemotePlayer) -> IO ()
  cleanUp ((LocalPlayer n t hdl), _) = do
    hClose hdl

  -- initiates communication between a server and a
  -- client by opening a server socket, and accepting
  -- a connection. Then, hellos are exchanged.
  create :: IO (LocalPlayer, RemotePlayer)
  create = do 
      putStrLn "What is your name?"
      xPlayer <- getLine

      sock <- open 2345
      dat <- accept sock >>= onJoined xPlayer
      return dat

    where    
      onJoined ln conn@(hdl, _, _) = do 
        (Hello name) <- receive hdl
        (Hello ln) `send` hdl
        
        let lp = (LocalPlayer ln X hdl) 
        let rm = (RemotePlayer name O hdl)

        return (lp, rm)
    