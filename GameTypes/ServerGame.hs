

module GameTypes.ServerGame where 
  import Net.Communication (open, accept, receive, send)
  import Net.Protocol (Message(..))
  import Players.RemotePlayer (RemotePlayer(..))
  import Players.LocalPlayer (LocalPlayer(..))
  import Network.Socket (sClose)
  import System.IO
  import TicTacToe (Token(..))

  -- closes the client handle and the server socket
  cleanUp :: (LocalPlayer, RemotePlayer) -> IO ()
  cleanUp (LocalPlayer _ _ hdl Nothing, _) = hClose hdl
  cleanUp (LocalPlayer _ _ hdl (Just sock), _) = do
    hClose hdl
    sClose sock

  -- initiates communication between a server and a
  -- client by opening a server socket, and accepting
  -- a connection. Then, hellos are exchanged.
  create :: IO (LocalPlayer, RemotePlayer)
  create = do 
      putStrLn "What is your name?"
      xPlayer <- getLine

      putStrLn "Waiting for player..."
      sock <- open 2345  
      accept sock >>= onJoined xPlayer sock
    
    where    
      onJoined ln sock (hdl, _, _) = do 
        (Hello name) <- receive hdl
        Hello ln `send` hdl
        
        let lp = LocalPlayer ln X hdl (Just sock)
        let rm = RemotePlayer name O hdl

        return (lp, rm)
    