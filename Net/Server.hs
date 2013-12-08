

module Net.Server where 
  import Net.Communication (open, accept, receive, send)
  import Net.Protocol (Message(..))
  import Players.RemotePlayer (RemotePlayer(..))
  import Players.LocalPlayer (LocalPlayer(..))
  import Network (HostName)
  import System.IO
  import TicTacToe (Token(..), TicTacToe(..))
  

  create :: IO (LocalPlayer, RemotePlayer, Handle)
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

        return (lp, rm, hdl)
    