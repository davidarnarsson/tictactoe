
module GameTypes.LocalGame where
  import TicTacToe
  import Players.SinglePlayer

  cleanUp :: (SinglePlayer, SinglePlayer) -> IO ()
  cleanUp _ = return ()

  create :: IO (SinglePlayer, SinglePlayer)
  create = do 
    p1 <- generatePlayer X
    p2 <- generatePlayer O
    return (p1, p2)

