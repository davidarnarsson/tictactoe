

module GameTypes.AIGame where 
  import Players.SinglePlayer
  import Players.AIPlayer
  import TicTacToe

  create :: IO (SinglePlayer, AIPlayer) 
  create = do
    x <- generatePlayer X 
    return (x, AIPlayer O) 

  cleanUp :: (SinglePlayer, AIPlayer) -> IO ()
  cleanUp _ = return ()
