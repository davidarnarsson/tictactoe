
-- This is in part based on the JSON parser illustrated
-- in the Real World Haskell book. 
--http://book.realworldhaskell.org/read/using-parsec.html
module Net.Protocol where 

  -- The protocol used : 
  {-
    Move (Int, Int) -> Represents player move on a 0-idxd grid
    Hello String    -> The players exchange hellos when connecting
                       with eachother's names
    Size Int        -> The size of the board, sent by the host to the
                       client, immediately after the hello 
    Goodbye         -> As of now, not used...
  -}
  data Message = Move (Int, Int) | Hello String | Size Int | Goodbye 
    deriving (Eq, Show, Read)

  -- parses a string into a Message, or an error
  parse :: String -> Message
  parse = read 
  
  -- serializes a message
  serialize :: Message -> String 
  serialize = show

