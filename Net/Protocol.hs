
module Net.Protocol where   
  import Test.QuickCheck
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

  instance Arbitrary Message where 
    arbitrary = oneof [ do
                          x <- arbitrary
                          y <- arbitrary
                          return $ Move (x, y)
                      , do 
                          str <- arbitrary 
                          return $ Hello str
                      , do
                          x <- arbitrary
                          return $ Size x
                      , return Goodbye ]

  -- Unsurprisingly, returns +++ OK, passed 100 tests. 
  prop_parse msg = msg == (parse . serialize) msg