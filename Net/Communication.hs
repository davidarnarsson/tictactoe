

module Net.Communication where
  import Network 
  import Network.Socket
  import System.IO as IO 
  import Data.Word
  import Net.Protocol (Message(..), serialize, parse)

  -- http://hackage.haskell.org/package/network-2.4.2.1/docs/Network.html

  -- opens a server socket
  open :: Int -> IO Socket
  open = listenOn2

  -- connects to a host on a port
  connect :: String -> Int -> IO Handle
  connect host port = connectTo host $ (PortNumber . fromIntegral) port

  -- Accepts a connection to a socket
  accept :: Socket -> IO (Handle, HostName, PortNumber)
  accept = Network.accept

  -- Sends a message to a handle 
  send :: Message -> Handle -> IO ()
  send msg hdl = do
    write hdl $ serialize msg
    hFlush hdl

  -- Receives a message from a handle
  receive :: Handle -> IO (Message)
  receive hdl = do 
    resp <- hGetLine hdl
    let msg = parse resp in return msg

  -- Writes a string to a handle
  write :: Handle -> String -> IO () 
  write = hPutStrLn
    
  -- there is a problem with the windows haskell implementation
  -- for socket listening, it does not work without setting the
  -- protocol to "defaultProtocol"
  -- http://stackoverflow.com/questions/10344247/in-haskell-socket-listening-failed-on-windows
  listenOn2 :: Int -> IO Socket
  listenOn2 port = do
    --proto <- getProtocolNumber "tcp"  ,here is the difference!!!!
    let proto = defaultProtocol
    sock <- Network.Socket.socket AF_INET Stream proto

    setSocketOption sock ReuseAddr 1

    bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
    listen sock maxListenQueue
    return sock
