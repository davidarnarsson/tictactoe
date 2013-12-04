

module Communication where
  import Network 
  import Network.Socket
  import System.IO
  import Data.Maybe
  import Data.Word

  -- http://hackage.haskell.org/package/network-2.4.2.1/docs/Network.html

  close :: Socket -> IO () 
  close = sClose

  open :: Int -> IO Socket
  open = listenOn2

  connect :: String -> Int -> IO Handle
  connect host port = connectTo host $ (PortNumber . fromIntegral) port

  accept :: Socket -> IO (Handle, HostName, PortNumber)
  accept = Network.accept

  handleConnection :: (Handle, HostName, PortNumber) -> IO () 
  handleConnection conn@(hdl, host, port) = do
    write hdl "Hello!"
    resp <- hGetLine hdl
    putStrLn $ "Client said: " ++ resp
    putStrLn $ "Continue?" 
    c <- fmap read getLine
    case c of 
      1 -> handleConnection conn
      _ -> do 
        hClose hdl
        return ()


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
