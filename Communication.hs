

module Communication where
  import Network.Socket
  import System.IO
  import Data.Word

  -- http://www.haskell.org/haskellwiki/Implement_a_chat_server
  -- http://book.realworldhaskell.org/read/sockets-and-syslog.html

  data SocketHandle = SocketHandle { socket :: Socket, handle :: Handle }

  open :: Int -> IO SocketHandle
  open port = do 
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
    listen sock 1
    -- handle 
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    return SocketHandle sock hdl

  getSocket :: Int -> IO (Socket)
  getSocket port = do 
    sock <- socket AF_INET Stream 0

    setSocketOption sock ReuseAddr 1

    bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)

    listen sock 1
    return sock

  openHandle :: Socket -> IO (Handle)
  openHandle socket = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl

  accept :: 

  connect :: 

  closeHandle :: Handle -> IO ()
  closeHandle = hClose

  closeSocket :: Socket -> IO ()
  closeSocket = 
  --closeHandle :: Handle ->

