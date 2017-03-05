import Control.Concurrent
import Control.Concurrent.Chan
import System.IO
import Network.Socket


data Server = Server {}
data Message = Message {}


-- lock mutex
-- update state
-- notify event listeners
serverUpdateState :: (Server -> Server) -> IO ()
serverUpdateState appl = do
    return ()


socketConnect :: SockAddr -> Server -> Server
socketConnect addr serv = serv


socketDisconnect :: SockAddr -> Server -> Server
socketDisconnect addr serv = serv


socketRecieve :: SockAddr -> Message -> Server -> Server
socketRecieve addr msg serv = serv


openSocket :: PortNumber -> IO Socket
openSocket port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 5
    return sock


acceptLoop :: Socket -> IO ()
acceptLoop sock = do
    connection <- accept sock
    let (_, addr) = connection in
        serverUpdateState (socketConnect addr)
    forkIO (socketLoop connection)
    acceptLoop sock


-- checks for disconnecting sockets
socketLoop :: (Socket, SockAddr) -> IO ()
socketLoop (sock, addr) = do
    msg <- readSocket sock
    serverUpdateState (socketRecieve addr msg)


readSocket :: Socket -> IO Message
readSocket sock = do
    return Message


main :: IO ()
main = do
    acceptor <- openSocket 1234
    acceptLoop acceptor
