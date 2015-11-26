 
import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
 
import GameData
import System.Random(getStdRandom,randomR)

type Msg = (Int, String)
 
main :: IO ()
main = do
    chan <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 1617 iNADDR_ANY)
    listen sock 2
    forkIO $ fix $ \loop -> do
        (_, msg) <- readChan chan
        loop
    mainLoop sock chan 0
 
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1
 
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
    let broadcast msg = do
        writeChan chan (nr, msg)
        putStrLn msg
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hi, what's your name?"
    name <- liftM init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \loop -> do
        (nr', line) <- readChan chan'
        when (nr /= nr') $ hPutStrLn hdl line
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        let line' = convertStrategy line
        case line' of
            Nothing -> do
                case line of
                    "quit" -> hPutStrLn hdl "Bye!"
                    _      -> do
                        broadcast (name ++ ": " ++ line)
                        loop                       
            Just x  -> do
                putStrLn (name ++ " choose " ++ show x)
                game hdl x
                loop
    killThread reader
    broadcast ("<-- " ++ name ++ " left.")
    hClose hdl



game hdl player = do
    -- computer
    randNum <- getStdRandom (randomR (0,2))
    hPutStr hdl  "Computer's choice: "
    let computer = randomStrategy randNum
    hPutStrLn hdl $ show computer

    -- result
    let result = playGame computer player
    hPutStrLn hdl  $ show result
    return "quit"
