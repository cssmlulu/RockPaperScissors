 
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
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hi, what's your name?"
    name <- liftM init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \listenerLoop -> do
        (nr', line) <- readChan chan'
        when (nr /= nr') $ hPutStrLn hdl line
        listenerLoop

    hPutStrLn hdl "Enter Rock, Paper or Scissors to play game. Or chat with others. Or you can quit."
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        rst <- server hdl name
        case rst of
            "quit" -> do
                broadcast ("<-- " ++ name ++ " left.")
                killThread reader
                hClose hdl
            _      -> loop
    where
        server hdl name = do
            line <- liftM init (hGetLine hdl)
            let line' = convertStrategy line
            case line' of
                Nothing -> do
                    case line of
                        "quit" -> do
                            return "quit"
                        _      -> do
                            broadcast (name ++ ": " ++ line)
                            return "continue"                      
                Just strategy  -> do
                    putStrLn (name ++ " choose: " ++ show strategy)
                    result <- game hdl strategy
                    broadcast (name ++ " " ++ show result)
                    hPutStrLn hdl ("You " ++ show result)
                    return "continue"
 
        broadcast msg = do
            writeChan chan (nr, msg)
            putStrLn msg

        game hdl strategy = do
            hPutStrLn hdl ("You choose: " ++ show strategy)
            -- computer
            randNum <- getStdRandom (randomR (0,2))
            hPutStr hdl  "Computer choose: "
            let computer = randomStrategy randNum
            hPutStrLn hdl $ show computer

            -- result
            let result = playGame computer strategy
            return result
