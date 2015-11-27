 
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
    hPutStrLn hdl "What's your name?"
    name <- liftM init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \listenerLoop -> do
        (nr', line) <- readChan chan'
        when (nr /= nr') $ hPutStrLn hdl line
        listenerLoop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        hPutStrLn hdl "1) Play Game.\n2) Player Info\nq) Quit."
        line <- liftM init (hGetLine hdl)
        case line of
            "q"  -> do
                broadcast ("<-- " ++ name ++ " left.")
                killThread reader
                hClose hdl
            "1"     -> do
                game hdl name (0,0,0)
                loop
            "2"     -> do
                hPutStrLn hdl $ ("Name:" ++ name ++ "\tID:" ++ show nr)
                loop
            _      -> do
                hPutStrLn hdl ("Error Input!")
                loop
    where
        broadcast msg = do
            writeChan chan (nr, msg)
            putStrLn msg

        game hdl name stats = do
            hPutStrLn hdl $ printStats stats
            hPutStrLn hdl "Enter Rock(r), Paper(p) or Scissors(s) to play game. Enter q to quit game mode."
            choice <- liftM init (hGetLine hdl)
            let choice' = convertStrategy choice
            case choice' of
                Nothing -> do
                    case choice of
                        "q"  -> return ()
                        _       -> do
                            hPutStrLn hdl ("Invalid move!")
                            game hdl name stats
                Just strategy -> do
                    --putStrLn (name ++ " choose: " ++ show strategy)
                    hPutStrLn hdl ("You choose: " ++ show strategy)
                    -- computer
                    randNum <- getStdRandom (randomR (0,2))
                    hPutStr hdl  "Computer choose: "
                    let computer = randomStrategy randNum
                    hPutStrLn hdl $ show computer

                    -- result
                    let result = playGame computer strategy
                    let newStats = updateStats stats result
                    --broadcast (name ++ " " ++ show result)
                    hPutStrLn hdl ("You " ++ show result)
                    game hdl name newStats
