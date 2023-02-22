module Read where
import System.IO
import Control.Exception
import Compute

readArg :: String -> IO String
readArg arg = do
    file <- try (readFile arg)::IO(Either SomeException String)
    case file of
        Left e -> throw e
        Right f -> return f

readArgs :: [String] -> IO Int
readArgs [] = hPutStrLn stderr "input file missing" >> return 84
readArgs [arg] = do
        file <- try(readArg arg)::IO(Either SomeException String)
        case file of
            Left e -> hPutStrLn stderr (show e) >> return 84
            Right f -> compute f
readArgs _ = hPutStrLn stderr "too many input" >> return 84
