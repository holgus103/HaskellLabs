module Lab81 where

import Control.Concurrent 
import System.IO
import Text.Read

castText  :: String -> IO ()
castText "" = return ();
castText (c:rest) = do {
    threadDelay 100000;
    putChar c;
    castText rest;
} 

readDefault :: (Read a) => a -> IO a
readDefault def = do
    line <- getLine 
    case readMaybe line of
        Nothing -> return def
        Just x -> return x

askDefault :: (Read a) => String -> a -> IO a
askDefault query val = do {
    putStrLn query;
    readDefault val
}