module Main where 

import System.IO
import System.IO.Error
import System.Environment
import Control.Monad

main :: IO ()
main = do 
    (a:args) <- getArgs 
    output <- openFile a WriteMode
    files <- sequence $ map (\x -> openFile x ReadMode) args
    processFiles output files



processFiles :: Handle -> [Handle] -> IO ()
processFiles output files = do
    chars <-  (catchIOError (sequence $ map hGetChar files) (\_-> return []))
    putStrLn chars
    processChars output files chars


processChars :: Handle -> [Handle] -> [Char] -> IO ()
processChars output files chars = 
    case chars of [] -> hClose output
                  x -> do {void $ hPutStr output chars; processFiles output files}
    