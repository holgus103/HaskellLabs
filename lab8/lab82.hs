module Main where

import System.IO
import System.Environment

data IOCommand = Read | Write Char | MoveF | MoveB

parseIOCommand :: String -> Maybe IOCommand
parseIOCommand "r" = Just Read
parseIOCommand ('w':c:[]) = Just $ Write c
parseIOCommand "f" = Just MoveF
parseIOCommand "b" = Just MoveB
parseIOCommand _ = Nothing

execSingleAction :: Handle -> IOCommand -> IO ()
execSingleAction file Read = do
    c <- hGetChar file
    putChar c

execSingleAction file (Write c) =
    hPutChar file c

execSingleAction file MoveF = 
    hSeek file RelativeSeek 1

execSingleAction file MoveB =
    hSeek file RelativeSeek (-1)

execActions :: Handle -> [IOCommand] -> IO ()
execActions file actions = do
    sequence $ map (\x -> execSingleAction file x) actions
    return ()
    --foldl (\state action -> execSingleAction file action) (return ()) actions

parseCommandList :: [String] -> Maybe [IOCommand]
parseCommandList commands = 
    sequence $ map parseIOCommand commands 

exec :: Handle -> Maybe [IOCommand] -> IO ()
exec _ Nothing = return ()
exec file (Just commands) = 
    execActions file commands


main :: IO ()
main = do
    args <- getArgs
    mainArgsOk args 

    
mainArgsOk :: [String] -> IO ()
mainArgsOk [] = putStrLn "no args"
mainArgsOk [e] = putStrLn "insufficient args"

mainArgsOk (path:args) = do 
    
    file <- openFile path ReadWriteMode 
    exec file $ parseCommandList args
    hClose file