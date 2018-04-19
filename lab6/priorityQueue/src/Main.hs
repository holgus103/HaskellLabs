module Main where
import Data.List
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M

type Name = String
type Priority = Int
type StIO a = StateT [(Name, Priority)] IO a


unknownCommand :: StIO Bool
unknownCommand = (lift $ putStrLn "Unknown command") >> return True

ask :: String -> StIO String
ask prompt = lift (putStrLn prompt >> getLine)

push :: StIO ()
push = do
  proc <- ask "name"
  priority <- read <$> ask "priority"
  modify' $ fun proc priority
  where 
    fun :: Name -> Priority -> [(Name, Priority)] -> [(Name, Priority)]
    fun proc priority [] = [(proc, priority)];
    fun proc priority l = res
      where
        (res, _) = foldl (\(result, success) (e_name, e_priority) -> 
          if (not success) && e_priority > priority
          then (result ++ [(proc, priority),(e_name, e_priority)], True)
          else (result ++ [(e_name, e_priority)], success)
          ) ([], False) l
    

pop :: StIO ()
pop = do
  (l:ls) <- get 
  lift (putStrLn $ fst l) >> put ls
  

commands :: M.Map String (StIO Bool)
commands = M.fromList [
  ("push", push >> return True),
  ("pop", pop >> return True),
  ("exit", return False)
  ]

readCommand :: StIO Bool
readCommand = lift getLine >>= processCommand

processCommand :: String -> StIO Bool
processCommand cmd = M.findWithDefault unknownCommand cmd commands

main :: IO ()
main = do
  r <- execStateT mainLoop []
  return ();


mainLoop :: StIO ()
mainLoop = do
  result <- readCommand
  if result
    then mainLoop
    else return ()
  
  
