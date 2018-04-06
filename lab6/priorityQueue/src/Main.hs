module Main where
import Data.List

type Name = String
type Priority = Int
type StIO a = StateT (M.Map Name Priority) IO a


ask :: String -> StIO String
ask prompt = lift (putStrLn prompt >> getLine)

push :: IoSt()
push = do
  proc <- ask "name"
  priority <- ask "priority"
  modify' fun
  where 
    fun m = res
    where
      (res, ans) = foldl (\(l, success) (e_proc, e_priority) ->
          if (not success) && e_prority > priority then (s ++ [(proc, priority)], True)
          else (s, False)
        ) m
    

pop :: IoSt()
pop = do
  (l:ls) <- get 
  put ls
  return l

commands :: M.Map String (StIO Bool)
commands = M.fromList [
  ("push", push >> return True),
  ("pop", pop >> return True)
  ("exit", return False)
  ]

readCommand :: StIO Bool
readCommand = lift getLine >>= processCommand

processCommand :: String -> StIO Bool
processCommand cmd = M.findWithDefault unknownCommand cmd commands

main :: IO ()
main = do
  r <- execStateT mainLoop []


mainLoop :: StIO ()
mainLoop = do
  result <- readCommand
  if result
    then mainLoop
    else return ()
  
  
