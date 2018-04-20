module Lab83 where

import Data.List
import Text.Read
import Control.Applicative
import Control.Monad

data Truck = Truck {
   freeCapacity :: Double
  ,boxes :: [Double]
    } deriving (Show)

data TruckOperation = UnloadAll | Load Double | Unload deriving (Show)

emptyTruck :: Double -> Truck
emptyTruck c = Truck c []

checkedLoad :: Double -> Maybe TruckOperation
checkedLoad x
  | x <=0 = Nothing
  | otherwise = Just $ Load x

parseUnloadAll :: String -> Maybe TruckOperation
parseUnloadAll "unloadAll" = Just UnloadAll
parseUnloadAll _ = Nothing

parseUnload :: String -> Maybe TruckOperation
parseUnload "unload" = Just Unload
parseUnload _ = Nothing

runTruckOp :: Truck -> TruckOperation -> Maybe Truck
runTruckOp (Truck c boxes) UnloadAll = Just (Truck (c+(foldl' (+) 0 boxes)) [])
runTruckOp (Truck _ []) Unload = Nothing
runTruckOp (Truck c (x:xs)) Unload = Just $ (Truck (c+x) xs)
runTruckOp (Truck c xs) (Load x)
  | x <= c = Just $ (Truck (c-x) (x:xs))
  | otherwise = Nothing

parseTrackOp :: String -> Maybe TruckOperation
parseTrackOp "unloadAll" = parseUnloadAll "unloadAll"
parseTrackOp "unload" = parseUnload "unload"
parseTrackOp num = do
    c <- readMaybe num
    checkedLoad c  

runStrTruckOp :: Truck -> String -> Maybe Truck
runStrTruckOp truck op = do 
    operation <- parseTrackOp op
    runTruckOp truck operation 

exec :: Truck -> [Maybe TruckOperation] -> Maybe Truck
exec truck [] = Just truck
exec truck (Nothing:_) = Nothing
exec truck ((Just op):rest) = do
    t <- runTruckOp truck op
    exec t rest 

runTruckRoute :: Double -> [String] -> Maybe Truck
runTruckRoute initial ops =
    exec (Truck initial []) $ map parseTrackOp ops
    
    
