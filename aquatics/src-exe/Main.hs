import Resistors
import Control.Monad (mapM_)

main :: IO ()
main = do
  putStrLn $ "Calcs: " ++ show (length calcs)
  putStrLn $ "serialOhmsParallelFaradsCalcs: " ++ show (length serialOhmsParallelFaradsCalcs)
  mapM_ print fnoodlesTargets
