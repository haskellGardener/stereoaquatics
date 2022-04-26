import Control.Monad
import Data.List

roundTo :: Int -> Double -> Double
roundTo 0 num = (fromIntegral . round $ num * 10) / 10
roundTo intExponent num = (fromIntegral . round $ num / f) * f
  where f = 10 ^ (intExponent - 1)

invert :: [] Double -> Double
invert ns = 1 / (sum $ map (1 /) ns)

resistorVals :: Int -> [] Double
resistorVals intExponent = map (* decade) firstDecade
  where
    firstDecade = [0.1,1,4.7,20,27,33,43,47] -- 0.1,1,4.7,20,27,33,43,47 -- First decade.
    decade = fromIntegral $ 10 ^ intExponent :: Double

combos :: [] Double -> [[Double]]
combos pp = filter (not . null) $ do
  ds1 <- ds
  ds2 <- ds
  ds3 <- ds
  ds4 <- ds
  ds5 <- ds
  ds6 <- ds -- Anything more than ds6 (e.g. ds7) adds few additional values for tons of compute time.
  pure $ filter (/= 0) [ds1,ds2,ds3,ds4,ds5,ds6]
  where ds = 0:pp

main :: IO ()
main = do
  mapM_ print parts
  print $ length res
  where res = sort $ nub (0 : (map (roundTo decade . invert) $ combos (resistorVals decade)))
        parts = map (part res) $ buildRangesByDecade decade
        decade = 3

part :: [] Double -> (Double, Double) -> [] Double
part ds (beg, end) = filter (\n -> n >= beg && n < end) ds

buildRangesByDecade :: Int -> [] (Double, Double)
buildRangesByDecade 0 = zip [0..9] [1 .. 10]
buildRangesByDecade intExponent = zip [decade, decade * 2 ..] [decade * 2, decade * 3 .. decadeSucc]
  where decade     = fromIntegral $ 10 ^ intExponent :: Double
        decadeSucc = fromIntegral $ 10 ^ (intExponent + 1) :: Double
                     
{-
# 0.1,1,4.7,20,27,33,43,47 -- First decade.

time stack exec aquatics-exe
[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
[1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9]
[2.0,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9]
[3.0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9]
[4.0,4.1,4.2,4.3,4.4,4.5,4.6,4.7,4.8,4.9]
[5.0,5.1,5.2,5.3,5.4,5.5,5.6,5.7,5.8,5.9]
[6.0,6.1,6.2,6.3,6.4,6.5,6.6,6.7,6.8,6.9]
[7.0,7.1,7.2,7.3,7.4,7.5,7.6,7.7,7.8,7.9]
[8.0,8.1,8.2,8.3,8.4,8.5,8.6,8.7,8.8,8.9]
[9.0,9.1,9.2,9.3,9.4,9.5,9.6,9.7,9.8,9.9]
140

real	0m0.377s
user	0m0.525s
sys	0m0.245s

-}
