module Stuff
where

import Control.Monad
import Data.List

import Prelude
  
roundToStuff :: Int -> Double -> Double
roundToStuff 0 num = (fromIntegral . round $ num * 10) / 10
roundToStuff intExponent num = (fromIntegral . round $ num / f) * f
  where f = 10 ^ (intExponent - 1)

invert :: [] Double -> Double
invert ns = 1 / (sum $ map (1 /) ns)

resistorVals :: Int -> [] Double
-- resistorVals 0 = [ 30000, 56200, 47000, 40200, 20500, 8450, 5600, 5300, 9310 ]
resistorVals intExponent = map (* decade) moreDecades
  where
    firstDecade = [0.1,1,4.7,20,27,33,43,47] -- 0.1,1,4.7,20,27,33,43,47 -- First decade.
    decade = fromIntegral $ 10 ^ intExponent :: Double

moreDecades :: [] Double             
moreDecades =
  [ 47.0    , 100.0   , 250.0   , 500.0   , 1000.0  , 2500.0  , 2700.0
  , 3000.0  , 5000.0  , 5300.0  , 5600.0  , 7150.0  , 7320.0  , 7320.0
  , 7500.0  , 7680.0  , 7870.0  , 7870.0  , 8000.0  , 8200.0  , 8250.0
  , 8450.0  , 8870.0  , 9090.0  , 9100.0  , 9310.0  , 9530.0  , 9760.0
  , 10200.0 , 10500.0 , 10700.0 , 11000.0 , 11300.0 , 20500.0 , 30000.0
  , 40200.0 , 47000.0 , 56200.0
  ]

-- moreDecades =
--   [ 47, 30000, 56200, 47000, 40200, 20500, 8450, 5600
--   , 5300, 9310, 1000, 5000, 9100, 8000, 2500, 2700, 10500
--   , 10700, 7500, 7870, 10200, 7680, 8200, 8250, 11000, 7870
--   , 11300, 3000, 7320, 9530, 9760, 9090, 7150, 8870, 7320
--   , 500, 250, 100
--   ]

-- combos :: [] Double -> [[Double]]
-- combos pp = filter (not . null) $ do
--   ds1 <- ds
--   ds2 <- ds
--   ds3 <- ds
--   ds4 <- ds
--   ds5 <- ds
--   ds6 <- ds -- Anything more than ds6 (e.g. ds7) adds few additional values for tons of compute time.
--   pure $ filter (/= 0) [ds1,ds2,ds3,ds4,ds5,ds6]
--   where ds = 0:pp


caps :: [] Double
caps = map (* (1 / nf)) $ nfs ++ pfs
  where
    nfs = map (* nf) [ 1, 1.2, 1.5, 1.8, 2, 2.2, 3.3, 4.7, 5.6, 6.8, 8.2, 10, 22, 100 ]
    pfs = map (* pf) [ 800, 750, 680, 470, 330, 220, 120, 100, 82 ]
    
    nf = 0.000000001
    pf = 0.000000000001
    









combos :: [] Double -> [[Double]]
combos pp = filter (not . null) $ do
  ds1 <- ds
  ds2 <- ds
  pure $ filter (/= 0) [ds1,ds2]
  where ds = 0:pp

run :: IO ()
run = do
  mapM_ print parts
  print $ length res
  where res = sort $ nub (0 : (map (roundToStuff decade . invert) $ combos (resistorVals decade)))
        parts = map (part res) $ buildRangesByDecade decade
        decade = 0

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
