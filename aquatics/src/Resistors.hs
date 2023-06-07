{-# LANGUAGE
    NoImplicitPrelude
  , DeriveDataTypeable
  , DeriveGeneric
  , RankNTypes
  , GeneralizedNewtypeDeriving
  , DerivingStrategies
#-}
-- https://kowainik.github.io/posts/deriving
{-# OPTIONS_GHC -Wno-name-shadowing #-} -- -fwarn-unused-imports
{-|
Module      : Resistors
Copyright   : © 2022 Robert Lee
License     : ISC

Maintainers : robert.lee@chicago.vc (Robert)
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken and unwrapped 154 column display.

Description : Resistors.
-}
{-
infixr 9  .
infixr 8  ^, ^^, ⋆⋆
infixl 7  ⋆, /, ‘quot‘, ‘rem‘, ‘div‘, ‘mod‘
infixl 6  +, -
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >
infixl 4  <$, <*>, <*, *>, <**>
infixr 3  &&
infixr 2  ||
infixl 1  ?, >>, >>=
infixr 1  =<<, <=<, >=>
infixr 0  $, $!, ‘seq‘
-}

module Resistors
where

-- Explicit Imports

import Data.Char        ( isNumber, toUpper )
import Data.Default     ( Default(..) )
import Data.Data        ( Data, Typeable )
import Data.Hashable
import Data.Ix          ( Ix )
import Data.List        ( nub, nubBy, sortBy, sort )
import Data.Text        ( Text )
import Data.Word        ( Word8 )
import Data.Bits        ( Bits, FiniteBits )
import GHC.Generics     ( Generic )
import GHC.Num          ( Num )
import Foreign.Storable ( Storable )
import Numeric          ( showFFloat )
import Data.Holmes
import Prelude hiding (String)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

data Geometry a
  = Parallel a [a]
  | Series   a [a]
    deriving (Show, Eq, Ord)

class (Eq a, Show a, Ord a, RealFrac a) => GeometryC a
  where
    seriesToSingle   :: [] a -> a
    parallelToSingle :: [] a -> a
    parallelGeometry :: [] a -> Geometry a
    seriesGeometry   :: [] a -> Geometry a
    round3Geometry   :: Geometry a -> Geometry a
    getScalar  :: Geometry a -> a
    getValues  :: Geometry a -> [a]
    getCount   :: Geometry a -> Int
    isSeries   :: Geometry a -> Bool
    isParallel :: Geometry a -> Bool
    -- defaults
    parallelGeometry ns = Parallel (parallelToSingle ns) ns
    seriesGeometry   ns = Series   (seriesToSingle   ns) ns
    round3Geometry   (Parallel s ns) = Parallel (round3 s) ns
    round3Geometry   (Series   s ns) = Series   (round3 s) ns
    getScalar  (Parallel s _) = s
    getScalar  (Series   s _) = s
    getValues  (Parallel _ n) = n
    getValues  (Series   _ n) = n
    getCount   (Parallel _ n) = length n
    getCount   (Series   _ n) = length n
    isSeries   (Parallel _ n) = False
    isSeries   (Series   _ n) = True
    isParallel g = not $ isSeries g


newtype Seconds = Seconds Double
  deriving stock   ( Data, Generic )
  deriving newtype ( Enum, Eq, Floating, Fractional, Num
                   , Ord, Read, Real, RealFloat, RealFrac
                   )
  deriving newtype (Hashable)

instance Show Seconds where
  show seconds = showFFloat Nothing seconds ""

newtype Ohms = Ohms Double
  deriving stock   ( Data, Generic ) -- Should Show be in stock it will show "Ohms 0", not just numerals.
  deriving newtype ( Enum, Eq, Floating, Fractional, Num
                   , Ord, Read, Real, RealFloat, RealFrac
                   ) -- deriving newtype Show is just numerals (e.g. "0"), not "Ohms 0" etc.
  deriving newtype (Hashable)

instance Show Ohms where
  show ohms = showFFloat Nothing ohms ""

instance GeometryC Ohms where
  seriesToSingle   = sum
  parallelToSingle = recip . sum . map recip

newtype Watts = Watts Double
  deriving stock   ( Data, Generic )
  deriving newtype ( Enum, Eq, Floating, Fractional, Num
                   , Ord, Read, Real, RealFloat, RealFrac
                   )
  deriving newtype (Hashable)

instance Show Watts where
  show watts = showFFloat Nothing watts ""

newtype Tolerance = Tolerance Double
  deriving stock   ( Data, Generic )
  deriving newtype ( Enum, Eq, Floating, Fractional, Num
                   , Ord, Read, Real, RealFloat, RealFrac
                   )
  deriving newtype (Hashable)

instance Show Tolerance where
  show tolerance = showFFloat Nothing tolerance ""

newtype Leads = Leads Word8
  deriving stock   (Generic, Data)
  deriving newtype ( Bits, Bounded, Enum, Eq, FiniteBits, Integral
                   , Ix, Num, Ord, Read, Real, Show, Storable
                   )

newtype Hertz = Hertz Double -- Hz
  deriving stock   ( Data, Generic )
  deriving newtype ( Enum, Eq, Floating, Fractional, Num
                   , Ord , Read, Real, RealFloat, RealFrac
                   )
  deriving newtype (Hashable)

instance Show Hertz where
  show hertz = showFFloat Nothing hertz ""

newtype Duty = Duty Double -- Duty Cycle (a percentage)
  deriving stock   ( Data, Generic )
  deriving newtype ( Enum, Eq, Floating, Fractional, Num
                   , Ord, Read, Real, RealFloat, RealFrac
                   )
  deriving newtype (Hashable)

instance Show Duty where
  show duty = showFFloat Nothing duty ""

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- Capacitors

newtype Farads = Farads Double
  deriving stock   ( Data, Generic )
  deriving newtype ( Enum, Eq, Floating, Fractional, Num
                   , Ord, Read, Real, RealFloat, RealFrac
                   )
  deriving newtype (Hashable)

instance Show Farads where
  show farads = showFFloat Nothing farads ""

instance GeometryC Farads where
  seriesToSingle   = recip . sum . map recip
  parallelToSingle = sum

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- Inductors

newtype Henrys = Henrys Double
  deriving stock   ( Data, Generic )
  deriving newtype ( Enum, Eq, Floating, Fractional, Num
                   , Ord, Read, Real, RealFloat, RealFrac
                   )
  deriving newtype (Hashable)

instance Show Henrys where
  show henrys = showFFloat Nothing henrys ""

instance GeometryC Henrys where
  seriesToSingle   = sum
  parallelToSingle = recip . sum . map recip

-- -----------------------------------------------------------------------------------------------------------------------------------------------------

data Mounting
  = ThroughHole
  | SMD
  deriving stock (Bounded, Data, Enum, Eq, Generic, Ix, Ord, Read, Show)

data Packaging
  = Radial
  | RadialTubular
  | Axial
  | SR10
  | SR20
  | TO_126
  | TO_220
  | TO_247
  | TO_264
  deriving stock (Bounded, Data, Enum, Eq, Generic, Ix, Ord, Read, Show)

data Resistor =
  Resistor
  { ohms      :: Ohms
  , watts     :: Watts
  , tolerance :: Tolerance
  , leads     :: Leads
  , mounting  :: Mounting
  , packaging :: Packaging
  } deriving stock (Eq, Ord, Read, Show)

instance Default Resistor where
  def = Resistor
        { ohms      = 0.0         -- :: Ohms
        , watts     = 0.0         -- :: Watts
        , tolerance = 0.0         -- :: Tolerance
        , leads     = 2           -- :: Leads
        , mounting  = ThroughHole -- :: Mounting
        , packaging = Axial       -- :: Packaging
        }

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
{-
5v 74LS123
Per the datasheet:
  Keep Rt well under 260kΩ.
  Keep Rt at or above 5kΩ.
  C(pF) has no restrictions.
  Minimum pulse duration 40ns

           K
tw(ns) = 0.28 * Rt(kΩ) * C(pF) * ( 1 + 0.7 / Rt(kΩ) ) -- For non-polarized caps.

example: 10uF cap with 11.8kΩ gives
         0.28 * 11.8 * 10000000 * ( 1 * 0.7 / 11.8 )

round $ twF 0.28 11800 (22 * nf) * 1000000000 -- Gives 77000 nanoseconds.

-- For electrolytic caps. See Figure 2 on pg7 datasheet for Diode placement.
          Kd
tw(ns) = 0.25 * Rt(kΩ) * C(pF) * ( 1 + 0.7 / Rt(kΩ) )
-}

-- | twF(ns) = 0.28 * Rt(kΩ) * C(pF) * ( 1 + 0.7 / Rt(kΩ) ) -- For non-polarized caps.
--   twF(ns) = 0.22 * Rt(kΩ) * C(pF) * ( 1 + 0.7 / Rt(kΩ) ) -- For polarized electrolytics.
twF :: Double -> Ohms -> Farads -> Seconds
twF k ohms farads = ns * convert (k * rt * cpF * ( 1 + 0.7 / rt )) -- It is necessary to multiply from nanoseconds to seconds
  where
    rt :: Double
    rt = convert $ ohms / 1000.0

    cpF :: Double
    cpF = convert $ farads * recip pf -- Convert to picofarads from farads.

twCD :: Ohms -> Farads -> Seconds
twCD rx cx = 0.468 * convert rx * convert cx -- Datasheet says 0.45

-- convert :: () =>
convert :: (RealFrac a, RealFrac b) => a -> b
convert = fromRational . toRational

hzDutyToSeconds :: Hertz -> Duty -> Seconds
hzDutyToSeconds hertz duty = dutySeconds
  where
    period = convert $ recip hertz
    dutySeconds = convert $ period * duty

toMili :: (RealFrac n) => n -> n
toMili n = n / 10 ^ 3

toMicro :: (RealFrac n) => n -> n
toMicro n = n / 10 ^ 6

toNano :: (RealFrac n) => n -> n
toNano n = n / 10 ^ 9

toPico :: (RealFrac n) => n -> n
toPico n = n / 10 ^ 12

toFemto :: (RealFrac n) => n -> n
toFemto n = n / 10 ^ 15

toAtto :: (RealFrac n) => n -> n
toAtto n = n / 10 ^ 18

decade :: (RealFrac a) => a -> Int
decade a
  | a < 0 = decade (a * -1)
  | a == 0 = 0
  | a < 0.00000000000000001  = -18  -- atto  (a)
  | a < 0.0000000000000001   = -17
  | a < 0.000000000000001    = -16
  | a < 0.00000000000001     = -15  -- femto (f)
  | a < 0.0000000000001      = -14
  | a < 0.000000000001       = -13
  | a < 0.00000000001        = -12  -- pico  (p)
  | a < 0.0000000001         = -11
  | a < 0.000000001          = -10
  | a < 0.00000001           = -9   -- nano  (n)
  | a < 0.0000001            = -8
  | a < 0.000001             = -7
  | a < 0.00001              = -6   -- micro (µ) -- NB: the µ is unicode not an ascii u
  | a < 0.0001               = -5
  | a < 0.001                = -4
  | a < 0.01                 = -3   -- mili  (m)
  | a < 0.1                  = -2   -- centi (c)
  | a < 1                    = -1   -- deci  (d)
  | a < 10                   = 0
  | a < 100                  = 1    -- deka  (da)
  | a < 1000                 = 2    -- hekto (h)
  | a < 10000                = 3    -- kilo  (k)
  | a < 100000               = 4
  | a < 1000000              = 5
  | a < 10000000             = 6    -- mega  (M)
  | a < 100000000            = 7
  | a < 1000000000           = 8
  | a < 10000000000          = 9    -- giga  (G)
  | a < 100000000000         = 10
  | a < 1000000000000        = 11
  | a < 10000000000000       = 12   -- tera  (T)
  | a < 100000000000000      = 13
  | a < 1000000000000000     = 14
  | a < 10000000000000000    = 15   -- peta  (P)
  | a < 100000000000000000   = 16
  | a < 1000000000000000000  = 17
  | a < 10000000000000000000 = 18   -- exa   (E)
  | otherwise = 999

-- | round3 performs a 3 place precision round.
--   e.g. round3 88940.3 = 88900.0
round3 :: (RealFrac a) => a -> a
round3 a
  | intExponent < 0 = let f = 10 ^ (-1 * intExponent)
                      in (fromIntegral . round $ a * f) / f
  | otherwise = let f = 10 ^ intExponent
                in (fromIntegral . round $ a / f) * f
  where
    intExponent = decade a - 2

µf = Farads 0.000001       -- NB: the µ is unicode not an ascii u
nf = Farads 0.000000001
pf = Farads 0.000000000001

µs = Seconds 0.000001       -- NB: the µ is unicode not an ascii u
ns = Seconds 0.000000001
ps = Seconds 0.000000000001

-- | capacitors
capacitors :: [] Farads
capacitors
  = map round3 $ nfs ++ pfs
  where
    nfs = map (* nf) [ 1, 1.2, 1.5, 1.8, 2, 2.2, 3.3, 4.7, 5.6, 6.8, 8.2, 10, 22, 100 ]
    pfs = map (* pf) [ 800, 750, 680, 470, 330, 220, 120, 100, 82 ]

resistors :: [] Ohms
resistors =
  [ 47.0    , 100.0   , 250.0   , 500.0   , 1000.0  , 1300.0  , 1400.0 , 2500.0
  , 2700.0  , 3000.0  , 5000.0  , 5300.0  , 5600.0  , 7150.0  , 7320.0
  , 7320.0  , 7500.0  , 7680.0  , 7870.0  , 7870.0  , 8000.0  , 8200.0
  , 8250.0  , 8450.0  , 8870.0  , 9090.0  , 9100.0  , 9310.0  , 9530.0
  , 9760.0  , 10000.0 , 10200.0 , 10500.0 , 10700.0 , 11000.0 , 11300.0
  , 18000.0 , 18700.0 , 20000.0 , 20500.0 , 30000.0 , 40200.0 , 47000.0
  , 56200.0
  ]

crossProduct :: (Ord a) => [] a -> [[a]]
crossProduct pp = nub az -- remove dups
  where
    az = do
      a0 <- pp
      a1 <- pp
      pure $ sort [a0,a1]

parallelOhmsGeometry :: [] (Geometry Ohms)
parallelOhmsGeometry = map parallelGeometry $ crossProduct resistors

seriesOhmsGeometry :: [] (Geometry Ohms)
seriesOhmsGeometry = map seriesGeometry $ (crossProduct resistors ++ [resistors])

ohmsGeometry :: [] (Geometry Ohms)
ohmsGeometry = parallelOhmsGeometry ++ seriesOhmsGeometry

parallelFaradsGeometry :: [] (Geometry Farads)
parallelFaradsGeometry = map parallelGeometry $ (crossProduct capacitors ++ [capacitors])

seriesFaradsGeometry :: [] (Geometry Farads)
seriesFaradsGeometry = map seriesGeometry $ crossProduct capacitors

faradsGeometry :: [] (Geometry Farads)
faradsGeometry = parallelFaradsGeometry ++ seriesFaradsGeometry

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- Sample code + WIP

dinesman :: IO (Maybe [ (Text, Defined Int) ]) -- Sample from the Data.Holmes README.
dinesman = do
  let guesses = 5 `from` [1 .. 5]

  mvs <- guesses `satisfying` \[ baker, cooper, fletcher, miller, smith ] -> and'
    [ distinct [ baker, cooper, fletcher, miller, smith ]
    , baker ./= 5
    , cooper ./= 1
    , fletcher ./= 1 .&& fletcher ./= 5
    , miller .> cooper
    , abs' (smith .- fletcher) ./= 1
    , abs' (fletcher .- cooper) ./= 1
    ]
  case mvs of
    Nothing -> pure Nothing
    Just vs -> pure . Just . sortBy (\(_,x) (_,y) -> x `compare` y) $ zip folks vs
  where
    folks = [ "baker", "cooper", "fletcher", "miller", "smith" ]

crossProducts :: [Geometry Ohms] -> [Geometry Farads] -> [] (Geometry Ohms, Geometry Farads)
crossProducts ogs fgs = do
  og <- rangeFiltered
  fg <- fgs
  pure (round3Geometry og, round3Geometry fg)
  where
    rangeFiltered = filter (\geo -> getScalar geo >= 5000 && getScalar geo < 250000) ogs

dutySeconds :: Duty -> Seconds
dutySeconds duty = hzDutyToSeconds 1800 duty

isWithin :: Seconds -> Seconds -> Seconds -> Bool
isWithin margin target candidate = candidate >= lower && candidate <= upper
  where
    fudge = target * margin
    upper = target + fudge
    lower = target - fudge

componentCount :: (GeometryC a, GeometryC b) => Geometry a -> Geometry b -> Int
componentCount a b = getCount a + getCount b

fnoodlesTargets :: [] (Int, Duty)
fnoodlesTargets = fnoodles targets

fnoodles :: [] Duty -> [] (Int, Duty)
fnoodles ds
  = map (\d -> (length $ matching d, round3 $ d * 100.0)) ds -- The snd of the pair is for display only.

matching :: Duty -> [] (Seconds, Geometry Ohms, Geometry Farads)
matching d
  = filter (\(_,o,f) -> componentCount o f <= 4)
  $ filter (\(s,o,f) -> isWithin 0.0002 dutySecs s)
    serialOhmsParallelFaradsCalcs
  where
    dutySecs = dutySeconds d

serialOhmsParallelFaradsCalcs :: [] (Seconds, Geometry Ohms, Geometry Farads)
serialOhmsParallelFaradsCalcs = twCalcs $ crossProducts seriesOhmsGeometry parallelFaradsGeometry

matchingRounded :: Duty -> [] (Seconds, Geometry Ohms, Geometry Farads)
matchingRounded d
  = filter (\(_,o,f) -> componentCount o f <= 4)
  $ filter (\(s,o,f) -> dutySecs == s)
    serialOhmsParallelFaradsCalcsRounded
  where
    dutySecs = round3 $ dutySeconds d

-- Pre rounding over multiple duty cycles can be faster for rounded comparisons.
serialOhmsParallelFaradsCalcsRounded :: [] (Seconds, Geometry Ohms, Geometry Farads)
serialOhmsParallelFaradsCalcsRounded = map (\(s,o,f) -> (round3 s,o,f)) serialOhmsParallelFaradsCalcs

twCalcs :: [] (Geometry Ohms, Geometry Farads) -> [] (Seconds, Geometry Ohms, Geometry Farads)
twCalcs geos = map (\(go,gf) -> ({- twF 0.28 -} twCD (getScalar go) (getScalar gf), go, gf)) geos

calcs :: [] (Seconds, Geometry Ohms, Geometry Farads)
calcs = twCalcs $ crossProducts ohmsGeometry faradsGeometry

-- targets :: [] Duty
-- targets = map (/ 100.0)
--   [ 5.3
--   , 5.4
--   , 5.5
--   , 5.6
--   , 5.7
--   , 5.8
--   , 5.9
--   , 6.0
--   , 6.1
--   , 6.2
--   , 6.3
--   , 6.4
--   , 6.5
--   , 6.6
--   , 6.8
--   , 7.6
--   ]

targets :: [] Duty
targets = map (/ 100.0)
  [  2.55 --  1    -- First tier ICs
  ,  5.6  --  2
  , 11.2  --  3
  , 16.9  --  4
  , 22.5  --  5
  , 28.1  --  6
  , 33.8  --  7
  , 39.4  --  8
  , 45.0  --  9    -- Second tier ICs
  , 50.6  -- 10
  , 56.3  -- 11
  , 61.9  -- 12
  , 73.1  -- 13
  , 78.8  -- 14
  , 84.4  -- 15
  , 90.0  -- 16
  ]








-- nubFunc = nubBy

-- nubBy (\(os,fs) (os', fs') -> sort os == sort os' && sort fs == sort fs') $ map (\(_, a, b) -> (getValues a, getValues b)) $ matching (6.8 / 100)
