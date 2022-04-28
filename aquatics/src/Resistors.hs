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
License     : All Rights Reserved

Maintainers : robert.lee@chicago.vc (Robert)
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken and unwrapped 154 column display.

Description : Provides processing for commissions based on CSV inputs.
Neighborhood: GoldCoast
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
import Data.List        ( sortBy )
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
    -- defaults
    parallelGeometry ns = Parallel (parallelToSingle ns) ns
    seriesGeometry   ns = Series   (seriesToSingle   ns) ns
    round3Geometry   (Parallel s ns) = Parallel (round3 s) ns
    round3Geometry   (Series   s ns) = Series   (round3 s) ns

newtype Seconds = Seconds Double
  deriving stock   (Generic, Data)
  deriving newtype (Eq, Ord, RealFloat, RealFrac, Real, Floating, Enum, Fractional, Num, Read)
  deriving newtype (Hashable)

instance Show Seconds where
  show seconds = showFFloat Nothing seconds ""

newtype Ohms = Ohms Double
  deriving stock   (Generic, Data)         -- Should Show be in stock it will show "Ohms 0", not just numerals.
  deriving newtype (Eq, Ord, RealFloat, RealFrac, Real, Floating, Enum, Fractional, Num, Read) -- deriving newtype Show is just numerals (e.g. "0"), not "Ohms 0" etc.
  deriving newtype (Hashable)

instance Show Ohms where
  show ohms = showFFloat Nothing ohms ""

instance GeometryC Ohms where
  seriesToSingle   = sum
  parallelToSingle = recip . sum . map recip
  parallelGeometry ns = Parallel (fromIntegral . round $ parallelToSingle ns) ns -- This could be changed to round only when values are larger than X.

newtype Watts = Watts Double
  deriving stock   (Generic, Data)
  deriving newtype (Eq, Ord, RealFloat, RealFrac, Real, Floating, Enum, Fractional, Num, Read)
  deriving newtype (Hashable)

instance Show Watts where
  show watts = showFFloat Nothing watts ""

newtype Tolerance = Tolerance Double
  deriving stock   (Generic, Data)
  deriving newtype (Eq, Ord, RealFloat, RealFrac, Real, Floating, Enum, Fractional, Num, Read)
  deriving newtype (Hashable)

instance Show Tolerance where
  show tolerance = showFFloat Nothing tolerance ""

newtype Leads = Leads Word8
  deriving stock   (Generic, Data)
  deriving newtype (Integral, Ix, Bounded, FiniteBits, Bits, Storable, Eq, Ord, Real, Enum, Num, Read, Show)

newtype Hertz = Hertz Double -- Hz
  deriving stock   (Generic, Data)
  deriving newtype (Eq, Ord, RealFloat, RealFrac, Real, Floating, Enum, Fractional, Num, Read)
  deriving newtype (Hashable)

instance Show Hertz where
  show hertz = showFFloat Nothing hertz ""

newtype Duty = Duty Double -- Duty Cycle (a percentage)
  deriving stock   (Generic, Data)
  deriving newtype (Eq, Ord, RealFloat, RealFrac, Real, Floating, Enum, Fractional, Num, Read)
  deriving newtype (Hashable)

instance Show Duty where
  show duty = showFFloat Nothing duty ""

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- Capacitors

newtype Farads = Farads Double
  deriving stock   (Generic, Data)
  deriving newtype (Eq, Ord, RealFloat, RealFrac, Real, Floating, Enum, Fractional, Num, Read)
  deriving newtype (Hashable)

instance Show Farads where
  show farads = showFFloat Nothing farads ""

instance GeometryC Farads where
  seriesToSingle   = recip . sum . map recip
  parallelToSingle = sum

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- Inductors

newtype Henrys = Henrys Double
  deriving stock   (Generic, Data)
  deriving newtype (Eq, Ord, RealFloat, RealFrac, Real, Floating, Enum, Fractional, Num, Read)
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
twF k ohms farads = fromRational . toRational         -- Convert from Double to seconds.
                  $ (k * rt * cpF * ( 1 + 0.7 / rt )) -- This expression produces Double in ns.
                  / 1000000000                        -- Convert from ns to seconds.
  where
    rt :: Double
    rt = fromRational . toRational $ ohms / 1000.0

    cpF :: Double
    cpF = fromRational . toRational
        $ farads * recip pf -- Convert to picofarads from farads.

hzDutyToSeconds :: Hertz -> Duty -> Seconds
hzDutyToSeconds hertz duty = fromRational $ toRational dutySeconds
  where
    period = fromRational . toRational $ recip hertz
    dutySeconds = period * duty
        
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

-- | capacitors uses (farads * (1/picofarad)) for rounding. It converts back to farads with (*picofarad).
capacitors :: [] Farads
capacitors = map (* pf) . map (fromIntegral . round) . map (* (recip pf)) $ nfs ++ pfs
  where
    nfs = map (* nf) [ 1, 1.2, 1.5, 1.8, 2, 2.2, 3.3, 4.7, 5.6, 6.8, 8.2, 10, 22, 100 ]
    pfs = map (* pf) [ 800, 750, 680, 470, 330, 220, 120, 100, 82 ]

resistors :: [] Ohms
resistors =
  [ 47.0    , 100.0   , 250.0   , 500.0   , 1000.0  , 2500.0  , 2700.0
  , 3000.0  , 5000.0  , 5300.0  , 5600.0  , 7150.0  , 7320.0  , 7320.0
  , 7500.0  , 7680.0  , 7870.0  , 7870.0  , 8000.0  , 8200.0  , 8250.0
  , 8450.0  , 8870.0  , 9090.0  , 9100.0  , 9310.0  , 9530.0  , 9760.0
  , 10200.0 , 10500.0 , 10700.0 , 11000.0 , 11300.0 , 20500.0 , 30000.0
  , 40200.0 , 47000.0 , 56200.0
  ]

crossProduct :: [] a -> [[a]]
crossProduct pp = do
  a0 <- pp
  a1 <- pp
  pure [a0,a1]

parallelOhmsGeometry :: [] (Geometry Ohms)
parallelOhmsGeometry = map parallelGeometry $ crossProduct resistors

seriesOhmsGeometry :: [] (Geometry Ohms)
seriesOhmsGeometry = map seriesGeometry $ crossProduct resistors

parallelFaradsGeometry :: [] (Geometry Farads)
parallelFaradsGeometry = map parallelGeometry $ crossProduct capacitors

seriesFaradsGeometry :: [] (Geometry Farads)
seriesFaradsGeometry = map seriesGeometry $ crossProduct capacitors

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

-- | multivibrator must used a Defined strategy as the enums are huge?
multivibrator :: [] Ohms -> [] Farads -> IO (Maybe ([] Ohms, [] Farads))
multivibrator os fs = error ""
  -- do
  -- foo <- and'
  --   [ rt .>= Ohms 5000
  --   , rt .<  Ohms 250000
  --   , tw .>= 40 * nf
  --   ]
  where
    nonPolarized = 0.28
    -- twF k rt c = k * rt * c * ( 1 + 0.7 / rt )

    -- strat :: Config Holmes (Intersect Ohms)
    -- strat = using $ map Intersect os


