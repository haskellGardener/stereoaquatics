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

import Data.Char            ( isNumber, toUpper )
import Data.Default         ( Default(..) )
import Data.Data            ( Data, Typeable )
import Data.Ix              ( Ix )
import Data.List            ( sortBy )
import Data.Text            ( Text )
import Data.Word            ( Word8 )
import Data.Bits            ( Bits, FiniteBits )
import GHC.Generics         ( Generic )
import GHC.Num              ( Num )
import Foreign.Storable     ( Storable )

import Data.Holmes
import Prelude hiding (String)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

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

newtype Ohms = Ohms Double
  deriving stock   (Generic, Data)         -- Should Show be in stock it will show "Ohms 0", not just numerals.
  deriving newtype (Eq, Ord, RealFrac, Real, Floating, Enum, Fractional, Num, Read, Show) -- deriving newtype Show is just numerals (e.g. "0"), not "Ohms 0" etc.

newtype Watts = Watts Double
  deriving stock   (Generic, Data)
  deriving newtype (Eq, Ord, RealFrac, Real, Floating, Enum, Fractional, Num, Read, Show)

newtype Tolerance = Tolerance Double
  deriving stock   (Generic, Data)
  deriving newtype (Eq, Ord, RealFrac, Real, Floating, Enum, Fractional, Num, Read, Show)

newtype Leads = Leads Word8
  deriving stock   (Generic, Data)
  deriving newtype (Integral, Ix, Bounded, FiniteBits, Bits, Storable, Eq, Ord, Real, Enum, Num, Read, Show)
