

module Relay
where

newtype Volt = Volt Int
  deriving (Ord, Eq, Show, Num)

data Switch = Switch
  { voltage :: Volt
  , closed  :: Bool
  }

data LatchRelay = LatchRelay
  { supply    :: Volt
  , set       :: Bool
  , reset     :: Bool
  , contactA  :: Switch
  , contactB  :: Switch
  , relayName :: Text
  }

data LatchPair = LatchPair
  { latchA :: LatchRelay
  , latchB :: LatchRelay
  }


{-
MCP23S09E



Relay 1 controls rail power to all relay contacts/circuits in relay 2.
Relay 1 MUST NOT control/feed power to relay 2 coils.


-}               
