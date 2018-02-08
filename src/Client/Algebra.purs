module Client.Algebra
( ClientDSLF(..)
, CLIENT
, NewCharacter(..)
, _client
, getCharacters
, makeCharacter
) where

import Prelude
import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Run (Run, lift)
import Simple.JSON (class WriteForeign)
import Siren.Types (Entity)

newtype NewCharacter = NewCharacter
  { race :: String
  , discipline :: String
  }

derive newtype instance wfNC :: WriteForeign NewCharacter
derive instance rgNC :: Generic NewCharacter _
instance showNC :: Show NewCharacter where show e = genericShow e

data ClientDSLF a
  = GetCharacters (Maybe Entity -> a)
  | MakeCharacter NewCharacter (Maybe Entity -> a)

derive instance clientFunctor :: Functor ClientDSLF

type CLIENT = FProxy ClientDSLF

_client = SProxy :: SProxy "client"

getCharacters :: forall r. Run (client :: CLIENT | r) (Maybe Entity)
getCharacters = lift _client (GetCharacters id)

makeCharacter :: forall r. NewCharacter -> Run (client :: CLIENT | r) (Maybe Entity)
makeCharacter c = lift _client (MakeCharacter c id)
