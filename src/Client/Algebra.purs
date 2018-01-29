module Client.Algebra
( ClientDSLF(..)
, Resource(..)
, CLIENT
, _client
, getChars
, log
) where

import Prelude
import Run (Run, lift)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Network.HTTP.Affjax (AffjaxResponse)

data ClientDSLF a
  = Log String a
  | Get Resource (Maybe (AffjaxResponse Foreign) -> a)

data Resource
  = AllCharacters

derive instance clientFunctor :: Functor ClientDSLF

type CLIENT = FProxy ClientDSLF

_client = SProxy :: SProxy "client"

log :: forall r. String -> Run (client :: CLIENT | r) Unit
log s = lift _client (Log s unit)

getChars :: forall r. Run (client :: CLIENT | r) (Maybe (AffjaxResponse Foreign))
getChars = lift _client (Get AllCharacters id)
