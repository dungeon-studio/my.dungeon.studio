module Client.Algebra
( ClientDSLF(..)
, CLIENT
, ResponseType(..)
, _client
, getRoot
, resolveAction
, resolveLink
) where

import Prelude
import CollectionJSON (CollectionJSON)
import Control.Alt ((<|>))
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Run (Run, lift)
import Simple.JSON (class ReadForeign, read)
import Siren.Types (Action, Entity, Link)

data ResponseType = REntity Entity | RCollectionJSON CollectionJSON
instance rfRT :: ReadForeign ResponseType where
  readImpl f
    = REntity <$> read f
    <|> RCollectionJSON <$> read f

data ClientDSLF a
  = GetRoot String (Maybe Entity -> a)
  | ResolveLink Link (Maybe ResponseType -> a)
  | ResolveAction Action (Maybe ResponseType -> a)

derive instance clientFunctor :: Functor ClientDSLF

type CLIENT = FProxy ClientDSLF

_client = SProxy :: SProxy "client"

getRoot
  :: forall r
   . String
  -> Run (client :: CLIENT | r) (Maybe Entity)
getRoot uri = lift _client (GetRoot uri id)

resolveLink
  :: forall r
   . Link
  -> Run (client :: CLIENT | r) (Maybe ResponseType)
resolveLink l = lift _client (ResolveLink l id)

resolveAction
  :: forall r
   . Action
  -> Run (client :: CLIENT | r) (Maybe ResponseType)
resolveAction a = lift _client (ResolveAction a id)
