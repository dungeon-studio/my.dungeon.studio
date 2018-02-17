module DungeonStudio.DSL.Client.Algebra
( ClientDSLF(..)
, CLIENT
, Payload(..)
, ResponseType(..)
, _client
, getRoot
, resolveAction
, resolveLink
) where

import Prelude
import Control.Alt ((<|>))
import Data.CollectionJSON (CollectionJSON)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Siren.Types (Action, Entity, Link)
import Data.StrMap (StrMap)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)
import Run (Run, lift)
import Simple.JSON (class ReadForeign, read)

data ResponseType = REntity Entity | RCollectionJSON CollectionJSON
instance rfRT :: ReadForeign ResponseType where
  readImpl f
    = REntity <$> read f
    <|> RCollectionJSON <$> read f

newtype Payload = Payload (StrMap String)
derive instance ntP :: Newtype Payload _

data ClientDSLF a
  = GetRoot String (Maybe Entity -> a)
  | ResolveLink Link (Maybe ResponseType -> a)
  | ResolveAction Action Payload (Maybe ResponseType -> a)

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
  -> Payload
  -> Run (client :: CLIENT | r) (Maybe ResponseType)
resolveAction a p = lift _client (ResolveAction a p id)
