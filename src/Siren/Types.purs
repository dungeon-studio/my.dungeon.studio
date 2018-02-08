module Siren.Types
( Entity(..)
, SubEntity(..)
, Link(..)
, Action(..)
, Field(..)
) where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.StrMap (StrMap)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, read)

-- | The top-level object for an @application/vnd.siren+json@ resource.
newtype Entity = Entity
  { class      :: Maybe (Array String)
  , properties :: Maybe (StrMap String)
  , entities   :: Maybe (Array SubEntity)
  , links      :: Array Link
  , actions    :: Maybe (Array Action)
  , title      :: Maybe String
  , rel        :: Maybe (Array String)
  }

derive instance ntE :: Newtype Entity _
derive newtype instance rfE :: ReadForeign Entity
derive instance rgE :: Rep.Generic Entity _
instance showE :: Show Entity where show e = genericShow e

-- | Nested object for an @application/vnd.siren+json@.
{-- type SubEntityRep = { entity :: Entity, rel :: Array String } --}
data SubEntity
  = EmbeddedLink Link
  | EmbeddedRepresentation Entity

derive instance rgSE :: Rep.Generic SubEntity _
instance showSE :: Show SubEntity where show e = genericShow e
instance rfSE :: ReadForeign SubEntity where
  readImpl f
    = EmbeddedLink <$> read f
    <|> EmbeddedRepresentation <$> read f

-- | Link to a related resource.
newtype Link = Link
  { class :: Maybe (Array String)
  , rel   :: Array String
  , href  :: String
  , type  :: Maybe String
  , title :: Maybe String
  }

derive instance ntL :: Newtype Link _
derive newtype instance rfL :: ReadForeign Link
derive instance rgL :: Rep.Generic Link _
instance showL :: Show Link where show = genericShow

-- | Behavior of an 'Entity'.
newtype Action = Action
  { name   :: String
  , class  :: Maybe (Array String)
  , method :: Maybe String
  , href   :: Maybe String
  , title  :: Maybe String
  , type   :: Maybe String
  , fields :: Maybe (Array Field)
  }

derive instance ntA :: Newtype Action _
derive newtype instance rfA :: ReadForeign Action
derive instance rgA :: Rep.Generic Action _
instance showA :: Show Action where show = genericShow

-- | Control inside of an 'Action'.
newtype Field = Field
  { name  :: String
  , class :: Maybe (Array String)
  , type  :: Maybe String
  , value :: Maybe String
  , title :: Maybe String
  }

derive instance ntF :: Newtype Field _
derive newtype instance rfF :: ReadForeign Field
derive instance rgF :: Rep.Generic Field _
instance showF :: Show Field where show = genericShow
