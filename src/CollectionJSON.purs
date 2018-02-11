module CollectionJSON
( CollectionJSON(..)
, Collection(..)
, Link(..)
, Item(..)
, Query(..)
, Template(..)
, Error(..)
, Datum(..)
, collectionMime
) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign)

collectionMime :: String
collectionMime = "application/vnd.collection+json"

-- * Core Data Types

newtype CollectionJSON = CollectionJSON { collection :: Collection }
derive instance ntCJSON :: Newtype CollectionJSON _
derive newtype instance rfCJSON :: ReadForeign CollectionJSON
derive instance rgCJSON :: Generic CollectionJSON _
instance showCJSON :: Show CollectionJSON where show e = genericShow e

-- | The top-level object for an @application/vnd.collection+json@ resource.
newtype Collection = Collection
  { version  :: String
  , href     :: String
  , links    :: Maybe (Array Link)
  , items    :: Maybe (Array Item)
  , queries  :: Maybe (Array Query)
  , template :: Maybe Template
  , error    :: Maybe Error
  }

derive instance ntC :: Newtype Collection _
derive newtype instance rfC :: ReadForeign Collection
derive instance rgC :: Generic Collection _
instance showC :: Show Collection where show e = genericShow e

newtype Link = Link
  { href   :: String
  , name   :: Maybe String
  , render :: Maybe String
  , prompt :: Maybe String
  }

derive instance ntL :: Newtype Link _
derive newtype instance rfL :: ReadForeign Link
derive instance rgL :: Generic Link _
instance showL :: Show Link where show e = genericShow e

-- | An element in the 'Collection'
newtype Item = Item
  { href    :: String
  , data    :: Maybe (Array Datum)
  , links   :: Maybe (Array Link)
  }

derive instance ntI :: Newtype Item _
derive newtype instance rfI :: ReadForeign Item
derive instance rgI :: Generic Item _
instance showI :: Show Item where show e = genericShow e

newtype Query = Query
  { href    :: String
  , rel     :: String
  , name    :: Maybe String
  , prompt  :: Maybe String
  , data    :: Maybe (Array Datum)
  }

derive instance ntQ :: Newtype Query _
derive newtype instance rfQ :: ReadForeign Query
derive instance rgQ :: Generic Query _
instance showQ :: Show Query where show e = genericShow e

-- | A fillable template for creation of a new object in the 'Collection'.
newtype Template = Template
  { data :: Maybe (Array Datum)
  }

derive instance ntT :: Newtype Template _
derive newtype instance rfT :: ReadForeign Template
derive instance rgT :: Generic Template _
instance showT :: Show Template where show e = genericShow e

-- | Information about latest error that occured when responding to a request.
newtype Error = Error
  { title   :: Maybe String
  , code    :: Maybe String
  , message :: Maybe String
  }

derive instance ntE :: Newtype Error _
derive newtype instance rfE :: ReadForeign Error
derive instance rgE :: Generic Error _
instance showE :: Show Error where show e = genericShow e

-- | Contents of a 'Collection' 'Item'.
newtype Datum = Datum
  { name    :: String
  , value   :: Maybe String
  , prompt  :: Maybe String
  }

derive instance ntD :: Newtype Datum _
derive newtype instance rfD :: ReadForeign Datum
derive instance rgD :: Generic Datum _
instance showD :: Show Datum where show e = genericShow e
