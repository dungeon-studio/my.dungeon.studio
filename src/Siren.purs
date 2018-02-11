module Siren
( _actions
, _entities
, _fields
, sirenMime
)
where

import Prelude
import Data.Lens.Iso (Iso, iso)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Lens (lens)
import Data.Lens.Types (Lens')
import Siren.Types
  ( Action
  , Entity
  , Field
  , Link
  , SubEntity
  )

sirenMime :: String
sirenMime = "application/vnd.siren+json"

_Newtype :: forall t a s b. Newtype t a => Newtype s b => Iso t s a b
_Newtype = iso unwrap wrap

_links :: Lens' Entity (Array Link)
_links = _Newtype <<< lens (_.links) (_ {links = _ })

_actions :: Lens' Entity (Array Action)
_actions = _Newtype <<< lens (\e -> fromMaybe [] e.actions) (\e v -> e { actions = Just v })

_fields :: Lens' Action (Array Field)
_fields = _Newtype <<< lens (\a -> fromMaybe [] a.fields) (\a v -> a { fields = Just v })

_entities :: Lens' Entity (Array SubEntity)
_entities = _Newtype <<< lens (\e -> fromMaybe [] e.entities) (\e v -> e { entities = Just v })
