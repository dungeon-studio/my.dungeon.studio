module Data.Siren
( _actions
, _entities
, _fields
, _href
, _rel
, _ActionName
, _FieldName
, getActionByName
, getLinkByRel
, sirenMime
)
where

import Data.Array (elem, find)
import Data.Lens.Iso (Iso, iso)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Lens (lens)
import Data.Lens.Getter ((^.))
import Data.Lens.Types (Lens')
import Data.Siren.Types
  ( Action
  , Entity
  , Field
  , Link
  , SubEntity
  )
import Prelude ((<<<), (#), (==))

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

_href :: Lens' Link String
_href = _Newtype <<< lens (_.href) (_ {href = _ })

_rel :: Lens' Link (Array String)
_rel = _Newtype <<< lens (_.rel) (_ {rel = _ })

_ActionName :: Lens' Action String
_ActionName = _Newtype <<< lens (_.name) (_ {name = _ })

_FieldName :: Lens' Field String
_FieldName = _Newtype <<< lens (_.name) (_ {name = _ })

getLinkByRel :: Entity -> String -> Maybe Link
getLinkByRel e rel = e ^. _links # find (\l -> rel `elem` (l ^. _rel))

getActionByName :: Entity -> String -> Maybe Action
getActionByName e name = e ^. _actions # find (\a -> a ^. _ActionName == name)
