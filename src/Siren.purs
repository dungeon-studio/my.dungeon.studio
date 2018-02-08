module Siren
( _entities
, getLinkByRel
)
where

import Prelude
import Data.Array (elem, head, filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Optic.Getter ((^.))
import Optic.Lens (lens)
import Optic.Types (Lens')
import Siren.Types (Action(..), Entity(..), Link(..), SubEntity)

type Rel = String
type Name = String

_links :: Lens' Entity (Array Link)
_links = lens (\(Entity e) -> e.links) (\(Entity e) v -> Entity e { links = v })

_actions :: Lens' Entity (Array Action)
_actions = lens (\(Entity e) -> fromMaybe [] e.actions) (\(Entity e) v -> Entity e { actions = Just v })

_entities :: Lens' Entity (Array SubEntity)
_entities = lens (\(Entity e) -> fromMaybe [] e.entities) (\(Entity e) v -> Entity e { entities = Just v })

getLinkByRel :: Entity -> String -> Maybe Link
getLinkByRel e rel = head $ e ^. _links # filter (\(Link l) -> rel `elem` l.rel)

getActionByName :: Entity -> String -> Maybe Action
getActionByName e n = head $ e ^. _actions # filter (\(Action a) -> a.name == n)
