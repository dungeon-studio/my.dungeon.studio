module DungeonStudio.Routes
( Route(..)
, routing
) where

import Control.Alt ((<$), (<|>))
import Prelude (class Show, Unit, (*>))
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Route = Characters | CharacterCreate

oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

characters :: Match Route
characters = Characters <$ (homeSlash *> lit "characters")

createCharacter :: Match Route
createCharacter = CharacterCreate <$ (homeSlash *> lit "characters" *> lit "create")

instance showRoutes :: Show Route where
  show Characters = "Characters"
  show CharacterCreate = "Create Character"

routing :: Match Route
routing = createCharacter <|> characters
