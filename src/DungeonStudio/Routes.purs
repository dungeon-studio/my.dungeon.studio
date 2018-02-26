module DungeonStudio.Routes
( Route(..)
, routing
) where

import Control.Alt ((<$))
import Prelude (class Show)
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Route = Characters

instance showRoutes :: Show Route where
  show Characters = "Characters"

routing :: Match Route
routing = characters
  where
    characters = Characters <$ lit ""
