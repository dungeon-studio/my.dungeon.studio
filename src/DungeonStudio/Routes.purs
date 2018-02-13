module DungeonStudio.Routes where

import Control.Alt ((<$))
import Prelude (class Show)
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Routes = Characters

instance showRoutes :: Show Routes where
  show Characters = "Characters"

routing :: Match Routes
routing = characters
  where
    characters = Characters <$ lit ""
