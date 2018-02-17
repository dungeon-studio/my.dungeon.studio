module DungeonStudio.Control.Monad
( AppEffects
, AppM
) where

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import DOM.WebStorage (STORAGE)
import DungeonStudio.DSL.Auth0.Core (AUTH0EFF)
import DungeonStudio.DSL.Auth0.Algebra (AUTH0)
import DungeonStudio.DSL.Client.Algebra (CLIENT)
import DungeonStudio.Env (Env)
import Halogen.Aff as HA
import Run (Run, AFF, EFF)
import Run.Reader (READER)
import Network.HTTP.Affjax (AJAX)

type AppEffects = HA.HalogenEffects
  ( ajax :: AJAX
  , auth0 :: AUTH0EFF
  , console :: CONSOLE
  , dom :: DOM
  , now :: NOW
  , storage :: STORAGE
  )

type AppM = Run
  ( auth0 :: AUTH0
  , client :: CLIENT
  , aff :: AFF AppEffects
  , eff :: EFF AppEffects
  , reader :: READER Env
  )
