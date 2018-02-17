module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import DungeonStudio.DSL.Auth0.Core (WebAuth, getWebAuth)
import DungeonStudio.DSL.Auth0.Eval (runAuth0)
import DungeonStudio.DSL.Client.Eval (runClient)
import DungeonStudio.Env (Env, getEnv)
import DungeonStudio.Components.Container as Container
import DungeonStudio.Control.Monad (AppEffects, AppM)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Run (runBaseAff')
import Run.Reader (runReader)

runApp
  :: Env
  -> WebAuth
  -> AppM
  ~> Aff AppEffects
runApp env wa =
  runBaseAff'
  <<< runReader env
  <<< runAuth0 wa
  <<< runClient

main :: Eff AppEffects Unit
main = HA.runHalogenAff do
  case getEnv of
    Left es -> liftEff $ error $ "Environment not configured properly: " <> show es
    Right env ->  do
      wa <- liftEff $ getWebAuth env -- Auth0 WebAuth instance
      body <- HA.awaitBody
      app <- runUI (H.hoist (runApp env wa) Container.component) unit body
      liftEff $ Container.matchRoutes app
