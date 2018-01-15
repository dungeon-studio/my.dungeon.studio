module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Auth0 as Auth0
import Auth0.Eval (runAuth0)
import Client.Eval (runClient)
import Container as Container
import Control.Monad.App (AppEffects, AppM)
import Env (getEnv)
import State (State)
import Run (runBaseAff')
import Run.Reader (runReader)

runApp
  :: State
  -> AppM
  ~> Aff AppEffects
runApp st = runBaseAff' <<< runReader st <<< runAuth0 <<< runClient

main :: Eff AppEffects Unit
main = HA.runHalogenAff do
  case getEnv of
    Left es -> liftEff $ error $ "Environment not configured properly: " <> show es
    Right env ->  do
      webAuth <- liftEff $ Auth0.init env
      body <- HA.awaitBody
      app <- runUI (H.hoist (runApp { env, webAuth }) Container.component) unit body
      liftEff $ Container.matchRoutes app
