module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Functor.Variant (on)
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Auth0 (WebAuth, webAuth)
import Auth0.Algebra (_auth0)
import Auth0.Eval (handleAuth0)
import Client.Algebra (_client)
import Client.Eval (handleClient)
import Container as Container
import Control.Monad.App (AppEffects, AppM)
import Run (interpret, runBaseAff', send)

runApp
  :: WebAuth
  -> AppM
  ~> Aff AppEffects
runApp wa = runBaseAff'
  <<< interpret (on _auth0 (handleAuth0 wa) send)
  <<< interpret (on _client handleClient send)

main :: Eff AppEffects Unit
main = HA.runHalogenAff do
  origin <- liftEff $ window >>= Window.location >>= Location.origin
  let auth0 = webAuth { clientID: "qCOuPm76SHhtqUY1dA29TWL4CGt0VJNU"
                      , domain: "alunduil.auth0.com"
                      , redirectUri: origin
                      , responseType: "token"
                      }
  body <- HA.awaitBody
  app <- runUI (H.hoist (runApp auth0) Container.component) unit body
  liftEff $ Container.matchRoutes app
