module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window
import DOM.WebStorage (STORAGE)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Auth0 (AUTH0EFF, webAuth)
import Auth0.Eval (runAuth0)
import Container as Container

main :: Eff ( HA.HalogenEffects ( auth0 :: AUTH0EFF, dom :: DOM, storage :: STORAGE, now :: NOW ) ) Unit
main = HA.runHalogenAff do
  origin <- liftEff $ window >>= Window.location >>= Location.origin
  let auth0 = webAuth { clientID: "qCOuPm76SHhtqUY1dA29TWL4CGt0VJNU"
                      , domain: "alunduil.auth0.com"
                      , redirectUri: origin
                      , responseType: "token"
                      }
  body <- HA.awaitBody
  app <- runUI (H.hoist (runAuth0 auth0) Container.component) unit body
  liftEff $ Container.matchRoutes app
