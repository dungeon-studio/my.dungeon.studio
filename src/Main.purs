module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import DOM.WebStorage (STORAGE)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Auth0 (AUTH0EFF, Auth0Config, webAuth)
import Auth0.Eval (runAuth0)
import Container (component)

foreign import auth0Config :: Auth0Config

main :: Eff ( HA.HalogenEffects ( auth0 :: AUTH0EFF, dom :: DOM, storage :: STORAGE, now :: NOW ) ) Unit
main = HA.runHalogenAff do
  let auth0 = webAuth auth0Config
  runUI (H.hoist (runAuth0 auth0) component) unit =<< HA.awaitBody
