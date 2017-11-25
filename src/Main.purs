module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import DOM (DOM)
import DOM.WebStorage (STORAGE)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Auth0 (AUTH0EFF, Auth0Config(..), webAuth)
import Auth0.Eval (runAuth0)
import Container (component)

auth0Config :: Auth0Config
auth0Config = Auth0Config
  { domain: "alunduil.auth0.com"
  , clientID: "qCOuPm76SHhtqUY1dA29TWL4CGt0VJNU"
  , responseType: "token id_token"
  , audience: "https://alunduil.auth0.com/userinfo"
  , scope: "openid"
  , redirectUri: "http://localhost:5000"
  }

main :: Eff ( HA.HalogenEffects ( auth0 :: AUTH0EFF, dom :: DOM, storage :: STORAGE, now :: NOW ) ) Unit
main = HA.runHalogenAff do
  let auth0 = webAuth auth0Config
  runUI (H.hoist (runAuth0 auth0) component) unit =<< HA.awaitBody
