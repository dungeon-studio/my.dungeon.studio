module State
( State
) where

import Auth0 (WebAuth)
import Env (Env)

type State =
  { env :: Env
  , webAuth :: WebAuth
  }

