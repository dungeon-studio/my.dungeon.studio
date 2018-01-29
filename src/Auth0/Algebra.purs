module Auth0.Algebra
( Auth0DSLF(..)
, AUTH0
, _auth0
, authorize
, getSession
, isAuthenticated
, logout
, parseHash
, setSession
) where

import Prelude
import Run (Run, lift)
import Auth0 (Session)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant.Internal (FProxy)

data Auth0DSLF a
  = Authorize a
  | Logout a
  | GetSession ((Maybe Session) -> a)
  | SetSession Session a
  | ParseHash ((Maybe Session) -> a)
  | CheckAuth (Boolean -> a)
derive instance auth0Functor :: Functor Auth0DSLF

type AUTH0 = FProxy Auth0DSLF

_auth0 = SProxy :: SProxy "auth0"

authorize :: forall r. Run (auth0 :: AUTH0 | r) Unit
authorize = lift _auth0 (Authorize unit)

getSession :: forall r. Run (auth0 :: AUTH0 | r) (Maybe Session)
getSession = lift _auth0 (GetSession id)

setSession :: forall r. Session -> Run (auth0 :: AUTH0 | r) Unit
setSession s = lift _auth0 (SetSession s unit)

parseHash :: forall r. Run (auth0 :: AUTH0 | r) (Maybe Session)
parseHash = lift _auth0 (ParseHash id)

isAuthenticated :: forall r. Run (auth0 :: AUTH0 | r) Boolean
isAuthenticated = lift _auth0 (CheckAuth id)

logout :: forall r. Run (auth0 :: AUTH0 | r) Unit
logout = lift _auth0 (Logout unit)
