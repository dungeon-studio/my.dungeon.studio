module DungeonStudio.DSL.Auth0.Core
( AUTH0EFF
, Auth0Config
, LSKey(..)
, Session(..)
, WebAuth
, authorize
, getWebAuth
, parseHash
, sessionKey
) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff(), kind Effect)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window
import DungeonStudio.Env (Env(..))
import Prelude (Unit, ($), (<<<), (>>=), bind, pure)

type Auth0Config =
  { clientID :: String
  , domain :: String
  , redirectUri :: String
  , audience :: String
  , scope :: String
  , responseType :: String
  }

foreign import data AUTH0EFF  :: Effect
foreign import data WebAuth   :: Type

foreign import webAuth :: Auth0Config -> WebAuth

foreign import authorize
  :: forall eff
   . WebAuth
  -> Eff ( auth0 :: AUTH0EFF | eff ) Unit

foreign import _parseHash
  :: forall a eff
   . (a -> Maybe a)
  -> Maybe a
  -> WebAuth
  -> EffFnAff ( auth0 :: AUTH0EFF | eff ) (Maybe Session)

getWebAuth :: forall e. Env -> Eff ( dom :: DOM | e) WebAuth
getWebAuth (Env env) = do
  origin <- window >>= Window.location >>= Location.origin
  pure $ webAuth
    { clientID: "YCjAwXOrmOCvnmDPUPxY6DkB8qN28R8m"
    , domain: "alunduil.auth0.com"
    , redirectUri: origin
    , audience: env.auth0Audience
    , scope: "read:characters create:characters delete:characters"
    , responseType: "token"
    }

parseHash
  :: forall eff
   . WebAuth
  -> Aff ( auth0 :: AUTH0EFF | eff ) (Maybe Session)
parseHash = fromEffFnAff <<< _parseHash Just Nothing

-- Session newtype - need it to derive Generic instance
newtype Session = Session
  { accessToken :: String
  , expiresAt :: Number
  , expiresIn :: Number
  }
derive instance genericSession :: Generic Session
derive instance ntS :: Newtype Session _

-- LocalStorage keys (only one for now: the key which refers to our Session)
-- `a` is a phantom type and lets keys encode the type of the data stored
-- See https://github.com/eskimor/purescript-localstorage for more info
data LSKey a = SessionKey
derive instance genericLSKey :: Generic (LSKey a)

sessionKey :: LSKey Session
sessionKey = SessionKey
