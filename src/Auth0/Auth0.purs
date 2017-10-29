module Auth0
( AUTH0EFF
, Auth0Config(..)
, LSKey(..)
, Session
, WebAuth
, authorize
, parseHash
, sessionKey
, webAuth
) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff(), kind Effect)
import Data.Generic (class Generic)

foreign import data AUTH0EFF  :: Effect
foreign import data WebAuth   :: Type
foreign import webAuth :: Auth0Config -> WebAuth
foreign import authorize :: forall eff. WebAuth -> Eff ( auth0 :: AUTH0EFF | eff ) Unit
foreign import _parseHash :: forall eff. WebAuth -> EffFnAff ( auth0 :: AUTH0EFF | eff ) Session

parseHash :: forall eff. WebAuth -> Aff ( auth0 :: AUTH0EFF | eff ) Session
parseHash = fromEffFnAff <<< _parseHash

newtype Session = Session
  { access_token :: String
  , id_token :: String
  , expires_at :: String
  }

derive instance genericSession :: Generic Session

data LSKey a = SessionKey
derive instance genericLSKey :: Generic (LSKey a)

sessionKey :: LSKey Session
sessionKey = SessionKey

newtype Auth0Config = Auth0Config
  { domain :: String
  , clientID :: String
  , responseType :: String
  , audience :: String
  , scope :: String
  , redirectUri :: String
  }
