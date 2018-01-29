module Auth0
( AUTH0EFF
, Auth0Config
, LSKey(..)
, Session(..)
, WebAuth
, authorize
, init
, parseHash
, sessionKey
, webAuth
) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff(), kind Effect)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window
import Env (Env, auth0Audience)
import Optic.Getter ((^.))

foreign import data AUTH0EFF  :: Effect
foreign import data WebAuth   :: Type
foreign import webAuth :: Auth0Config -> WebAuth
foreign import authorize :: forall eff. WebAuth -> Eff ( auth0 :: AUTH0EFF | eff ) Unit
foreign import _parseHash :: forall a eff. (a -> Maybe a) -> Maybe a -> WebAuth -> EffFnAff ( auth0 :: AUTH0EFF | eff ) (Maybe Session)

init :: forall e. Env -> Eff ( dom :: DOM | e) WebAuth
init env = do
  origin <- window >>= Window.location >>= Location.origin
  pure $ webAuth
    { clientID: "YCjAwXOrmOCvnmDPUPxY6DkB8qN28R8m"
    , domain: "alunduil.auth0.com"
    , redirectUri: origin
    , audience: env ^. auth0Audience
    , scope: "read:characters"
    , responseType: "token"
    }

parseHash :: forall eff. WebAuth -> Aff ( auth0 :: AUTH0EFF | eff ) (Maybe Session)
parseHash = fromEffFnAff <<< _parseHash Just Nothing

data LSKey a = SessionKey
newtype Session = Session
  { accessToken :: String
  , expiresAt :: Number
  , expiresIn :: Number
  }
derive instance genericSession :: Generic Session
derive instance genericLSKey :: Generic (LSKey a)

sessionKey :: LSKey Session
sessionKey = SessionKey

type Auth0Config =
  { clientID :: String
  , domain :: String
  , redirectUri :: String
  , audience :: String
  , scope :: String
  , responseType :: String
  }
