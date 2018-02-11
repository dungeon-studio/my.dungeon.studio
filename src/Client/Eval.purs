module Client.Eval
( handleClient
, runClient
) where

import Prelude
import Auth0 (Session(..))
import Auth0.Algebra (AUTH0, getSession)
import Client.Algebra (ClientDSLF(..), ResponseType, CLIENT, _client)
import Control.Monad.App (AppEffects)
import Data.Either (Either(..), hush)
import Data.Foreign (MultipleErrors)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.HTTP.Method (Method(..), fromString)
import Data.String (Pattern(..), contains)
{-- import Debug.Trace (traceAnyA) --}
import Env (Env(..))
import Network.HTTP.Affjax (affjax, defaultRequest, get)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Run (Run, AFF, EFF, interpret, liftAff, on, send)
import Run.Reader (READER, ask)
import Simple.JSON (readJSON, writeJSON)
import Siren.Types (Action(..), Link(..))

runClient
  :: Run ( client :: CLIENT
         , auth0 :: AUTH0
         , aff :: AFF AppEffects
         , eff :: EFF AppEffects
         , reader :: READER Env
         )
  ~> Run ( auth0 :: AUTH0
         , reader :: READER Env
         , aff :: AFF AppEffects
         , eff :: EFF AppEffects
         )
runClient = interpret (on _client handleClient send)

handleClient
  :: forall r
   . ClientDSLF
  ~> Run ( auth0 :: AUTH0
         , reader :: READER Env
         , aff :: AFF AppEffects
         , eff :: EFF AppEffects
         | r
         )

handleClient (GetRoot u a) = do
  (Env env) <- ask
  session <- getSession
  case session of
    Nothing -> pure $ a Nothing
    Just (Session s) -> do
      res <- liftAff $ affjax $ defaultRequest
        { url = "http://" <> env.apiHost <> u
        , method = Left GET
        , headers = [ RequestHeader "Authorization" $ "Bearer " <> s.accessToken
                    , RequestHeader "Content-Type" "application/vnd.siren+json"
                    ]
        }
      {-- traceAnyA $ readJSON (res.response) :: Either MultipleErrors Entity --}
      pure $ a $ hush $ readJSON res.response

-- TODO How do I know which are protected resources?
handleClient (ResolveLink (Link l) a) = do
  (Env env) <- ask
  if (Pattern "://") `contains` l.href
    then liftAff $ get l.href >>= _.response >>> parse >>> hush >>> a >>> pure
    else do
      session <- getSession
      case session of
        Nothing -> pure $ a Nothing
        Just (Session s) -> do
          res <- liftAff $ affjax $ defaultRequest
            { url = "http://" <> env.apiHost <> l.href
            , method = Left GET
            , headers = [ RequestHeader "Authorization" $ "Bearer " <> s.accessToken
                        , RequestHeader "Content-Type" $ fromMaybe "" l.type
                        ]
            }
      {-- traceAnyA $ readJSON (res.response) :: Either MultipleErrors ResponseType --}
          pure $ a $ hush $ parse res.response

handleClient (ResolveAction (Action action) a) = do
  (Env env) <- ask
  session <- getSession
  case session of
    Nothing -> pure $ a Nothing
    Just (Session s) -> do
      res <- liftAff $ affjax $ defaultRequest
        { url = "http://" <> env.apiHost <> action.href
        , method = maybe (Left GET) fromString action.method
        , content = Just $ writeJSON action.fields
        , headers = [ RequestHeader "Authorization" $ "Bearer " <> s.accessToken
                    , RequestHeader "Content-Type" $ fromMaybe "" action.type
                    ]
        }
      pure $ a $ hush $ parse res.response

-- TODO Make this throw on parse errors?
parse :: String -> Either MultipleErrors ResponseType
parse = readJSON
