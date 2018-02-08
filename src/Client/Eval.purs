module Client.Eval
( handleClient
, runClient
) where

import Prelude
import Auth0 (Session(..))
import Auth0.Algebra (AUTH0, getSession)
import Client.Algebra (ClientDSLF(..), CLIENT, _client)
import Control.Monad.App (AppEffects)
import Data.Either (Either(..), hush)
import Data.Foreign (MultipleErrors)
import Data.Maybe (Maybe(..))
import Data.HTTP.Method (Method(..))
import Debug.Trace (traceAnyA)
import Env (Env(..))
import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Run (Run, AFF, EFF, interpret, liftAff, on, send)
import Run.Reader (READER, ask)
import Simple.JSON (class ReadForeign, readJSON, writeJSON)
import Siren.Types (Entity)

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

handleClient (MakeCharacter c a) = do
  traceAnyA $ writeJSON c
  (Env env) <- ask
  session <- getSession
  case session of
    Nothing -> pure (a Nothing)
    Just (Session s) -> do
      res <- liftAff $ affjax $ defaultRequest
        { url = "http://" <> env.apiHost <> "/characters"
        , method = Left POST
        , content = Just $ writeJSON c
        , headers = [ RequestHeader "Authorization" $ "Bearer " <> s.accessToken
                    , RequestHeader "Content-Type" "application/json"
                    ]
        }
      traceAnyA $ (parse res.response) :: Either MultipleErrors Entity
      pure $ a $ hush $ parse res.response

handleClient (GetCharacters a) = do
  (Env env) <- ask
  session <- getSession
  case session of
    Nothing -> pure (a Nothing)
    Just (Session s) -> do
      res <- liftAff $ affjax $ defaultRequest
        { url = "http://" <> env.apiHost <> "/characters"
        , method = Left GET
        , headers = [ RequestHeader "Authorization" $ "Bearer " <> s.accessToken
                    , RequestHeader "Content-Type" "application/vnd.siren+json"
                    ]
        }
      traceAnyA $ (parse res.response) :: Either MultipleErrors Entity
      pure $ a $ hush $ parse res.response

parse :: forall a. ReadForeign a => String -> Either MultipleErrors a
parse = readJSON
