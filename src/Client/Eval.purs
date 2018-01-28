module Client.Eval
( handleClient
, runClient
) where

import Prelude
import Control.Monad.App (AppEffects)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Optic.Getter ((^.))
import Run (Run, AFF, EFF, interpret, liftAff, liftEff, on, send)
import Run.Reader (READER, ask)

import Auth0 (Session(..))
import Auth0.Algebra (AUTH0, getSession)
import Client.Algebra (ClientDSLF(..), CLIENT, Resource(..), _client)
import Env (apiHost)
import State (State)

runClient
  :: Run ( client :: CLIENT, auth0 :: AUTH0, aff :: AFF AppEffects, eff :: EFF AppEffects, reader :: READER State )
  ~> Run ( auth0 :: AUTH0, reader :: READER State, aff :: AFF AppEffects, eff :: EFF AppEffects )
runClient = interpret (on _client handleClient send)

handleClient
  :: forall r
   . ClientDSLF
  ~> Run ( auth0 :: AUTH0, reader :: READER State, aff :: AFF AppEffects, eff :: EFF AppEffects | r )
handleClient (Log s a) = do
  liftEff $ log s $> a

handleClient (Get AllCharacters a) = do
  st <- ask
  session <- getSession
  case session of
    Nothing -> pure (a Nothing)
    Just (Session s) -> do
      res <- liftAff $ attempt $ affjax $ defaultRequest
        { url = "http://" <> st.env ^. apiHost <> "/characters"
        , method = Left GET
        , headers = [ RequestHeader "Accept" "application/vnd.siren+json"
                    , RequestHeader "Authorization" $ "Bearer " <> s.accessToken
                    ]
        }
      case res of
        Left err -> do
            liftEff $ log $ show err
            pure (a Nothing)
        Right res' -> pure (a (Just res'))
