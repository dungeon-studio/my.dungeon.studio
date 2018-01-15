module Client.Eval
( handleClient
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
import Run (Run, AFF, EFF, liftAff, liftEff)

import Auth0 (Session(..))
import Auth0.Algebra (AUTH0, getSession)
import Client.Algebra (ClientDSLF(..), Resource(..))

handleClient
  :: forall r
   . ClientDSLF
  ~> Run ( auth0 :: AUTH0, aff :: AFF AppEffects, eff :: EFF AppEffects | r )
handleClient (Log s a) = do
  liftEff $ log s
  pure a

handleClient (Get AllCharacters a) = do
  session <- getSession
  case session of
    Nothing -> pure (a Nothing)
    Just (Session s) -> do
      liftEff $ log $ s.accessToken
      res <- liftAff $ attempt $ affjax $ defaultRequest
        { url = "http://r.api.dungeon.studio/characters"
        , method = Left GET
        , headers = [ RequestHeader "Accept" "application/vnd.siren+json"
                    , RequestHeader "Bearer" s.accessToken
                    ]
        }
      case res of
        Left err -> do
            liftEff $ log $ show err
            pure (a Nothing)
        Right res' -> pure (a (Just res'))
