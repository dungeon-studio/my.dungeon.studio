module DungeonStudio.DSL.Auth0.Eval
( handleAuth0
, runAuth0
) where

import Prelude
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Now (now)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Data.Newtype (unwrap, wrap)
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window
import DOM.WebStorage (getItem, removeItem, setItem, getLocalStorage)
import DungeonStudio.DSL.Auth0.Core (Session(..), WebAuth, authorize, parseHash, sessionKey)
import DungeonStudio.DSL.Auth0.Algebra (Auth0DSLF(..), AUTH0, _auth0)
import DungeonStudio.Env (Env)
import DungeonStudio.Control.Monad (AppEffects)
import Run (Run, AFF, EFF, interpret, liftEff, liftAff, on, send)
import Run.Reader (READER)

runAuth0
  :: WebAuth
  -> Run ( auth0 :: AUTH0
         , aff :: AFF AppEffects
         , eff :: EFF AppEffects
         , reader :: READER Env
         )
  ~> Run ( aff :: AFF AppEffects
         , eff :: EFF AppEffects
         , reader :: READER Env
         )
runAuth0 wa = interpret (on _auth0 (handleAuth0 wa) send)

handleAuth0
  :: forall r
   . WebAuth
  -> Auth0DSLF
  ~> Run ( aff :: AFF AppEffects
         , eff :: EFF AppEffects
         , reader :: READER Env
         | r
         )
handleAuth0 wa (Authorize a) = do
  liftEff $ authorize wa
  pure a

handleAuth0 wa (Logout a) = do
  ls <- liftEff getLocalStorage
  liftEff $ removeItem ls sessionKey
  liftEff $ window >>= Window.location >>= Location.reload
  pure a

handleAuth0 wa (GetSession a) = do
  ls <- liftEff getLocalStorage
  session <- liftEff $ getItem ls sessionKey
  pure (a session)

handleAuth0 wa (SetSession (Session session) a) = do
  time <- liftEff now
  let ms = unInstant time
      exp = Milliseconds $ session.expiresIn * 1000.0
      s = wrap $ session{ expiresAt = unwrap $ ms + exp }
  ls <- liftEff getLocalStorage
  liftEff $ setItem ls sessionKey s
  liftEff $ window >>= Window.location >>= Location.replace "/"
  pure a

handleAuth0 wa (ParseHash a) = do
  session <- liftAff $ attempt $ parseHash wa
  case session of
    Left _ -> pure $ a Nothing
    Right Nothing -> pure $ a Nothing
    Right (Just s) -> do
      ls <- liftEff $ getLocalStorage
      liftEff $ setItem ls sessionKey s
      pure $ a (Just s)

handleAuth0 wa (CheckAuth a) = do
  ls <- liftEff $ getLocalStorage
  session <- liftEff $ getItem ls sessionKey
  case session of
    Nothing -> pure (a false)
    (Just (Session s)) -> do
      time <- liftEff now
      let isLoggedIn = (unwrap $ unInstant time) < s.expiresAt
      pure (a isLoggedIn)
