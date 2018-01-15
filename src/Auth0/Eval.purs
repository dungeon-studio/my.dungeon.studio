module Auth0.Eval
( handleAuth0
, runAuth0
) where

import Prelude
import Control.Monad.App (AppEffects)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Now (now)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Data.Newtype (unwrap)
import DOM.HTML (window)
import DOM.HTML.Location as Location
import DOM.HTML.Window as Window
import DOM.WebStorage (getItem, removeItem, setItem, getLocalStorage)
import Run (Run, AFF, EFF, interpret, liftEff, liftAff, on, send)
import Run.Reader (READER, ask)

import Auth0 (Session(..), authorize, parseHash, sessionKey)
import Auth0.Algebra (Auth0DSLF(..), AUTH0, _auth0)
import State (State)

runAuth0
  :: Run ( auth0 :: AUTH0, reader :: READER State, aff :: AFF AppEffects, eff :: EFF AppEffects )
  ~> Run ( aff :: AFF AppEffects, eff :: EFF AppEffects, reader :: READER State )
runAuth0 = interpret (on _auth0 handleAuth0 send)

handleAuth0
  :: forall r
   . Auth0DSLF
  ~> Run ( aff :: AFF AppEffects, eff :: EFF AppEffects, reader :: READER State | r )
handleAuth0 (Authorize a) = do
  st <- ask
  liftEff $ authorize st.webAuth
  pure a

handleAuth0 (Logout a) = do
  ls <- liftEff getLocalStorage
  liftEff $ removeItem ls sessionKey
  liftEff $ window >>= Window.location >>= Location.reload
  pure a

handleAuth0 (GetSession a) = do
  ls <- liftEff getLocalStorage
  session <- liftEff $ getItem ls sessionKey
  pure (a session)

handleAuth0 (SetSession (Session session) a) = do
  time <- liftEff now
  let s = Session $ session{ expiresAt = unwrap $ (unInstant time) + (Milliseconds $ session.expiresIn * 1000.0) }
  ls <- liftEff getLocalStorage
  liftEff $ setItem ls sessionKey s
  liftEff $ window >>= Window.location >>= Location.replace "/"
  pure a

handleAuth0 (ParseHash a) = do
  st <- ask
  session <- liftAff $ attempt $ parseHash st.webAuth
  case session of
    Left _ -> pure $ a Nothing
    Right Nothing -> pure $ a Nothing
    Right (Just s) -> do
      ls <- liftEff $ getLocalStorage
      liftEff $ setItem ls sessionKey s
      pure $ a (Just s)

handleAuth0 (CheckAuth a) = do
  ls <- liftEff $ getLocalStorage
  session <- liftEff $ getItem ls sessionKey
  case session of
    Nothing -> pure (a false)
    (Just (Session s)) -> do
      time <- liftEff now
      let isLoggedIn = (unwrap $ unInstant time) < s.expiresAt
      pure (a isLoggedIn)
