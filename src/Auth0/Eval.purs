module Auth0.Eval
( handleAuth0
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
import Run (Run, AFF, EFF, liftEff, liftAff)

import Auth0 (WebAuth, Session(..), authorize, parseHash, sessionKey)
import Auth0.Algebra (Auth0DSLF(..))

handleAuth0
  :: forall r
   . WebAuth
  -> Auth0DSLF
  ~> Run ( aff :: AFF AppEffects, eff :: EFF AppEffects | r )
handleAuth0 wa (Ask a) = pure (a wa)

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
  let s = Session $ session{ expiresAt = unwrap $ (unInstant time) + (Milliseconds $ session.expiresIn * 1000.0) }
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
