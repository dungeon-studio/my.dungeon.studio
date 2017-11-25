module Component where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Run (Run)

import Auth0.Algebra (AUTH0, authorize, isAuthenticated, parseHash, setSession)

data Query a = Init a | Login a | Logout a
data State = Authenticated | NotAuthenticated
type Monad = Run ( auth0 :: AUTH0 )

component :: H.Component HH.HTML Query Unit Void Monad
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer
    , finalizer
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = NotAuthenticated

  render :: State -> H.ComponentHTML Query
  render Authenticated =
    HH.div_
      [
        HH.button
          [ HE.onClick (HE.input_ Logout) ]
          [ HH.text "Logout" ]
      ]
  render NotAuthenticated =
    HH.div_
      [
        HH.button
          [ HE.onClick (HE.input_ Login) ]
          [ HH.text "Login" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Monad
  eval = case _ of
    Init next -> do
      ms <- lift $ parseHash
      case ms of
        Just session -> do
          lift $ setSession session
          H.put Authenticated
          pure next
        Nothing -> do
          isLoggedIn <- lift $ isAuthenticated
          if isLoggedIn
            then do
              H.put Authenticated
              pure next
            else do
              pure next
    Login next -> do
       lift $ authorize
       pure next
    Logout next -> do
       pure next

  initializer = Just $ H.action Init
  finalizer = Nothing
