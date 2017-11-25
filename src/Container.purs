module Container where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Const (Const)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Halogen.Component.ChildPath as CP
import Login as Login
import Run (Run)

import Auth0.Algebra (AUTH0, isAuthenticated, parseHash, setSession)

data Query a = Init a
data State = Authenticated | NotAuthenticated | Loading

type Input = Unit
type Output = Void
type ChildQuery = Login.Query <\/> Const Void
type ChildSlot = Unit \/ Void
type Monad = Run ( auth0 :: AUTH0 )

headerClass :: HH.ClassName
headerClass = HH.ClassName "f6 lh-copy tl ttu tracked-mega sans-serif avenir white pa3"

component :: H.Component HH.HTML Query Input Output Monad
component =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer
    , finalizer
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = Loading

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Monad
  render st = HH.div [ HP.class_ $ HH.ClassName "w-100 vh-100" ]
    [
      HH.div [ HP.class_ headerClass ] [ HH.text "Dungeon Studio" ]
      , case st of
          NotAuthenticated -> HH.slot' CP.cp1 unit Login.component unit absurd
          Authenticated -> HH.div
            [ HP.class_ $ HH.ClassName "ttu tracked white f5 pa3" ]
            [ HH.text "Logged in"  ]
          Loading -> HH.div_ []
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output Monad
  eval = case _ of
    Init next -> do
      ms <- lift $ parseHash
      case ms of
        Just session -> do
          lift $ setSession session
          pure next
        Nothing -> do
          isLoggedIn <- lift $ isAuthenticated
          if isLoggedIn
            then do
              H.put Authenticated
              pure next
            else do
              H.put NotAuthenticated
              pure next

  initializer = Just $ H.action Init
  finalizer = Nothing
