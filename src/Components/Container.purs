module Components.Container
( component
, matchRoutes
, Query(..)
) where

import Prelude

import Auth0.Algebra as Auth0
import Components.Characters as Characters
import Components.Login as Login
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.App (AppM)
import Control.Monad.Eff (Eff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP
import Routers as RT
import Routing (matches)

data Query a = Init a | Logout a | ChangeRoute RT.Routes a
data AuthStatus = Authenticated | NotAuthenticated | Loading
type State = { auth :: AuthStatus, route :: RT.Routes }
type Input = Unit
type Output = Void
type ChildQuery = Coproduct2 Login.Query Characters.Query
type ChildSlot = Either2 Unit Unit
type Monad = AppM

headerClass :: HH.ClassName
headerClass = HH.ClassName "f6 lh-copy tl ttu tracked-mega sans-serif avenir white pa3"

buttonClass :: HH.ClassName
buttonClass = HH.ClassName "f6 pointer near-white bg-animate bg-near-black hover-bg-gray tc pa2 ph3 pv1 ttu tracked"

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
  initialState = { auth: Loading, route: RT.Characters }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Monad
  render st = HH.div [ HP.class_ $ HH.ClassName "w-100 vh-100" ]
    [ case st.auth of
        NotAuthenticated -> HH.slot' CP.cp1 unit Login.component unit absurd
        Authenticated -> HH.div_ [ header, content st ]
        Loading -> HH.div_ []
    ]

  header = HH.header
    [ HP.class_ $ HH.ClassName "bg-black-90 top-0 w-100 ph3 pv4 pv4-ns ph4-m ph5-l" ]
    [ HH.nav
      [ HP.class_ $ HH.ClassName "f6 fw6 ttu tracked" ]
      [
        HH.a
          [ HP.class_ $ HH.ClassName "link dim white dib mr3", HP.href "#/" ]
          [ HH.text "Characters" ]
      , HH.a
          [ HP.class_ $ HH.ClassName "link fr dim white dib", HP.href "#", HE.onClick (HE.input_ Logout) ]
          [ HH.text "Logout" ]
      ]
    ]

  content st = HH.div [ HP.class_ $ HH.ClassName "pa3" ]
    [ case st.route of
        RT.Characters -> HH.slot' CP.cp2 unit Characters.component unit absurd
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output Monad
  eval = case _ of
    Init next -> do
      ms <- lift $ Auth0.parseHash -- TODO: Move this into a separate component
      case ms of
        Just session -> do
          lift $ Auth0.setSession session
          pure next
        Nothing -> do
          isLoggedIn <- lift $ Auth0.isAuthenticated
          H.modify _{ auth = if isLoggedIn then Authenticated else NotAuthenticated }
          pure next
    ChangeRoute route next -> do
      H.modify \st -> st{ route = route }
      pure next
    Logout next -> do
      lift $ Auth0.logout $> next

  initializer = Just $ H.action Init
  finalizer = Nothing

matchRoutes
  :: forall eff
   . H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
  -> Eff (HA.HalogenEffects eff) Unit
matchRoutes app = matches RT.routing (redirects app)
  where
    redirects driver _ = launchAff_ <<< driver.query <<< H.action <<< ChangeRoute