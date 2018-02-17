module DungeonStudio.Components.Container
( component
, matchRoutes
, Query(..)
) where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import DungeonStudio.DSL.Auth0.Algebra as Auth0
import DungeonStudio.Components.Entity as Entity
import DungeonStudio.Components.Login as Login
import DungeonStudio.CSS (css)
import DungeonStudio.Control.Monad (AppM)
import DungeonStudio.Routes as RT
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP
import Routing (matches)

data Query a = Init a | Logout a | ChangeRoute RT.Routes a
data AuthStatus = Authenticated | NotAuthenticated | Loading
type State = { auth :: AuthStatus, route :: RT.Routes }
type Input = Unit
type Output = Void
type ChildQuery = Coproduct2 Login.Query Entity.Query
type ChildSlot = Either2 Unit Unit
type Monad = AppM

headerClass :: String
headerClass = "f6 lh-copy tl ttu tracked-mega sans-serif avenir white pa3"

buttonClass :: String
buttonClass = "f6 pointer near-white bg-animate bg-near-black hover-bg-gray tc pa2 ph3 pv1 ttu tracked"

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
  render st = HH.div [ css "w-100 vh-100" ]
    [ case st.auth of
        NotAuthenticated -> HH.slot' CP.cp1 unit Login.component unit absurd
        Authenticated -> HH.div_ [ header, content st ]
        Loading -> HH.div_ []
    ]

  header = HH.header
    [ css "bg-black-90 top-0 w-100 ph3 pv4 pv4-ns ph4-m ph5-l" ]
    [ HH.nav
      [ css "f6 fw6 ttu tracked" ]
      [ HH.a
          [ css "link dim white dib mr3", HP.href "#/" ]
          [ HH.text "Characters" ]
      , HH.a
          [ css "link fr dim white dib", HP.href "#", HE.onClick (HE.input_ Logout) ]
          [ HH.text "Logout" ]
      ]
    ]

  content st = HH.div [ css "pa3" ]
    [ case st.route of
        RT.Characters -> HH.slot' CP.cp2 unit (Entity.component "/characters") unit absurd
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

-- TODO: Move routing into DSL
matchRoutes
  :: forall eff
   . H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
  -> Eff (HA.HalogenEffects eff) Unit
matchRoutes app = matches RT.routing (redirects app)
  where
    redirects driver _ = launchAff_ <<< driver.query <<< H.action <<< ChangeRoute
