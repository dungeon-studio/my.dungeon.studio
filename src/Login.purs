module Login
( Query
, component
) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Run (Run)

import Auth0.Algebra (AUTH0, authorize)

data Query a = Login a
type Input = Unit
type Output = Void
type State = Unit
type Monad = Run ( auth0 :: AUTH0 )

buttonClass :: HH.ClassName
buttonClass = HH.ClassName "f6 pointer near-white bg-animate bg-near-black hover-bg-gray tc ph4 pv2 ttu tracked"

component :: H.Component HH.HTML Query Input Output Monad
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render _ = HH.div [ HP.class_ $ HH.ClassName "dt vh-100 w-100" ]
    [ HH.div
        [ HP.class_ $ HH.ClassName "dtc v-mid tc white ph3 ph4-l" ]
        [
          HH.div [ HP.class_ $ HH.ClassName "f4 fw2 tracked ttu mb4" ] [ HH.text "Dungeon Studio" ]
        , HH.button
            [ HP.class_ buttonClass, HE.onClick (HE.input_ Login) ]
            [ HH.text "Login/Sign up" ]
        ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Output Monad
  eval = case _ of
    Login next -> do
      lift $ authorize
      pure next
