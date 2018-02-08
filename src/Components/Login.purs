module Components.Login
( Query
, component
) where

import Prelude

import Control.Monad.App (AppM)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Auth0.Algebra (authorize)
import Assets (getImgSrc)

data Query a = Login a
type Input = Unit
type Output = Void
type State = Unit
type Monad = AppM

buttonClass :: HH.ClassName
buttonClass = HH.ClassName "f6 pointer white bg-animate bg-dark-gray hover-bg-gray tc bn ph3 mb3 pv2 ttu tracked"

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
          HH.img [ HP.class_ $ HH.ClassName "pa3 mw3 hover dim", HP.src $ getImgSrc "logo-splash.svg" ]
        , HH.div [ HP.class_ $ HH.ClassName "f4 fw2 tracked ttu mb4 lh-title" ]
          [
            HH.span [ HP.class_ $ HH.ClassName "fw4" ] [ HH.text "Dungeon" ]
          , HH.span [ HP.class_ $ HH.ClassName "fw1" ] [ HH.text " Studio" ]
          ]
        , HH.button
            [ HP.class_ buttonClass, HE.onClick (HE.input_ Login) ]
            [ HH.text "Login" ]
        ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Output Monad
  eval = case _ of
    Login next -> do
      lift $ authorize
      pure next
