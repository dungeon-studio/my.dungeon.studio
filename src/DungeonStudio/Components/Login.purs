module DungeonStudio.Components.Login
( Query
, component
) where

import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import DungeonStudio.Assets (getImgSrc)
import DungeonStudio.DSL.Auth0.Algebra (authorize)
import DungeonStudio.Control.Monad (AppM)
import DungeonStudio.CSS (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, ($), const, discard, pure, unit)

data Query a = Login a
type Input = Unit
type Output = Void
type State = Unit
type Monad = AppM

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
  render _ = HH.div [ css "dt vh-100 w-100" ]
    [ HH.div
        [ css "dtc v-mid tc white-text ph3 ph4-l" ]
        [
          HH.img [ css "pa3 mw3 hover dim", HP.src $ getImgSrc "logo-splash.svg" ]
        , HH.div [ css "f4 fw2 tracked ttu mb4 lh-title" ]
            [
              HH.span [ css "fw6" ] [ HH.text "Dungeon" ]
            , HH.span [ css "fw1" ] [ HH.text " Studio" ]
            ]
        , HH.button
            [ css "btn waves-effect waves-light fw6", HE.onClick (HE.input_ Login) ]
            [ HH.text "Login" ]
        ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Output Monad
  eval = case _ of
    Login next -> do
      lift $ authorize
      pure next
