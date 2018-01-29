module Characters
( Query
, component
) where

import Prelude

import Control.Monad.App (AppM)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH

import Client.Algebra as Client

data Query a = Init a
type Input = Unit
type Output = Void
type State = Unit
type Monad = AppM

component :: H.Component HH.HTML Query Input Output Monad
component =
  H.lifecycleComponent
    { initialState: const unit
    , render
    , eval
    , initializer
    , finalizer
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render _ = HH.div [] [ HH.text "Coming Soon" ]

  eval :: Query ~> H.ComponentDSL State Query Output Monad
  eval = case _ of
    Init next -> lift $ Client.getChars $> next

  initializer = Just $ H.action Init
  finalizer = Nothing
