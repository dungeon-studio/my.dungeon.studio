module DungeonStudio.Components.Entity
( Query
, component
) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Siren (_entities)
import Data.Siren.Types (Entity(..), Link(..), SubEntity(..))
import Debug.Trace (traceAnyA)
import DungeonStudio.DSL.Client.Algebra (getRoot)
import DungeonStudio.Control.Monad (AppM)
import DungeonStudio.CSS (css)
import Halogen as H
import Halogen.HTML as HH

data Query a = Init String a

type Input = Unit
type Output = Void
type Monad = AppM
type State = { root :: Maybe Entity }

initialState :: State
initialState = { root: Nothing }

component :: String -> H.Component HH.HTML Query Input Output Monad
component path =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer
    , finalizer
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render st = HH.div [ css "w-100 tc pa5" ] [ renderSubEntities st.root ]
    where
      renderSubEntities = case _ of
        Nothing -> HH.div_ []
        Just root -> HH.div_ $ renderSubEntity <$> root ^. _entities

      renderSubEntity = case _ of
        EmbeddedLink l -> HH.div_ []
        EmbeddedRepresentation (Entity ent) -> HH.div_ $ renderLink <$> ent.links

      renderLink (Link l) =
        HH.div
          [ css "white pv3 tracked" ]
          [ HH.div
              [ css "ttu"]
              [ HH.text $ fromMaybe "Untitled" l.title]
          , HH.div_ [ HH.text l.href ]
          ]

  eval :: Query ~> H.ComponentDSL State Query Output Monad
  eval = case _ of
    Init p next -> do
      root <- lift $ getRoot p
      H.modify _{ root = root } $> next

  initializer = Just $ H.action (Init path)
  finalizer = Nothing
