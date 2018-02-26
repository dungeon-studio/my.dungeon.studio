module DungeonStudio.Components.Entity
( Query
, component
) where

import Control.Monad.Trans.Class (lift)
import Data.Array (filter, notElem)
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Siren (_entities)
import Data.Siren.Types (Entity(..), Link(..), SubEntity(..))
import Debug.Trace (traceAnyA)
import DungeonStudio.DSL.Client.Algebra (getRoot)
import DungeonStudio.Control.Monad (AppM)
import DungeonStudio.Components.EntityAction as EntityAction
import DungeonStudio.CSS (css)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, ($), (#), (<$>), ($>), bind, const)

data Query a = Init String a

type Input = Unit
type Output = Void
type Monad = AppM
type State = { root :: Maybe Entity }
type ChildQuery = EntityAction.Query
type ChildSlot = Unit

initialState :: State
initialState = { root: Nothing }

component :: String -> H.Component HH.HTML Query Input Output Monad
component path =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer
    , finalizer
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Monad
  render st = case st.root of
    Nothing -> HH.div_ []
    Just root ->
      HH.div
        [ css "container" ]
        -- TODO: Do not hardcode this link?
        [ HH.div
            [ css "row" ]
            [ HH.div
                [ css "col s12 l3 m4" ]
                [ HH.div
                  [ css "card" ]
                  [ HH.div
                    [ css "card-content" ]
                    [ HH.a
                      [ css "card-title link underline-hover", HP.href "#/characters/create" ]
                      [ HH.text "Create Character" ]
                    ]
                  ]
                ]
            , HH.div_ $ renderSubEntity <$> root ^. _entities
            ]
        ]
      where
        renderSubEntity = case _ of
          EmbeddedLink l -> HH.div_ []
          EmbeddedRepresentation (Entity ent) ->
            HH.div
              [ css "col s12 l3 m4" ]
              [ HH.div
                [ css "card" ]
                $ renderLink <$> (ent.links # filter (\(Link l) -> "self" `notElem` l.rel))
              ]

        renderLink (Link l) =
          HH.div
            [ css "card-content" ]
            [ HH.div
                [ css "card-title"]
                [ HH.text $ fromMaybe "Untitled" l.title]
            , HH.div [ css "truncate" ] [ HH.text l.href ]
            ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output Monad
  eval = case _ of
    Init p next -> do
      root <- lift $ getRoot p
      H.modify _{ root = root } $> next

  initializer = Just $ H.action (Init path)
  finalizer = Nothing
