module DungeonStudio.Components.Entity
( Query
, component
) where

import Control.Monad.Trans.Class (lift)
import CSS (fromString)
import CSS.Flexbox (JustifyContentValue(..), justifyContent)
import CSS.Geometry (height)
import CSS.Size (pct, px)
import Data.Array (filter, notElem)
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Siren (_entities)
import Data.Siren.Types (Entity(..), Link(..), SubEntity(..))
import Debug.Trace (traceAnyA)
import DungeonStudio.DSL.Client.Algebra (getRoot)
import DungeonStudio.Control.Monad (AppM)
import DungeonStudio.Components.EntityAction as EntityAction
import DungeonStudio.Components.Loader (loader)
import DungeonStudio.CSS (css)
import DungeonStudio.Materialize (initCarousel)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, ($), (#), (<$>), ($>), (>>=), bind, const, discard, pure)
import Run (liftEff)

data Query a = Init String a

type Input = Unit
type Output = Void
type Monad = AppM
type State = { root :: Maybe Entity, loading :: Boolean }
type ChildQuery = EntityAction.Query
type ChildSlot = Unit

initialState :: State
initialState = { root: Nothing, loading: true }

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
  render st = case st.loading of
    true ->
      HH.div
        [ css "row valign-wrapper"
        , style do
            height $ px 500.0
            justifyContent $ JustifyContentValue $ fromString "center"
        ]
        [ loader ]
    false -> case st.root of
      Nothing -> HH.div_ []
      Just root ->
        HH.div
          [ css "row carousel"
          , style do height $ px 500.0
          , HP.ref (H.RefLabel "carousel")
          ]
          [ HH.div
              [ css "col s12 l3 m4 carousel-item"
              , style do height $ pct 100.0
              ]
              [ HH.div
                [ css "card medium grey lighten-2" ]
                [ HH.div
                    [ css "valign-wrapper center-text card-content"
                    , style do
                        justifyContent $ JustifyContentValue $ fromString "center"
                        height $ pct 90.0
                    ]
                    [ HH.a
                      -- TODO: Do not hardcode this link?
                      [ css "link dim black-text"
                      , HP.href "#/characters/create"
                      ]
                      [ HH.i
                        [ css "large material-icons" ]
                        [ HH.text "add" ]
                      ]
                    ]
                , HH.div
                    [ css "card-action grey darken-1 white-text" ]
                    [ HH.text "New Character" ]
                ]
              ]
              , HH.div_ $ renderSubEntity <$> root ^. _entities
          ]
        where
          renderSubEntity = case _ of
            EmbeddedLink l -> HH.div_ []
            EmbeddedRepresentation (Entity ent) ->
              HH.div
                [ css "carousel-item col s12 l3 m4" ]
                [ HH.div
                  [ css "card medium grey lighten-2" ]
                  [ HH.div
                      [ css "card-content" ]
                      $ renderLink <$> (ent.links # filter (\(Link l) -> "self" `notElem` l.rel))
                  , HH.div
                      [ css "card-action grey darken-1 white-text" ]
                      [ HH.text "No name" ]
                  ]
                ]

          renderLink (Link l) =
            HH.div_
              [ HH.div
                  [ css "f5 fw5 ttu tracked"]
                  [ HH.text $ fromMaybe "Untitled" l.title]
              , HH.div [ css "truncate", HP.title l.href ] [ HH.text l.href ]
              ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output Monad
  eval = case _ of
    Init p next -> do
      root <- lift $ getRoot p
      H.modify _{ root = root, loading = false }
      H.getHTMLElementRef (H.RefLabel "carousel") >>= case _ of
        Nothing -> pure next
        Just el -> do
          lift $ liftEff $ initCarousel el $> next

  initializer = Just $ H.action (Init path)
  finalizer = Nothing
