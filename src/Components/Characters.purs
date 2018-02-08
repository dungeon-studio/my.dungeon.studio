module Components.Characters
( Query
, component
) where

import Prelude

import Client.Algebra (NewCharacter(..), getCharacters, makeCharacter)
import Control.Monad.App (AppM)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..), maybe)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Optic.Getter ((^.))
import Siren (_entities, getLinkByRel)
import Siren.Types (Entity, Link(..), SubEntity(..))

data Query a =
  Init a
  | Submit Event a
  | UpdateRace String a
  | UpdateDiscipline String a

type Input = Unit
type Output = Void
type State =
  { race :: String
  , discipline :: String
  , root :: Maybe Entity
  }
type Monad = AppM

component :: H.Component HH.HTML Query Input Output Monad
component =
  H.lifecycleComponent
    { initialState: const $ { race: "", discipline: "", root: Nothing }
    , render
    , eval
    , initializer
    , finalizer
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render st = HH.div [] [ listSubEntities st.root ]
    where
      createForm =
        HH.form [ HE.onSubmit (HE.input Submit) ]
          [ HH.label
              [ HP.class_ $ HH.ClassName "mh2 white", HP.for "race" ]
              [ HH.text "Race" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.id_ "race"
              , HE.onValueChange (HE.input UpdateRace)
              ]
          , HH.label
              [ HP.class_ $ HH.ClassName "mh2 white", HP.for "discipline" ]
              [ HH.text "Discipline" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.id_ "discipline"
              , HE.onValueChange (HE.input UpdateDiscipline)
              ]
          , HH.button
              [ HP.type_ ButtonSubmit ]
              [ HH.text "Create" ]
          ]

      listSubEntities = case _ of
        Nothing -> HH.div_ []
        Just root -> HH.div_ $ renderEntity <$> root ^. _entities

      renderEntity = case _ of
        EmbeddedLink l -> HH.div_ []
        EmbeddedRepresentation ent ->
          HH.div_ $ (\rel -> renderLink $ getLinkByRel ent rel)
            <$> ["self", "discipline", "race"]

      renderLink = case _ of
        Nothing -> HH.div_ []
        Just (Link link) ->
          HH.div
            [ HP.class_ $ HH.ClassName "white pv3 tracked" ]
            [ HH.div
              [ HP.class_ $ HH.ClassName "ttu"]
              [ HH.text $ maybe "Untitled" id link.title]
          , HH.div
              []
              [ HH.text $ link.href ]
            ]

  eval :: Query ~> H.ComponentDSL State Query Output Monad
  eval = case _ of
    Init next -> do
       entity <- lift $ getCharacters
       H.modify _{ root = entity }
       pure next
    UpdateRace race next -> H.modify _{ race = race } $> next
    UpdateDiscipline d next -> H.modify _{ discipline = d } $> next
    Submit e next -> do
      liftEff $ preventDefault e
      st <- H.get
      _ <- lift $ makeCharacter $ NewCharacter { race: st.race , discipline: st.discipline }
      pure next

  initializer = Just $ H.action Init
  finalizer = Nothing
