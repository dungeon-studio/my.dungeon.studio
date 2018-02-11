module Components.Characters
( Query
, component
) where

import Prelude

import Client.Algebra (ResponseType(..), getRoot, resolveAction, resolveLink)
import CollectionJSON (CollectionJSON(..), Collection(..), Item(..), collectionMime)
import Control.Alt ((<|>))
import Control.Monad.App (AppM)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Parallel (class Parallel, parTraverse)
import Data.Array (catMaybes, elem, filter, find)
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceAnyA)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Siren (_entities)
import Siren.Types (Entity(..), Link(..), SubEntity(..))

data Query a =
  Init a
  {-- | Submit Event a --}
  {-- | UpdateRace String a --}
  {-- | UpdateDiscipline String a --}

type Input = Unit
type Output = Void
type State =
  { collections :: Array CollectionMap
  , root  :: Maybe Entity
  }
type Monad = AppM

type CollectionMap =
  { link :: Link
  , collection :: Collection
  }

initialState :: State
initialState =
  { collections: []
  , root: Nothing
  }

component :: H.Component HH.HTML Query Input Output Monad
component =
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
  render st = HH.div [] [ listSubEntities st.root ]
    where
      {-- createForm = --}
      {--   HH.form [ HE.onSubmit (HE.input Submit) ] --}
      {--     [ HH.label --}
      {--         [ HP.class_ $ HH.ClassName "mh2 white", HP.for "race" ] --}
      {--         [ HH.text "Race" ] --}
      {--     , HH.input --}
      {--         [ HP.type_ HP.InputText --}
      {--         , HP.id_ "race" --}
      {--         , HE.onValueChange (HE.input UpdateRace) --}
      {--         ] --}
      {--     , HH.label --}
      {--         [ HP.class_ $ HH.ClassName "mh2 white", HP.for "discipline" ] --}
      {--         [ HH.text "Discipline" ] --}
      {--     , HH.input --}
      {--         [ HP.type_ HP.InputText --}
      {--         , HP.id_ "discipline" --}
      {--         , HE.onValueChange (HE.input UpdateDiscipline) --}
      {--         ] --}
      {--     , HH.button --}
      {--         [ HP.type_ ButtonSubmit ] --}
      {--         [ HH.text "Create" ] --}
      {--     ] --}

      listSubEntities = case _ of
        Nothing -> HH.div_ []
        Just root -> HH.div_ $ renderSubEntity <$> root ^. _entities

      renderSubEntity = case _ of
        EmbeddedLink l -> HH.div_ []
        EmbeddedRepresentation (Entity ent) -> HH.div_ $ renderLink <$> ent.links

      renderLink (Link l) =
          HH.div
            [ HP.class_ $ HH.ClassName "white pv3 tracked" ]
            [ HH.div
              [ HP.class_ $ HH.ClassName "ttu"]
              [ HH.text $ maybe "Untitled" id l.title]
          , HH.div
              []
              [ HH.text l.href ]
            ]

  eval :: Query ~> H.ComponentDSL State Query Output Monad
  eval = case _ of
    Init next -> do
      root <- lift $ getRoot "/characters"
      case root of
        Nothing -> pure next
        ent@(Just e) -> do
          rs <- fetchCollections e
          H.modify _{ root = ent
                    , collections = collectionMaps rs
                    }
          st <- H.get
          traceAnyA $ getItems st.collections "races"
          pure next
    {-- UpdateRace race next -> H.modify _{ raceInput = race } $> next --}
    {-- UpdateDiscipline d next -> H.modify _{ disciplineInput = d } $> next --}
    {-- Submit e next -> do --}
      {-- liftEff $ preventDefault e --}
      {-- st <- H.get --}
      {-- let action = getActionByName st.root "create-character" --}
      {--     fields = action ^. _fields --}
      {-- _ <- lift $ resolveAction $ Action { race: st.raceInput , discipline: st.disciplineInput } --}

  initializer = Just $ H.action Init
  finalizer = Nothing

fetchCollections
  :: forall m h
   . Parallel m (h AppM)
  => Functor (h AppM)
  => MonadTrans h
  => Entity
  -> h AppM (Array (Tuple Link (Maybe ResponseType)))
fetchCollections (Entity e) =
  parTraverse (\l -> Tuple l <$> (lift $ resolveLink l))
    $ e.links # filter (\(Link l) -> maybe false ((==) collectionMime) l.type)

collectionMaps
  :: Array (Tuple Link (Maybe ResponseType))
  -> Array CollectionMap
collectionMaps rs =
  catMaybes $ flip map rs
    $ case _ of
      Tuple l (Just (RCollectionJSON (CollectionJSON c))) ->
        Just { link: l, collection: c.collection }
      _ -> Nothing

getItems
  :: Array CollectionMap
  -> String
  -> Maybe (Array Item)
getItems cms rel = maybe Nothing items cmap
  where cmap = cms # find (\cm -> rel `elem` (link cm # _.rel))
        link = _.link >>> unwrap
        items = _.collection >>> unwrap >>> _.items
