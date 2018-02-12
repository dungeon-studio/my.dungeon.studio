module Components.Characters
( Query
, component
) where

import Prelude

import Client.Algebra (ResponseType(..), getRoot, resolveAction, resolveLink)
import CollectionJSON (CollectionJSON(..), Collection(..), Datum(..), Item(..), collectionMime)
import Control.Alt ((<|>))
import Control.Monad.App (AppM)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Parallel (class Parallel, parTraverse)
import Data.Array ((!!), catMaybes, elem, filter, find, head)
import Data.Either (either)
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Path.Pathy (currentDir, dir, parseRelFile, relativeTo, rootDir)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..), fst)
import Data.URI.HierarchicalPart (_path)
import Data.URI.Host (_NameAddress)
import Data.URI.Scheme (print)
import Data.URI.URI (_authority, _hierPart, _hosts, _scheme, parse)
import Debug.Trace (traceAnyA)
import DOM.Event.Event (preventDefault, target)
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
  | Submit Event a
  | UpdateRace String a
  | UpdateDiscipline String a

type Input = Unit
type Output = Void
type State =
  { collections :: Array CollectionMap
  , race :: Maybe String
  , discipline :: Maybe String
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
  , race: Nothing
  , discipline: Nothing
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
  render st = HH.div [] [ createForm, listSubEntities st.root ]
    where
      createForm =
        HH.form [ HP.id_ "character", HE.onSubmit (HE.input Submit) ]
          [ HH.label
              [ HP.class_ $ HH.ClassName "mh2 white", HP.for "race" ]
              [ HH.text "Race" ]
          , HH.select
              [ HP.id_ "race" , HE.onValueChange (HE.input UpdateRace) ]
              (itemsToOptions "races" st.collections)
          , HH.label
              [ HP.class_ $ HH.ClassName "mh2 white", HP.for "discipline" ]
              [ HH.text "Discipline" ]
          , HH.select
              [ HP.id_ "discipline" , HE.onValueChange (HE.input UpdateDiscipline) ]
              (itemsToOptions "disciplines" st.collections)
          , HH.button
              [ HP.type_ ButtonSubmit ]
              [ HH.text "Create" ]
          ]

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
          traceAnyA $ getItems "races" st.collections
          pure next
    UpdateRace uri next -> H.modify _{ race = Just uri } $> next
    UpdateDiscipline uri next -> H.modify _{ discipline = Just uri } $> next
    Submit e next -> do
      liftEff $ preventDefault e
      pure next
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
  :: String
  -> Array CollectionMap
  -> Array Item
getItems rel cmaps = maybe [] items cmap
  where cmap = cmaps # find (\cm -> rel `elem` (link cm # _.rel))
        link = _.link >>> unwrap
        items = _.collection >>> unwrap >>> _.items >>> fromMaybe []

getAbsoluteURI :: String -> String -> String
getAbsoluteURI uri rel = show path
  where path = either (const "" ) uriPath (parse uri)
        uriPath u = maybe "" (\h -> scheme u <> "//" <> h <> "/" <> rel) (head $ names u)
        names u = ((_ ^. _NameAddress) <<< fst) <$> hosts u
        hosts u = maybe [] (_ ^. _hosts) (authority u)
        authority u = u ^. _hierPart ^. _authority
        scheme u = maybe "http://" print $ u ^. _scheme

itemToOption
  :: forall p i
   . String
  -> Array CollectionMap
  -> Item
  -> HH.HTML p i
itemToOption rel cmaps (Item i) = case i.data of
  Nothing -> HH.option_ []
  Just ds -> do
    let link = _.link >>> unwrap
        cmap = cmaps # find (\cm -> rel `elem` (link cm # _.rel))
        value = maybe "" (\cm -> getAbsoluteURI (link cm # _.href) i.href) cmap
        label = ds # find (\(Datum d) -> d.name == "name")
          >>> maybe "Untitled" (unwrap >>> _.prompt >>> fromMaybe "Untitled")
    HH.option [ HP.value value ] [ HH.text label ]

itemsToOptions
  :: forall p i
   . String
  -> Array CollectionMap
  -> Array (HH.HTML p i)
itemsToOptions rel cmaps = itemToOption rel cmaps <$> getItems rel cmaps
