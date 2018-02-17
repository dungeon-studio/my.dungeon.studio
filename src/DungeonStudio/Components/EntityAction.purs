module DungeonStudio.Components.EntityAction
( Query
, component
) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Parallel (class Parallel, parTraverse)
import Data.Array (catMaybes, concatMap, elem, filter, find, foldl, head)
import Data.CollectionJSON (CollectionJSON(..), Collection, Datum(..), Item(..), collectionMime)
import Data.Either (either)
import Data.Lens.Getter ((^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Siren (_fields, _href, _FieldName, getActionByName, getLinkByRel)
import Data.Siren.Types (Action, Entity(..), Field(..), Link(..))
import Data.StrMap (StrMap, empty, insert, lookup)
import Data.Tuple (Tuple(..), fst)
import Data.URI.Host (_NameAddress)
import Data.URI.Scheme (print)
import Data.URI.URI (_authority, _hierPart, _hosts, _scheme, parse)
import Debug.Trace (traceAnyA)
import DungeonStudio.DSL.Client.Algebra (ResponseType(..), resolveAction, resolveLink)
import DungeonStudio.Control.Monad (AppM)
import DungeonStudio.CSS (css)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA

data Query a =
  Init Entity String a
  | Submit Event a
  | UpdateSelect String String a

type Input = Unit
type Output = Void
type State =
  { collections :: Array CollectionMap
  , fields      :: StrMap String
  , root        :: Maybe Entity
  , action      :: Maybe Action
  }
type Monad = AppM

type CollectionMap =
  { link        :: Link
  , collection  :: Collection
  }

buttonClass :: String
buttonClass = "f6 pointer white bg-animate bg-dark-gray hover-bg-gray tc bn ph3 mt3 pv2 ttu tracked"

initialState :: State
initialState =
  { collections: []
  , fields: empty
  , root: Nothing
  , action: Nothing
  }

component :: Entity -> String -> H.Component HH.HTML Query Input Output Monad
component ent actionName =
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
  render st = HH.div [ css "w-100 tc pa5" ] renderForm
    where
      renderForm =
        [ HH.div_
          [ HH.div
              [ css "white fw7 pb3 header" ]
              [ HH.text "Create a Character"]
          , HH.form
              [ HP.id_ "character", HE.onSubmit (HE.input Submit) ]
              [ renderFields
              , HH.button
                  [ css buttonClass, HP.type_ ButtonSubmit ]
                  [ HH.text "Create" ]
              ]
          ]
        ]

      renderFields = case st.action of
        Nothing -> HH.div_ []
        Just act -> HH.div_ $ act ^. _fields <#>
          -- TODO: How do I know race and discipline are related to
          -- the "races" and "disciplines" collections on the Characters entity?
          \(Field field) -> case field.name of
            "race" -> renderItemSelect st.collections "race"
            "discipline" -> renderItemSelect st.collections "discipline"
            _ -> HH.div [ css "white" ] [ HH.text field.name ]

      renderItemSelect cmaps rel =
        HH.div_
            [ HH.label
                [ css "mh2 white ttu tracked db pv3"
                , HP.for rel
                ]
                [ HH.text rel ]
            , HH.select
                [ HP.id_ rel
                , css "pa1 bg-white w5-ns w-70 mb3"
                , HE.onValueChange $ HE.input (UpdateSelect rel)
                ]
                (itemsToOptions rel cmaps)
            , maybe (HH.div_ []) renderItem
                $ lookup rel st.fields
                  # maybe Nothing (flip lookupItem cmaps)
            ]

      renderItem (Item i) = maybe (HH.div_ []) renderData i.data

      renderData ds =
        HH.div
          [ css "white" ]
          $ map renderDatum $ ds # filter (\(Datum d) -> d.name /= "name")
        where
          renderDatum (Datum d) =
            HH.div
              []
              [ HH.span [] [ HH.text $ fromMaybe "" d.prompt ]
              , HH.span [] [ HH.text $ " " ]
              , HH.span [] [ HH.text $ fromMaybe "" d.value ]
              ]

  eval :: Query ~> H.ComponentDSL State Query Output Monad
  eval = case _ of
    Init e an next -> do
      rs <- fetchCollections e
      H.modify _{ root = Just e
                , collections = collectionMaps rs
                , action = getActionByName e an
                } $> next
    UpdateSelect rel path next -> do
       st <- H.get
       case st.root of
         Nothing -> pure next
         Just root -> H.modify _{ fields = insert rel path st.fields } $> next
    Submit e next -> do
      st <- H.get
      case (Tuple st.root st.action) of
        Tuple (Just root) (Just action) -> do
          let rels = (_ ^. _FieldName) <$> action ^. _fields
              base rel = maybe "" (_ ^. _href) $ getLinkByRel root $ rel <> "s"
              pathFor n = fromMaybe "" (lookup n st.fields)
              -- TODO: Fold over all fields and do not assume the field name is a Siren relation
              fields = foldl (\m rel -> insert rel (getAbsoluteURI (base rel) (pathFor rel)) m) empty rels
          liftEff $ preventDefault e
          lift $ resolveAction action (wrap fields) $> next
        _ -> pure next

  initializer = Just $ H.action $ Init ent actionName
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

collectionMaps :: Array (Tuple Link (Maybe ResponseType)) -> Array CollectionMap
collectionMaps rs =
  catMaybes $ rs <#>
    case _ of
      Tuple l (Just (RCollectionJSON (CollectionJSON c))) ->
        Just { link: l, collection: c.collection }
      _ -> Nothing

getItems :: CollectionMap -> Array Item
getItems = _.collection >>> unwrap >>> _.items >>> fromMaybe []

getItemsByRel :: String -> Array CollectionMap -> Array Item
getItemsByRel rel cmaps = maybe [] getItems cmap
  where cmap = cmaps # find (\cm -> rel `elem` (link cm # _.rel))
        link = _.link >>> unwrap

lookupItem :: String -> Array CollectionMap -> Maybe Item
lookupItem href cmaps = item
  where item = items # find (\(Item i) -> i.href == href)
        items = getItems `concatMap` cmaps

getAbsoluteURI :: String -> String -> String
getAbsoluteURI baseURI relPath = fullPath
  where fullPath = either (const "" ) uriPath (parse baseURI)
        uriPath u = maybe "" (\h -> scheme u <> "//" <> h <> "/" <> relPath) (head $ names u)
        names u = ((_ ^. _NameAddress) <<< fst) <$> hosts u
        hosts u = maybe [] (_ ^. _hosts) (authority u)
        authority u = u ^. _hierPart ^. _authority
        scheme u = maybe "http://" print $ u ^. _scheme

itemToOption :: forall p i . Item -> HH.HTML p i
itemToOption (Item i) = case i.data of
  Nothing -> HH.option_ []
  Just ds -> HH.option [ HP.value i.href ] [ HH.text $ label ds ]
    where label = find (\(Datum d) -> d.name == "name") >>>
      maybe "Untitled" (unwrap >>> _.prompt >>> fromMaybe "Untitled")

-- TODO How are singular and plural relation strings connected?
itemsToOptions :: forall p i . String -> Array CollectionMap -> Array (HH.HTML p i)
itemsToOptions rel cmaps = [emptyOption] <> (itemToOption <$> getItemsByRel (rel <> "s") cmaps)

emptyOption :: forall p i. HH.HTML p i
emptyOption =
  HH.option
    [ HP.value "", HP.selected true, HP.disabled true, HPA.hidden "true" ]
    [ HH.text "Select..." ]
