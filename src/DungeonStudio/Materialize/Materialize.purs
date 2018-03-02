module DungeonStudio.Materialize
( MATERIALEFF
, Instance
, initCarousel
) where

import Control.Monad.Eff (Eff(), kind Effect)
import DOM.HTML.Types (HTMLElement)

foreign import data MATERIALEFF :: Effect
foreign import data Instance    :: Type

foreign import initCarousel
  :: forall eff
   . HTMLElement
  -> Eff ( material :: MATERIALEFF | eff ) Instance
