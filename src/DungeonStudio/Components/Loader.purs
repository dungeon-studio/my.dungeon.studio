module DungeonStudio.Components.Loader
( loader
) where

import DungeonStudio.CSS (css)
import Halogen.HTML as HH

loader :: forall t1 t2. HH.HTML t2 t1
loader =
  HH.div
    [ css "preloader-wrapper small active" ]
    [ HH.div
        [ css "spinner-layer spinner-blue-only" ]
        [ HH.div
            [ css "circle-clipper left" ]
            [ HH.div
              [ css "circle" ]
              []
            ]
        , HH.div
            [ css "gap-patch" ]
            [ HH.div
              [ css "circle" ]
              []
            ]
        , HH.div
            [ css "circle-clipper right" ]
            [ HH.div
              [ css "circle" ]
              []
            ]
        ]
    ]
