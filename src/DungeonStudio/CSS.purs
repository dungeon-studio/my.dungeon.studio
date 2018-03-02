module DungeonStudio.CSS
( css
) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude ((<<<))

--| Helper for adding CSS classes
css :: forall a b. String -> HH.IProp ( "class" :: String | a ) b
css = HP.class_ <<< HH.ClassName
