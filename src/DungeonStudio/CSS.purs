module DungeonStudio.CSS
( css
) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

--| Helper for adding CSS classes
css :: forall a b. String -> HH.IProp ( "class" :: String | a ) b
css = HP.class_ <<< HH.ClassName
