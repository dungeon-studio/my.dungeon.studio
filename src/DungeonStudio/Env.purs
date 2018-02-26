module DungeonStudio.Env
( Env(..)
, env
, getEnv
) where

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Foreign (Foreign, MultipleErrors)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Prelude (class Show, ($))
import Simple.JSON (class ReadForeign, read)

newtype Env = Env
  { auth0Audience :: String
  , apiHost :: String
  }

derive instance ntE :: Newtype Env _
derive newtype instance rfE :: ReadForeign Env
derive instance rgE :: Rep.Generic Env _
instance showEnv :: Show Env where show = genericShow

-- We must get our env values from the transformed foreign module
-- Because the foreign module contains `process.env` values
-- And those values are replaced with strings at build time
foreign import env :: Foreign

getEnv :: Either MultipleErrors Env
getEnv = runExcept $ read env
