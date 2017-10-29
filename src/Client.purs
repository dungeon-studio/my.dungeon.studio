module EarthdawnClient where

import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(..))

data Endpoint body return = Endpoint {
  method :: Method,
  url :: String
}

newEarthdawn4eEndpoint :: Endpoint Foreign Foreign
newEarthdawn4eEndpoint = Endpoint
  { method: POST
  , url: "/earthdawn/4e"
  }
