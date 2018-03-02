module DungeonStudio.Assets
( getImgSrc
) where

import Prelude ((<>))

basePath :: String
basePath = "/assets"

getImgSrc :: String -> String
getImgSrc n = basePath <> "/images/" <> n
