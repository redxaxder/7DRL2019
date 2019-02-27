module ResourceLoading where

import Extra.Prelude

import Affjax (get)
import Affjax.ResponseFormat (json)
import Data.Argonaut.Core (Json, caseJsonString, caseJsonArray)
import Effect.Aff (Aff)

indexFile :: String
indexFile = "resources/index.json"

data JsonFormat a = JsonFormat (Json -> Maybe a)

indexFileFormat :: JsonFormat (Array String)
indexFileFormat = JsonFormat go
  where
  go j = caseJsonArray Nothing readStrings j >>= sequence
  readStrings strings = Just $ caseJsonString Nothing Just <$> strings

loadResources :: Aff (Maybe (Array String))
loadResources = loadJson indexFileFormat indexFile

loadJson :: forall a. JsonFormat a -> String -> Aff (Maybe a)
loadJson (JsonFormat f) uri = do
  { status, statusText, headers, body } <- get json uri
  case body of
       Left bad -> pure Nothing
       Right success -> pure $ f success

