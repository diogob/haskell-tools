module HaskellTools.Model (..) where

import Json.Decode exposing (Decoder, string, list, int)
import Json.Decode.Pipeline exposing (decode, required, optional)


--Config


topReposUrl : String
topReposUrl =
  "http://localhost:3000/top_repos"



-- Model


type alias Model =
  List Repo


type alias Repo =
  { name : String
  , owner : String
  , url : String
  , watchers : Int
  , forks : Int
  }


decodeRepos : Decoder Model
decodeRepos =
  decode Repo
    |> required "name" string
    |> required "owner" string
    |> required "url" string
    |> optional "watchers" int 0
    |> optional "forks" int 0
    |> list
