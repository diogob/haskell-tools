module HaskellTools.Model (..) where

import HaskellTools.Repo.Model as Repo
import Json.Decode exposing (Decoder, string, list, int)
import Json.Decode.Pipeline exposing (decode, required, optional)


--Config


topReposUrl : String
topReposUrl =
  "http://localhost:3000/top_repos"



-- Model


type alias Model =
  List Repo.Model


decodeRepos : Decoder Model
decodeRepos =
  decode Repo.Model
    |> required "name" string
    |> required "owner" string
    |> required "url" string
    |> optional "watchers" int 0
    |> optional "forks" int 0
    |> list
