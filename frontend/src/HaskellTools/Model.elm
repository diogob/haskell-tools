module HaskellTools.Model (..) where

import HaskellTools.Repo.Model as Repo
import Json.Decode exposing (Decoder, string, list, int)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Model =
  List Repo.Model


decodeModel : Decoder Model
decodeModel =
  decode Repo.Model
    |> required "name" string
    |> required "owner" string
    |> required "url" string
    |> optional "watchers" int 0
    |> optional "forks" int 0
    |> list
