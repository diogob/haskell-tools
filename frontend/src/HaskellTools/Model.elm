module HaskellTools.Model (..) where

import HaskellTools.Package.Model as Package
import Json.Decode exposing (Decoder, string, list, int, at)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Model =
  List Package.Model

decodeDependency : Decoder Package.Dependency
decodeDependency =
  decode Package.Dependency
    |> required "package_name" string
    |> required "version_range" string

decodeModel : Decoder Model
decodeModel =
  decode Package.Model
    |> required "package_name" string
    |> required "version" string
    |> required "license" string
    |> required "description" string
    |> required "category" string
    |> required "homepage" string
    |> required "package_url" string
    |> required "repo_type" string
    |> required "repo_location" string
    |> required "stars" int
    |> required "forks" int
    |> required "collaborators" int
    |> required "extensions" (list string)
    |> required "dependencies" (list decodeDependency)
    |> required "created_at" string
    |> required "updated_at" string
    |> list
