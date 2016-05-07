module HaskellTools.Package.Model (..) where
import Json.Decode exposing (Decoder, string, list, int, at)
import Json.Decode.Pipeline exposing (decode, required, optional)

type alias Model =
  { package_name  : String
  , version       : String
  , license       : String
  , description   : String
  , category      : String
  , homepage      : String
  , package_url   : String
  , repo_type     : String
  , repo_location : String
  , stars         : Int
  , forks         : Int
  , collaborators : Int
  , extensions    : List String
  , dependencies  : List String
  , dependents  : List String
  , created_at    : String
  , updated_at    : String
}

decodeModel : Decoder (List Model)
decodeModel =
  decode Model
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
    |> required "dependencies" (list string)
    |> required "dependents" (list string)
    |> required "created_at" string
    |> required "updated_at" string
    |> list
