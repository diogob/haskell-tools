module HaskellTools.Api exposing (apiUrl
                                 , Packages
                                 , Package
                                 , ApiMsg (..)
                                 , searchPackages
                                 )

import HttpBuilder exposing(..)
import Task
import Json.Encode as Encode
import Json.Decode exposing (Decoder, succeed, string, list, int, at, (:=))
import Json.Decode.Extra exposing (..)

type alias Packages = List Package

type ApiMsg
  = FetchPackages (Result String Packages)

type alias Package =
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

apiUrl : String -> String
apiUrl = (++) "http://localhost:3000"

packagesUrl : String
packagesUrl = apiUrl "/packages"

searchUrl : String
searchUrl = apiUrl "/rpc/package_search"

getPackages : Cmd ApiMsg
getPackages =
  get packagesUrl
    |> (send (jsonReader decodeModel) stringReader)
    |> Task.perform toError toOk

searchPackages : String -> Cmd ApiMsg
searchPackages query =
    let
        body = Encode.object
               [ ("query", Encode.string query) ]
    in
        post searchUrl
            |> withHeaders [("Content-Type", "application/json"), ("Accept", "application/json")]
            |> withHeader "Range" "0-99"
            |> withJsonBody body
            |> (send (jsonReader decodeModel) stringReader)
            |> Task.perform toError toOk

toError : a -> ApiMsg
toError _ = FetchPackages (Err "Err")

toOk : Response Packages -> ApiMsg
toOk r = FetchPackages (Ok r.data)

decodeModel : Decoder Packages
decodeModel =
  list (
    succeed Package
      |: ("package_name" := string)
      |: ("version" := string)
      |: ("license" := string)
      |: ("description" := string)
      |: ("category" := string)
      |: ("homepage" := string)
      |: ("package_url" := string)
      |: ("repo_type" := string)
      |: ("repo_location" := string)
      |: ("stars" := int)
      |: ("forks" := int)
      |: ("collaborators" := int)
      |: ("extensions" := (list string))
      |: ("dependencies" := (list string))
      |: ("dependents" := (list string))
      |: ("created_at" := string)
      |: ("updated_at" := string)
  )
