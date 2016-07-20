module HaskellTools.Api exposing (apiUrl
                                 , Package
                                 , Packages
                                 , Extension
                                 , Extensions
                                 , ApiMsg (..)
                                 , searchPackages
                                 , getExtensions
                                 , getPackages
                                 , getTopLibraries
                                 , getTopApps
                                 , getMostUsed
                                 )

import HttpBuilder exposing(..)
import Task
import Json.Encode as Encode
import Json.Decode exposing (Decoder, succeed, string, list, int, at, (:=))
import Json.Decode.Extra exposing (..)

type alias Extension =
    { extension : String
    , packages : Int
    }

type alias Extensions = List Extension

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
  , all_dependencies : Int
  , all_dependents   : Int
}

type alias Packages = List Package

type ApiMsg
  = FetchPackages Packages
  | FetchTopLibraries Packages
  | FetchTopApps Packages
  | FetchMostUsed Packages
  | FetchExtensions Extensions
  | Error String

apiUrl : String -> String
apiUrl = (++) "http://localhost:3000"

packagesUrl : String
packagesUrl = apiUrl "/packages"

extensionsUrl : String
extensionsUrl = apiUrl "/extensions?order=packages.desc"

searchUrl : String
searchUrl = apiUrl "/rpc/package_search"

getExtensions : Cmd ApiMsg
getExtensions =
  get extensionsUrl
    |> withHeader "Range" "0-9"
    |> (send (jsonReader decodeExtensions) stringReader)
    |> Task.perform toError (FetchExtensions << .data)

getPackages : List (String, String) -> (Packages -> ApiMsg) -> Cmd ApiMsg
getPackages query toPackages =
    url packagesUrl query
        |> get
        |> withHeader "Range" "0-9"
        |> (send (jsonReader decodePackages) stringReader)
        |> Task.perform toError (toPackages << .data)

getTopLibraries : Cmd ApiMsg
getTopLibraries = getPackages [("ratio", "gt.1"), ("order", "stars.desc")] FetchTopLibraries

getTopApps : Cmd ApiMsg
getTopApps = getPackages [("ratio", "lte.1"), ("order", "stars.desc")] FetchTopApps

getMostUsed : Cmd ApiMsg
getMostUsed = getPackages [("order", "all_dependents.desc")] FetchMostUsed

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
            |> (send (jsonReader decodePackages) stringReader)
            |> Task.perform toError toPackages

toError : a -> ApiMsg
toError _ = Error "API Error"

toPackages : Response Packages -> ApiMsg
toPackages = FetchPackages << .data

decodePackages : Decoder Packages
decodePackages =
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
      |: ("all_dependencies" := int)
      |: ("all_dependents" := int)
  )

decodeExtensions : Decoder Extensions
decodeExtensions =
  list (
    succeed Extension
      |: ("extension" := string)
      |: ("packages" := int)
  )

