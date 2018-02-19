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

import Http exposing (Error)
import HttpBuilder exposing(..)
import Json.Encode as Encode
import Json.Decode exposing (Decoder, succeed, string, list, int, at, field)
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
apiUrl = (++) "http://api.haskelltools.com"

packagesUrl : String
packagesUrl = apiUrl "/packages"

extensionsUrl : String
extensionsUrl = apiUrl "/extensions?order=packages.desc"

searchUrl : String
searchUrl = apiUrl "/rpc/package_search"

renderExtensions : Result Error Extensions -> ApiMsg
renderExtensions r =
  case r of
    Err httpError -> toError httpError
    Ok extensions -> FetchExtensions extensions

getExtensions : Cmd ApiMsg
getExtensions =
  get extensionsUrl
    |> withHeader "Range" "0-9"
    |> withExpect (Http.expectJson decodeExtensions)
    |> send renderExtensions

renderPackages : (Packages -> ApiMsg) -> Result Error Packages -> ApiMsg
renderPackages toPackages r =
  case r of
    Err httpError -> toError httpError
    Ok packages -> toPackages packages

getPackages : List (String, String) -> (Packages -> ApiMsg) -> Cmd ApiMsg
getPackages query toPackages =
    get packagesUrl
        |> withQueryParams query
        |> withHeader "Range" "0-9"
        |> withExpect (Http.expectJson decodePackages)
        |> send (renderPackages toPackages)

getTopLibraries : Cmd ApiMsg
getTopLibraries = getPackages [("ratio", "gt.1"), ("order", "stars.desc")] FetchTopLibraries

getTopApps : Cmd ApiMsg
getTopApps = getPackages [("ratio", "lte.1"), ("order", "stars.desc")] FetchTopApps

getMostUsed : Cmd ApiMsg
getMostUsed = getPackages [("order", "all_dependents.desc")] FetchMostUsed

renderSearch : Result Error Packages -> ApiMsg
renderSearch r =
  case r of
    Err httpError -> toError httpError
    Ok packages -> FetchPackages packages

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
            |> withExpect (Http.expectJson decodePackages)
            |> send renderSearch

toError : a -> ApiMsg
toError _ = Error "API Error"

decodePackages : Decoder Packages
decodePackages =
  list (
    succeed Package
      |: (field "package_name" string)
      |: (field "version" string)
      |: (field "license" string)
      |: (field "description" string)
      |: (field "category" string)
      |: (field "homepage" string)
      |: (field "package_url" string)
      |: (field "repo_type" string)
      |: (field "repo_location" string)
      |: (field "stars" int)
      |: (field "forks" int)
      |: (field "collaborators" int)
      |: (field "extensions" (list string))
      |: (field "dependencies" (list string))
      |: (field "dependents" (list string))
      |: (field "created_at" string)
      |: (field "updated_at" string)
      |: (field "all_dependencies" int)
      |: (field "all_dependents" int)
  )

decodeExtensions : Decoder Extensions
decodeExtensions =
  list (
    succeed Extension
      |: (field "extension" string)
      |: (field "packages" int)
  )

