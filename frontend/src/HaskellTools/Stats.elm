module HaskellTools.Stats exposing (Model, Msg, view, update, init)

import Html exposing (..)
import Html.Attributes exposing (class)

import Task

import Json.Decode exposing (Decoder, succeed, string, list, int, at, (:=))
import Json.Decode.Extra exposing (..)

import HttpBuilder exposing(..)

import HaskellTools.Api exposing (..)

type alias Model =
  { totalPackages : Int
  , extensions : Extensions
  , topLibraries : List String
  , topEndUser : List String
  }

view : Model -> Html msg
view model =
    div []
        [ viewExtensions model.extensions
        ]

init : (Model, Cmd Msg)
init = (
         { totalPackages = 0
         , extensions = []
         , topLibraries = []
         , topEndUser = []
         }
       , getExtensions
       )

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    FetchExtensions result ->
      case result of
        Ok exts ->
          ( {model | extensions = exts}, Cmd.none )
        Err error ->
          ( model, Cmd.none )

type Msg
  = FetchExtensions (Result String Extensions)

-- private

viewExtensions : Extensions -> Html msg
viewExtensions extensions =
    table [ class "primary full" ]
        [ thead []
              [ tr []
                    [ th []
                          [ text "Extension used in cabal files" ]
                    , th []
                        [ text "Packages" ]
                    ]
              ]
        , tbody [] (List.map extensionTR extensions)
        ]

type alias Extension =
    { extension : String
    , packages : Int
    }

type alias Extensions = List Extension

extensionsUrl : String
extensionsUrl = apiUrl "/extensions?order=packages.desc"

getExtensions : Cmd Msg
getExtensions =
  get extensionsUrl
    |> withHeader "Range" "0-10"
    |> (send (jsonReader decodeExtensions) stringReader)
    |> Task.perform toError toOk

decodeExtensions : Decoder Extensions
decodeExtensions =
  list (
    succeed Extension
      |: ("extension" := string)
      |: ("packages" := int)
  )

toError : a -> Msg
toError _ = FetchExtensions (Err "Err")

toOk : Response Extensions -> Msg
toOk r = FetchExtensions (Ok r.data)

extensionTR : Extension -> Html msg
extensionTR e =
    tr []
        [ td [] [ text e.extension ]
        , td [] [ toString e.packages |> text ]
        ]
