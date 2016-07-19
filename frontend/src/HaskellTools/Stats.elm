module HaskellTools.Stats exposing (Model, Msg, view, update, init)

import Html exposing (..)
import Html.Attributes exposing (class)

import HaskellTools.Api exposing (..)

type alias Model =
  { totalPackages : Int
  , extensions : Extensions
  , topLibraries : Packages
  , topApps : Packages
  , mostUsed : Packages
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
         , topApps = []
         , mostUsed = []
         }
       , Cmd.batch [getExtensions, getTopLibraries, getTopApps, getMostUsed]
       )

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchExtensions result ->
            ( {model | extensions = result}, Cmd.none )
        -- the catch all bellow is for api calls that are not relevant for the stats module
        _ ->
              ( model, Cmd.none )

type alias Msg = ApiMsg

-- private

viewExtensions : Extensions -> Html msg
viewExtensions extensions =
    table [ class "primary full" ]
        [ thead []
              [ tr []
                    [ th []
                          [ text "Extensions used in cabal files" ]
                    , th []
                        [ text "# of packages" ]
                    ]
              ]
        , tbody [] (List.map extensionTR extensions)
        ]

extensionTR : Extension -> Html msg
extensionTR e =
    tr []
        [ td [] [ text e.extension ]
        , td [] [ toString e.packages |> text ]
        ]
