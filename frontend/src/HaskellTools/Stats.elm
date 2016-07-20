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
    div [ class "flex" ]
        [ div [] [viewExtensions model.extensions]
        , div [] [viewTopLibraries model.topLibraries]
        , div [] [viewTopApps model.topApps]
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
        FetchTopLibraries result ->
            ( {model | topLibraries = result}, Cmd.none )
        FetchTopApps result ->
            ( {model | topApps = result}, Cmd.none )
        -- the catch all bellow is for api calls that are not relevant for the stats module
        _ ->
              ( model, Cmd.none )

type alias Msg = ApiMsg

-- private

header : List String -> Html msg
header titles =
    let
        renderTh title = th [] [text title]
    in
        thead [] [ tr [] (List.map renderTh titles) ]

viewExtensions : Extensions -> Html msg
viewExtensions extensions =
    let
        extensionTR e =
            tr []
                [ td [] [ text e.extension ]
                , td [] [ toString e.packages |> text ]
                ]
    in
        table [ class "primary full" ]
            [ header ["Extensions used in cabal files", "Packages"]
            , tbody [] (List.map extensionTR extensions)
            ]

viewTopLibraries : Packages -> Html msg
viewTopLibraries packages =
    let
        topLibrariesTR e =
            tr []
                [ td [] [ text e.package_name ]
                , td [] [ toString e.all_dependents |> text ]
                , td [] [ toString e.stars |> text ]
                ]
    in
        table [ class "primary full" ]
            [ header ["Top Libraries", "Dependents (including transient)", "Stars" ]
            , tbody [] (List.map topLibrariesTR packages)
            ]


viewTopApps : Packages -> Html msg
viewTopApps packages =
    let
        topAppsTR e =
            tr []
                [ td [] [ text e.package_name ]
                , td [] [ toString e.all_dependencies |> text ]
                , td [] [ toString e.stars |> text ]
                ]
    in
        table [ class "primary full" ]
            [ header ["Top Apps", "Dependencies (including transient)", "Stars" ]
            , tbody [] (List.map topAppsTR packages)
            ]
