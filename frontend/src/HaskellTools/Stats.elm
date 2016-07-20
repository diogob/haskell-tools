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
viewTopLibraries = viewPackages [ ("Top Libraries", .package_name)
                                , ("Dependents (including transient)", (.all_dependents >> toString))
                                , ("Stars", (.stars >> toString))
                                ]

viewTopApps : Packages -> Html msg
viewTopApps = viewPackages [ ("Top Apps", .package_name)
                           , ("Dependencies (including transient)", (.all_dependencies >> toString))
                           , ("Stars", (.stars >> toString))
                           ]

viewPackages : List (String, (Package -> String)) -> Packages -> Html msg
viewPackages builder packages =
    let
        (titles, trBuilder) = List.unzip builder
        renderTd e = td [] [text e]
        pkgToStrs pkg = List.map ((|>) pkg) trBuilder
        renderTr pkg =
            tr [] (List.map renderTd (pkgToStrs pkg))
    in
        table [ class "primary full" ]
            [ header titles
            , tbody [] (List.map renderTr packages)
            ]
