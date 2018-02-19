module HaskellTools exposing (..)

import Html as App
import HaskellTools.Search as S
import HaskellTools.Stats as ST
import HaskellTools.PackageList as PL
import Html exposing (Html, header, h1, img, text, div, ul)
import Html.Attributes exposing (class, src, width)

main : Program Never Model Msg
main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

view : Model -> Html Msg
view model =
  div
    []
    [ topBar
    , Html.main_
        [ class "app-body flex demo" ]
        [ App.map SearchMsg S.view
        , if List.length model.packages > 0
            then PL.view model.packages
            else ST.view model.stats
        ]
    ]

topBar : Html msg
topBar =
  header
    [ class "top-bar" ]
    [ h1
        []
        [ img [ src "assets/img/Haskell-Logo.svg", width 50 ] []
        , text " "
        , text "Haskell Tools"
        ]
    ]

type Msg
  = PackageListMsg PL.Msg
  | SearchMsg S.Msg
  | ShowError String
  | StatsMsg ST.Msg

type alias Model =
  { packages : PL.Model
  , error : String
  , stats : ST.Model
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    ShowError msg ->
      ( model, Cmd.none )
    SearchMsg (S.SearchPackages query) ->
      PL.update (PL.SearchPackages query) model.packages
        |> updatePackages model
    PackageListMsg subAction ->
      PL.update subAction model.packages
        |> updatePackages model
    StatsMsg subAction ->
      ST.update subAction model.stats
        |> updateStats model

init : ( Model, Cmd Msg )
init =
  let
    (stats, fx) = ST.init
  in
    ({packages = [], error = "", stats = stats},  Cmd.map StatsMsg fx)

-- private functions
updatePackages : Model -> (PL.Model, Cmd PL.Msg) -> (Model, Cmd Msg)
updatePackages model (updatedPackages, fx) =
  ( { model | packages = updatedPackages }, Cmd.map PackageListMsg fx )

updateStats : Model -> (ST.Model, Cmd ST.Msg) -> (Model, Cmd Msg)
updateStats model (updatedStats, fx) =
  ( { model | stats = updatedStats }, Cmd.map StatsMsg fx )
