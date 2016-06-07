module HaskellTools exposing (..)

import Html.App as App
import HaskellTools.Search as S
import HaskellTools.PackageList as PL
import Html exposing (Html, header, h1, img, text, div, ul)
import Html.Attributes exposing (class, src, width)

main : Program Never
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
    , Html.main'
        [ class "app-body" ]
        [ App.map SearchMsg S.view
        , PL.view model.packages
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

type alias Model =
  { packages : PL.Model
  , error : String
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case (Debug.log "action" action) of
    ShowError msg ->
      ( model, Cmd.none )
    SearchMsg (S.SearchPackages query) ->
      PL.update (PL.SearchPackages query) model.packages
        |> updatePackages model
    PackageListMsg subAction ->
      PL.update subAction model.packages
        |> updatePackages model

init : ( Model, Cmd Msg )
init =
  let
    (initialPackages, fx) = PL.init
  in
    ({packages = initialPackages, error = ""},  Cmd.map PackageListMsg fx)

-- private functions
updatePackages : Model -> (PL.Model, Cmd PL.Msg) -> (Model, Cmd Msg)
updatePackages model (updatedPackages, fx) =
  ( { model | packages = updatedPackages }, Cmd.map PackageListMsg fx )
