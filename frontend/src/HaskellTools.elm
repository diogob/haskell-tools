module HaskellTools exposing (..)

import Html.App as App
import HaskellTools.SearchBox as S
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

view : Model -> Html msg
view model =
  div
    []
    [ topBar
    , Html.main'
        [ class "app-body" ]
        [ S.view
        , PL.view model.packages
        ]
    ]

topBar : Html msg
topBar =
  header
    [ class "top-bar" ]
    [ h1
        []
        [ img [ src "assets/img/Haskell-Logo.svg", width 100 ] []
        , text "HaskellTools"
        ]
    ]

type Msg
  = PackageListMsg PL.Msg
  | NoOp
  | ShowError String

type alias Model =
  { packages : PL.Model
  , error : String
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case (Debug.log "action" action) of
    NoOp ->
      ( model, Cmd.none )
    ShowError msg ->
      ( model, Cmd.none )
    PackageListMsg subAction ->
      let
        ( updatedPackages, fx ) =
          PL.update subAction model.packages
      in
        ( { model | packages = updatedPackages }, Cmd.map PackageListMsg fx )

init : ( Model, Cmd Msg )
init =
  let
    (initialPackages, fx) = PL.init
  in
    ({packages = initialPackages, error = ""},  Cmd.map PackageListMsg fx)
