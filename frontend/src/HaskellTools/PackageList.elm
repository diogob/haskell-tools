module HaskellTools.PackageList exposing (Model, Msg(..), view, update, init)

import Html exposing (Html, ul, li, text, a, div, h4, p)
import Html.Attributes exposing (href, target)
import String

import HaskellTools.Api exposing (..)

type Msg
  = Api ApiMsg
  | SearchPackages String

type alias Model = Packages

view : Model -> Html msg
view model =
  ul
    []
    (List.map packageView model)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    SearchPackages query ->
        if String.length query > 0
        then ( model, Cmd.map Api (searchPackages query) )
        else ( [], Cmd.none )
    Api (FetchPackages result) -> ( result, Cmd.none )
    _ -> ( model, Cmd.none )

init : (Model, Cmd Msg)
init = ([], Cmd.none)

-- private functions

separator : Html msg
separator = text " - "

packageView : Package -> Html msg
packageView model =
  li
    []
    [ p
      []
      [ h4 [] [ text model.package_name ]
      , ul
        []
        [ li [] [text model.description]
        , li [] [text ("Category: " ++ model.category)]
        , li []
          [ a
            [ href ("http://hackage.haskell.org/package/" ++ model.package_name), target "blank" ]
            [ text "Hackage" ]
          , separator
          , a
            [ href model.repo_location, target "blank" ]
            [ text "Source code" ]
          ]
        , li []
          [ text ((toString (List.length model.dependencies)) ++ " Dependencies")
          , separator
          , text ((toString (List.length model.dependents)) ++ " Dependents")
          , separator
          , text ((toString model.stars) ++ " Stars")
          , separator
          , text ((toString model.forks) ++ " Forks")
          , separator
          , text ((toString model.collaborators) ++ " Collaborators")
          ]
        ]
      ]
    ]
