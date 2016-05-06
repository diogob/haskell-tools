module HaskellTools.Package.View (..) where

import HaskellTools.Action exposing (..)
import HaskellTools.Package.Model exposing (Model)
import Html exposing (Html, ul, li, text, a, div, h2)
import Html.Attributes exposing (href, target)
import Signal


view : Signal.Address Action -> Model -> Html
view address model =
  li
    []
    [ div
      []
      [ h2 [] [ text model.package_name ]
      , ul
        []
        [ li [] [text ("Category: " ++ model.category)]
        , li []
          [ a
            [ href ("http://hackage.haskell.org/package/" ++ model.package_name), target "blank" ]
            [ text "Hackage" ]
          ]
        , li []
          [ a
            [ href model.repo_location, target "blank" ]
            [ text "Source code" ]
          ]
        , li [] [text ("Dependencies: " ++ (toString (List.length model.dependencies))) ]
        , li [] [text ("Dependents: " ++ (toString (List.length model.dependents))) ]
        , li [] [text ("Stars: " ++ (toString model.stars))]
        , li [] [text ("Forks: " ++ (toString model.forks))]
        , li [] [text ("Collaborators: " ++ (toString model.collaborators))]
        ]
      ]
    ]
