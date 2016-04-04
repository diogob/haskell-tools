module HaskellTools.View.Head (..) where

import Html exposing (Html, header, h1, img, text)
import Html.Attributes exposing (class, src, width)


view : Html
view =
  header
    [ class "top-bar" ]
    [ h1
        []
        [ img [ src "assets/img/Haskell-Logo.svg", width 100 ] []
        , text "HaskellTools"
        ]
    ]
