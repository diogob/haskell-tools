module HaskellTools.Stats exposing (Model, view)

import Html exposing (Html, text)

type alias Model =
  { totalPackages : Int
  }

view : Model -> Html msg
view model = text ("Total packages: " ++ (toString model.totalPackages))
