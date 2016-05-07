module HaskellTools (..) where

import Effects exposing (Effects, Never)
import HaskellTools.Package.Effects exposing (getPackages)
import HaskellTools.Model exposing (AppModel)
import HaskellTools.Update exposing (update)
import HaskellTools.View.App exposing (view)
import Html exposing (Html)
import StartApp
import Task

app : StartApp.App AppModel
app =
  StartApp.start
    { init = ( {packages = [], errorMessage = ""}, getPackages )
    , update = update
    , view = view
    , inputs = []
    }

main : Signal Html
main =
  app.html

-- Ports
port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
