module HaskellTools (..) where

import Effects exposing (Effects, Never)
import HaskellTools.Effects exposing (getTopRepos)
import HaskellTools.Model exposing (Model)
import HaskellTools.Update exposing (update)
import HaskellTools.View.App exposing (view)
import Html exposing (Html)
import StartApp
import Task

app : StartApp.App Model
app =
  StartApp.start
    { init = ( [], getTopRepos )
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
