module Main (..) where

import Effects exposing (Effects, Never)
import Html exposing (Html, ol, li, div, button, text, a)
import Html.Attributes exposing (href, target)
import Http
import Json.Decode exposing (Decoder, decodeValue, succeed, string, list, int, (:=))
import Json.Decode.Extra exposing ((|:))
import StartApp
import Task


--Config


topReposUrl : String
topReposUrl =
  "http://localhost:3000/top_repos"


decodeRepos : Decoder Model
decodeRepos =
  list
    (succeed Repo
      |: ("name" := string)
      |: ("owner" := string)
      |: ("url" := string)
      |: ("watchers" := int)
      |: ("forks" := int)
    )



-- Model


type alias Model =
  List Repo


type alias Repo =
  { name : String
  , owner : String
  , url : String
  , watchers : Int
  , forks : Int
  }


init : ( Model, Effects Action )
init =
  ( [], getTopRepos )



-- Update


type Action
  = RequestMore
  | NewRepos (Maybe (List Repo))


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    RequestMore ->
      ( model, getTopRepos )

    NewRepos maybeRepos ->
      ( Maybe.withDefault [] maybeRepos
      , Effects.none
      )



-- View


view : Signal.Address Action -> Model -> Html
view address model =
  ol
    []
    (List.map (repoView address) model)


repoView : Signal.Address Action -> Repo -> Html
repoView address model =
  li
    []
    [ a
        [ href model.url, target "blank" ]
        [ text <| model.owner ++ " / " ++ model.name ]
    ]



-- Main


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


main : Signal Html
main =
  app.html



-- Effects


getTopRepos : Effects Action
getTopRepos =
  Http.get decodeRepos topReposUrl
    |> Task.toMaybe
    |> Task.map NewRepos
    |> Effects.task



-- Ports


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
