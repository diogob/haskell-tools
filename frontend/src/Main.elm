module Main where

import Html exposing (Html, ol, li, div, button, text, a)
import Html.Attributes exposing (href, target)
import Html.Events exposing (onClick)
import StartApp
import Effects exposing (Effects, Never)
import Http
import Json.Decode exposing (Decoder, decodeValue, succeed, string, list, int, (:=))
import Json.Decode.Extra exposing ((|:))

import Task

-- main : App Repos
app = StartApp.start { init = init, update = update, view = view, inputs = [] }

main = app.html

(=>) = (,)

init : (Repos, Effects Action)
init = ( []
       , getTopRepos
       )

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

view : Signal.Address Action -> Repos -> Html
view address model =
  let repos = List.map (repoView address) model
  in ol [] repos


type Action
  = RequestMore
  | NewRepos (Maybe (List Repo))

type alias Repos = List Repo

type alias Repo =
  { name: String
  , owner: String
  , url: String
  , watchers: Int
  , forks: Int
  }

update : Action -> Repos -> (Repos, Effects Action)
update action model =
  case action of
    RequestMore -> (model, getTopRepos)
    NewRepos maybeRepos -> ( Maybe.withDefault [] maybeRepos
                           , Effects.none
                           )

getTopRepos : Effects Action
getTopRepos =
  Http.get decodeRepos topReposUrl
    |> Task.toMaybe
    |> Task.map NewRepos
    |> Effects.task

topReposUrl : String
topReposUrl = "http://localhost:3000/top_repos"

decodeRepos : Decoder (List Repo)
decodeRepos =
  list (succeed Repo
      |: ("name" := string)
      |: ("owner" := string)
      |: ("url" := string)
      |: ("watchers" := int)
      |: ("forks" := int))

repoView : Signal.Address Action -> Repo -> Html
repoView address model =
  li []
        [ a [href model.url, target "blank"] [ text model.owner
               , text " / "
               , text model.name
               ]
        ]
