module HaskellTools.Repo.Model (..) where


type alias Model =
  { name : String
  , owner : String
  , url : String
  , watchers : Int
  , forks : Int
  }
