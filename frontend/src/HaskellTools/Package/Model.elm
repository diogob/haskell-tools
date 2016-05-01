module HaskellTools.Package.Model (..) where

import Date exposing (Date)

type alias Dependency =
  { package_name : String
  , version_rage : String
  }

type alias Model =
  { package_name  : String
  , version       : String
  , license       : String
  , description   : String
  , category      : String
  , homepage      : String
  , package_url   : String
  , repo_type     : String
  , repo_location : String
  , stars         : Int
  , forks         : Int
  , collaborators : Int
  , extensions    : List String
  , dependencies  : List Dependency
  , created_at    : String
  , updated_at    : String
}
