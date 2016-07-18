module HaskellTools.Api exposing (apiUrl
                                 , Packages
                                 , Package
                                 )

apiUrl : String -> String
apiUrl = (++) "http://localhost:3000"

type alias Packages = List Package

type alias Package =
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
  , dependencies  : List String
  , dependents  : List String
  , created_at    : String
  , updated_at    : String
}
