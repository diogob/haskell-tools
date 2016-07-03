module HaskellTools.Api exposing (apiUrl)

apiUrl : String -> String
apiUrl = (++) "http://localhost:3000"
