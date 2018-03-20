module Models exposing (..)

import RemoteData exposing (WebData)
import Http
import Dict
import Set


type Route
    = HomeRoute (Maybe String)
    | AboutRoute
    | NotFoundRoute


type alias Sources =
    Set.Set String


type alias Photo =
    { url : String
    , source : String
    , loaded : Bool
    , host : String
    , description : String
    , license : String
    , owner : String
    }


type alias Model =
    { query : String
    , previousSearch : String
    , photos : WebData (List Photo)
    , route : Route
    , error : Maybe Http.Error
    , viewing : Maybe Photo
    , sources : Sources
    }


initialModel : Route -> Model
initialModel route =
    { query = ""
    , previousSearch = ""
    , photos = RemoteData.Loading
    , route = route
    , error = Nothing
    , viewing = Nothing
    , sources = Set.singleton "local"
    }
