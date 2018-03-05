module Models exposing (..)

import RemoteData exposing (WebData)
import Http


type Route
    = HomeRoute (Maybe String)
    | AboutRoute
    | NotFoundRoute


type alias Photo =
    { url : String
    , source : String
    , loaded : Bool
    , host : String
    }


type alias Model =
    { query : String
    , previousSearch : String
    , photos : WebData (List Photo)
    , route : Route
    , error : Maybe Http.Error
    , viewing : Maybe Photo
    }


initialModel : Route -> Model
initialModel route =
    { query = ""
    , previousSearch = ""
    , photos = RemoteData.Loading
    , route = route
    , error = Nothing
    , viewing = Nothing
    }
