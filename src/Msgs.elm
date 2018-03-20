module Msgs exposing (..)

import Models exposing (Photo)
import Navigation exposing (Location)
import RemoteData exposing (WebData)
import Keyboard
import Dom exposing (..)


type Msg
    = UpdateQuery String
    | UrlChange Location
    | SearchSubmit
    | FetchPhotos (WebData (List Photo))
    | ImageLoaded String
    | FocusOn String
    | FocusResult (Result Dom.Error ())
    | ViewPhoto Photo String
    | ClearInput
    | KeyMsg Keyboard.KeyCode
    | NoOp
    | StopViewing
    | ToggleFlickr
    | ToggleBing
    | ToggleCheckbox String
