module Update exposing (..)

import Msgs exposing (Msg)
import Models exposing (Route(..), Model)
import Routing exposing (parseLocation)
import Navigation exposing (newUrl)
import Commands exposing (searchImage)
import RemoteData exposing (RemoteData(..))
import Dom
import Dom.Scroll
import List.Extra exposing (elemIndex, getAt)
import Task
import Set as S


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.UpdateQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        Msgs.UrlChange location ->
            let
                newRoute =
                    parseLocation location
            in
                case newRoute of
                    HomeRoute query ->
                        case query of
                            Just query ->
                                ( { model
                                    | route = newRoute
                                    , query = query
                                    , photos = Loading
                                    , previousSearch = query
                                    , viewing = Nothing
                                  }
                                , searchImage query model.sources
                                )

                            Nothing ->
                                -- Remove the query when user navigates back to index
                                ( { model
                                    | route = newRoute
                                    , query = ""
                                    , photos = Loading
                                    , viewing = Nothing
                                  }
                                , Task.attempt (always Msgs.NoOp) <| Dom.focus "searchInput"
                                )

                    _ ->
                        -- Update the route by default
                        ( { model | route = newRoute }, Cmd.none )

        Msgs.KeyMsg code ->
            case model.route of
                HomeRoute query ->
                    case model.viewing of
                        Just viewing ->
                            case model.photos of
                                RemoteData.Success photos ->
                                    let
                                        currentIndex =
                                            elemIndex viewing photos
                                    in
                                        case currentIndex of
                                            Just index ->
                                                if code == 27 then
                                                    -- Escape Key
                                                    ( { model | viewing = Nothing }, Cmd.none )
                                                else
                                                    let
                                                        newIndex =
                                                            case code of
                                                                37 ->
                                                                    index - 1

                                                                39 ->
                                                                    index + 1

                                                                _ ->
                                                                    index
                                                    in
                                                        let
                                                            nextPhoto =
                                                                getAt newIndex photos
                                                        in
                                                            case nextPhoto of
                                                                Just photo ->
                                                                    ( { model | viewing = Just photo }, Cmd.none )

                                                                Nothing ->
                                                                    ( model, Cmd.none )

                                            _ ->
                                                ( model, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Msgs.FetchPhotos response ->
            ( { model | photos = response }, Cmd.none )

        Msgs.ToggleCheckbox target ->
            let
                newSources =
                    if S.member target model.sources then
                        S.remove target model.sources
                    else
                        S.insert target model.sources
            in
                ( { model | sources = newSources }, Cmd.none )

        Msgs.SearchSubmit ->
            -- Dont do anything if the query is empty
            if String.isEmpty model.query then
                ( model, Cmd.none )
            else
                ( model, Cmd.batch [ newUrl ("/?q=" ++ model.query), searchImage model.query model.sources ] )

        Msgs.ClearInput ->
            ( { model | query = "" }, Task.attempt (always Msgs.NoOp) <| Dom.focus "searchInput" )

        Msgs.ViewPhoto photo index ->
            let
                _ =
                    Debug.log "scrolling to index" index
            in
                ( { model | viewing = Just photo }
                , Task.attempt (always Msgs.NoOp) <| Dom.Scroll.toBottom index
                )

        Msgs.StopViewing ->
            ( { model | viewing = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )
