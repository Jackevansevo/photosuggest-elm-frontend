module Commands exposing (..)

import Http
import Json.Decode exposing (Decoder, bool, list, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Models exposing (Photo)
import Msgs exposing (Msg)
import RemoteData


searchImage : String -> Cmd Msg
searchImage query =
    if String.isEmpty query then
        Cmd.none
    else
        let
            url =
                "http://localhost:8001/?q=" ++ query

            _ =
                Debug.log "query" query
        in
            Http.get url photosDecoder
                |> RemoteData.sendRequest
                |> Cmd.map Msgs.FetchPhotos


photosDecoder : Decoder (List Photo)
photosDecoder =
    list photoDecoder


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "source" string
        |> optional "loaded" bool False
        |> optional "host" string ""
