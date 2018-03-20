module Commands exposing (..)

import Http
import Json.Decode exposing (Decoder, bool, list, string, at)
import Models exposing (Sources, Model)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Models exposing (Photo)
import Msgs exposing (Msg)
import RemoteData
import Set as S


buildSources : Sources -> String
buildSources sources =
    -- [TODO] Look for more generic url building function
    if not (S.isEmpty sources) then
        "&sources=" ++ (String.join "&" (S.toList sources))
    else
        ""


searchImage : String -> Sources -> Cmd Msg
searchImage query sources =
    if String.isEmpty query then
        Cmd.none
    else
        let
            url =
                "http://localhost:8001/?q=" ++ query ++ buildSources sources
        in
            Http.get url photosDecoder
                |> RemoteData.sendRequest
                |> Cmd.map Msgs.FetchPhotos


photosDecoder : Decoder (List Photo)
photosDecoder =
    at [ "results" ] (list photoDecoder)


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "source" string
        |> optional "loaded" bool False
        |> optional "host" string ""
        |> optional "description" string ""
        |> optional "license" string ""
        |> optional "owner_name" string ""
