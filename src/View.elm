module View exposing (..)

import Html exposing (..)
import Models exposing (Model, Photo, Sources)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit, on)
import Msgs exposing (..)
import RemoteData exposing (WebData)
import Svg.Attributes
import Set as S


-- [TODO]
-- Get rid of ugly chained css classes and  ELM CSS properties to render
-- components


view : Model -> Html Msg
view model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        Models.HomeRoute query ->
            case query of
                Just query ->
                    resultsView model

                Nothing ->
                    homeView model

        Models.AboutRoute ->
            aboutView model

        Models.NotFoundRoute ->
            notFoundView


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]


resultsView : Model -> Html Msg
resultsView model =
    case model.error of
        Just _ ->
            div []
                [ div [] [ text "Something went wrong" ] ]

        Nothing ->
            resultsPage model (imageGrid model)


sourceIcon : String -> Html Msg
sourceIcon source =
    case String.toLower source of
        "flickr" ->
            i [ class "fab fa-flickr" ] []

        "bing" ->
            i [ class "fab fa-windows" ] []

        _ ->
            i [ class "fab fa-question-circle" ] []


copyButton : String -> Html Msg
copyButton url =
    a [ class "f6 no-underline silver br2 ba b--black ph3 pv2 hover-moon-gray mr2", style [ ( "background", "#111111" ) ] ]
        [ span [] [ i [ class "fas fa-link pr2" ] [], text "Copy" ] ]


photoDescription : Photo -> String
photoDescription photo =
    if String.isEmpty (String.trim photo.description) then
        "n/a"
    else
        photo.description


imageModal : Photo -> Html Msg
imageModal photo =
    let
        visitBtn =
            if String.isEmpty photo.host then
                text ""
            else
                a [ class "f6 no-underline silver br2 ba b--black ph3 pv2 hover-moon-gray mr2", href photo.host, style [ ( "background", "#111111" ) ] ]
                    [ span [] [ i [ class "fas fa-globe pr2" ] [], text "Visit" ] ]
    in
        div [ class "modal fixed", style [ ( "z-index", "100" ), ( "width", "100%" ) ] ]
            [ div [ class "pa2 br2 relative w-90", style [ ( "margin", "5em auto" ), ( "background", "#222222" ) ] ]
                [ span [ class "pointer close f2 moon-gray absolute right-1", onClick StopViewing ] [ text "×" ]
                , div [ class "flex" ]
                    [ img [ class "pa4 mw7", src photo.url, style [ ( "height", "auto" ) ] ] []
                    , div [ class "w-100 white flex flex-column items-start" ]
                        [ h1 [ class "white" ]
                            [ text "Details" ]
                        , div
                            [ style [ ( "flex-grow", "1" ) ] ]
                            [ dl [ class "f6 lh-title mv2" ]
                                [ dt [ class "dib b" ]
                                    [ text "Source:" ]
                                , dd [ class "dib ml1" ]
                                    [ text photo.source ]
                                ]
                            , dl [ class "f6 lh-title mv2" ]
                                [ dt [ class "dib b" ]
                                    [ text "Description:" ]
                                , dd [ class "dib ml1" ]
                                    [ text (photoDescription photo) ]
                                ]
                            , dl [ class "f6 lh-title mv2" ]
                                [ dt [ class "dib b" ]
                                    [ text "Owner:" ]
                                , dd [ class "dib ml1" ]
                                    [ text photo.owner ]
                                ]
                            , dl [ class "f6 lh-title mv2" ]
                                [ dt [ class "dib b" ]
                                    [ text "License:" ]
                                , dd [ class "dib ml1" ]
                                    [ text photo.license ]
                                ]
                            ]
                        , div [ class "pb4" ]
                            [ a [ class "f6 no-underline silver br2 ba b--black ph3 pv2 hover-moon-gray mr2", href photo.url, style [ ( "background", "#111111" ) ] ]
                                [ span [] [ i [ class "fas fa-search pr2" ] [], text "View" ] ]
                            , visitBtn
                            , copyButton photo.url
                            ]
                        ]
                    ]
                ]
            ]


resultsPage : Model -> Html Msg -> Html Msg
resultsPage model html =
    let
        viewImage =
            case model.viewing of
                Just photo ->
                    (imageModal photo)

                Nothing ->
                    text ""
    in
        div [ class "h-100" ]
            [ div [ class "w-100 pa3 flex justify-between items-center bb", id "resultsHeader" ]
                [ div [ class "w-100 flex items-center" ]
                    [ a [ href "/", class "mr3" ]
                        [ img [ src "shutter.svg", Svg.Attributes.width "50", Svg.Attributes.height "50", class "dim" ]
                            []
                        ]
                    , (searchBar model)
                    , fieldset [ class "bn" ]
                        [ checkbox "flickr" model.sources
                        , checkbox "bing" model.sources
                        , checkbox "local" model.sources
                        ]
                    ]
                ]
            , viewImage
            , html
            ]


checkbox : String -> Sources -> Html Msg
checkbox msg sources =
    label []
        [ input
            [ type_ "checkbox"
            , onClick (Msgs.ToggleCheckbox msg)
            , checked (S.member msg sources)
            ]
            []
        , text msg
        ]


imageGrid : Model -> Html Msg
imageGrid model =
    let
        photoThumbnail ( index, photo ) =
            let
                photoID =
                    "photo" ++ (toString index)
            in
                a [ class "imageContainer", id photoID, class "ma2", onClick (ViewPhoto photo photoID) ]
                    [ img
                        [ src photo.url
                        , class "imageThumb w-100 h-100 dim"
                        ]
                        []
                    ]
    in
        case model.photos of
            RemoteData.Success photos ->
                if List.isEmpty photos then
                    div [ class "flex flex-column items-center justify-center h-100 bg-washed-yellow" ]
                        [ img
                            [ class "pb4"
                            , src "search.svg"
                            , Svg.Attributes.width "150"
                            , Svg.Attributes.height "150"
                            ]
                            []
                        , h1 [ class "fw6 f3 f2-ns lh-title mt0 mb3 amatic" ]
                            [ text ("No matches found for: " ++ "'" ++ model.previousSearch ++ "'") ]
                        ]
                else
                    div
                        [ id "photoGrid"
                        , class "flex flex-wrap justify-center items-start"
                        ]
                        (List.map photoThumbnail (List.indexedMap (,) photos))

            RemoteData.Loading ->
                loadingView

            RemoteData.NotAsked ->
                text ""

            RemoteData.Failure error ->
                (errorView error)


loadingView : Html msg
loadingView =
    div [ class "flex flex-column items-center justify-center h-100" ]
        [ div [ class "pb4 mb2 dark-gray" ] [ span [ class "fas fa-spinner fa-spin fa-10x" ] [] ]
        , h1 [ class "fw6 f3 f2-ns lh-title mt0 mb3 amatic" ] [ text "Loading" ]
        ]


errorView : error -> Html msg
errorView error =
    div [ class "flex flex-column items-center justify-center h-100 bg-washed-yellow" ]
        [ img [ class "pb4", src "bug.svg", Svg.Attributes.width "150", Svg.Attributes.height "150" ] []
        , h1 [ class "fw6 f3 f2-ns lh-title mt0 mb3 amatic" ]
            [ text "Something went wrong" ]
        ]


aboutView : Model -> Html msg
aboutView model =
    div [ class "mw6 center bg-white br3 pa3 pa4-ns mv3 ba b--black-10" ]
        [ div [ class "tc" ]
            [ h1 [ class "f4" ]
                [ text "About" ]
            , hr [ class "mw3 bb bw1 b--black-10" ]
                []
            ]
        , p [ class "lh-copy measure center f6 black-70" ]
            [ text "Photosuggest is a project to help find permissively licensed images" ]
        , div []
            [ text "Icons made by "
            , a [ href "http://www.freepik.com", title "Freepik" ]
                [ text "Freepik" ]
            , text "from "
            , a [ href "https://www.flaticon.com/", title "Flaticon" ]
                [ text "www.flaticon.com" ]
            , text "is licensed by "
            , a [ href "http://creativecommons.org/licenses/by/3.0/", target "_blank", title "Creative Commons BY 3.0" ]
                [ text "CC 3.0 BY" ]
            ]
        ]


homeView : Model -> Html Msg
homeView model =
    let
        svgImage =
            if String.isEmpty model.query then
                "shutter.svg"
            else
                "coloured-shutter.svg"
    in
        div [ class "flex flex-column justify-center items-center vh-100-ns" ]
            [ img [ src svgImage, Svg.Attributes.width "150", Svg.Attributes.height "150" ] []
            , h1 [ class "mv3", id "logoHeader", class "f1" ] [ text "Photosuggest" ]
            , (searchBar model)
            ]


searchBar : Model -> Html Msg
searchBar model =
    Html.form [ class "mw7 w-100", onSubmit SearchSubmit ]
        [ fieldset [ class "bn ma0 pa0" ]
            [ div [ id "searchBox", class "flex" ]
                [ label [ class "clip", for "q" ] [ text "Search" ]
                , (searchInput model)
                , (cancelButton model)
                , searchButton model.query
                ]
            ]
        ]


searchInput : Model -> Html Msg
searchInput model =
    input
        [ class "f6 f5-l input-reset br2 ba bw1 br-0 b--light-gray fl black-80 bg-white pa3 lh-solid w-80-ns w-70 br--left-ns"
        , name "q"
        , id "searchInput"
        , type_ "text"
        , value model.query
        , onInput UpdateQuery
        , tabindex 1
        ]
        []


cancelButton : Model -> Html Msg
cancelButton model =
    let
        btnStyle =
            if String.isEmpty model.query then
                style [ ( "visibility", "hidden" ) ]
            else
                style [ ( "visibility", "visible" ) ]
    in
        button
            [ class "pv3 ba bw1 bl-0 br-0 b--light-gray bg-animate bg-white pointer w-10 tc gray hover-black"
            , tabindex 3
            , onClick ClearInput
            , type_ "reset"
            ]
            [ i [ id "clearButton", class "fas fa-times", btnStyle ] [] ]


searchButton : String -> Html msg
searchButton query =
    let
        buttonColours query =
            if String.isEmpty query then
                "bg-white hover-bg-green gray hover-white bg-animate "
            else
                "bg-green white hover-bg-dark-green bg-animate"
    in
        button
            [ class ("pv3 br2 br--right ba bw1 bl-0 b--light-gray pointer w-10-l w-10-m w-20 " ++ buttonColours query)
            , required True
            , tabindex 2
            ]
            [ i [ class "fas fa-search" ] [] ]
