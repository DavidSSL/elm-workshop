module ElmHub exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, target, href, defaultValue, type', checked)
import Html.Events exposing (..)
import Html.App as Html
import Auth
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import String


getQueryString : String -> String
getQueryString query =
    -- See https://developer.github.com/v3/search/#example for how to customize!
    "access_token="
        ++ Auth.token
        ++ "&q="
        ++ query
        ++ "+language:elm&sort=stars&order=desc"


responseDecoder : Decoder (List SearchResult)
responseDecoder =
    Json.Decode.at [ "items" ] (Json.Decode.list searchResultDecoder)


searchResultDecoder : Decoder SearchResult
searchResultDecoder =
    decode SearchResult
        |> required "id" Json.Decode.int
        |> required "full_name" Json.Decode.string
        |> required "stargazers_count" Json.Decode.int


type alias Model =
    { query : String
    , results : List SearchResult
    , errorMessage : Maybe String
    , options : SearchOptions
    }


type alias SearchOptions =
    { sort : String
    , order : String
    , searchIn : List String
    , includeForks : Bool
    , userFilter : String
    }


type alias SearchResult =
    { id : Int
    , name : String
    , stars : Int
    }


initialModel : Model
initialModel =
    { query = "tutorial"
    , results = []
    , errorMessage = Nothing
    , options =
        { sort = ""
        , order = ""
        , searchIn = []
        , includeForks = True
        , userFilter = ""
        }
    }


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header []
            [ h1 [] [ text "ElmHub" ]
            , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
            ]
        , Html.map Options (viewOptions model.options)
        , input [ class "search-query", onInput SetQuery, defaultValue model.query ] []
        , button [ class "search-button", onClick Search ] [ text "Search" ]
        , viewErrorMessage model.errorMessage
        , ul [ class "results" ] (List.map viewSearchResult model.results)
        ]


viewErrorMessage : Maybe String -> Html a
viewErrorMessage errorMessage =
    case errorMessage of
        Just message ->
            div [ class "error" ] [ text message ]

        Nothing ->
            text ""


viewSearchResult : SearchResult -> Html Msg
viewSearchResult result =
    li []
        [ span [ class "star-count" ] [ text (toString result.stars) ]
        , a [ href ("https://github.com/" ++ result.name), target "_blank" ]
            [ text result.name ]
        , button [ class "hide-result", onClick (DeleteById result.id) ]
            [ text "X" ]
        ]


type Msg
    = Search
    | Options OptionsMsg
    | SetQuery String
    | DeleteById Int
    | HandleSearchResponse (List SearchResult)
    | HandleSearchError (Maybe String)
    | DoNothing


update : (String -> Cmd Msg) -> Msg -> Model -> ( Model, Cmd Msg )
update searchFeed msg model =
    case msg of
        Search ->
            ( model, searchFeed (getQueryString model.query) )

        Options optionsMsg ->
            ( { model | options = updateOptions optionsMsg model.options }, Cmd.none )

        SetQuery query ->
            ( { model | query = query }, Cmd.none )

        HandleSearchResponse results ->
            ( { model | results = results }, Cmd.none )

        HandleSearchError error ->
            ( { model | errorMessage = error }, Cmd.none )

        DeleteById idToHide ->
            let
                newResults =
                    model.results
                        |> List.filter (\{ id } -> id /= idToHide)

                newModel =
                    { model | results = newResults }
            in
                ( newModel, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


type OptionsMsg
    = SetSort String
    | SetOrder String
    | SetSearchIn (List String)
    | SetIncludeForks Bool
    | SetUserFilter String


updateOptions : OptionsMsg -> SearchOptions -> SearchOptions
updateOptions optionsMsg options =
    case optionsMsg of
        SetSort sort ->
            { options | sort = sort }

        SetOrder order ->
            { options | order = order }

        SetSearchIn searchIn ->
            { options | searchIn = searchIn }

        SetIncludeForks includeForks ->
            { options | includeForks = includeForks }

        SetUserFilter userFilter ->
            { options | userFilter = userFilter }


viewOptions : SearchOptions -> Html OptionsMsg
viewOptions model =
    div []
        [ input [ type' "text", defaultValue model.sort, onInput SetSort ] []
        , input [ type' "text", defaultValue model.order, onInput SetOrder ] []
        , input [ type' "text", defaultValue (String.join " " model.searchIn) ] []
        , input [ type' "checkbox", checked model.includeForks ] []
        , input [ type' "text", defaultValue model.userFilter, onInput SetUserFilter ] []
        ]


decodeGithubResponse : Json.Decode.Value -> Msg
decodeGithubResponse value =
    case Json.Decode.decodeValue responseDecoder value of
        Ok results ->
            HandleSearchResponse results

        Err err ->
            HandleSearchError (Just err)