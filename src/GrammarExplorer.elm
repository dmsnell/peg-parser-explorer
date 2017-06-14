module GrammarExplorer exposing (main)

import Http
import Html exposing (Html, button, div, input, label, programWithFlags, text, textarea)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import Regex as RE


type alias Flags =
    {}


type alias Model =
    { document : String
    , grammar : String
    , grammarUrl : String
    , parsed : String
    , pegjsaas : String
    , trace : String
    }


type Msg
    = FetchGrammar
    | LoadGrammar (Result Http.Error String)
    | UpdateGrammar String
    | UpdateGrammarUrl String
    | UpdateDocument String
    | UpdateParse (Result Http.Error ParseResult)
    | UpdatePegJSaaS String


main : Platform.Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { document = ">>>><<>><<<>>"
            , grammar = "Stack\n  = ops:Op* { return ops.reduce( ( a, b ) => a + b, 0 ) }\n\nOp\n  = \">\" { return 1 }\n  / \"<\" { return -1 }\n"
            , grammarUrl = "https://raw.githubusercontent.com/WordPress/gutenberg/master/blocks/api/post.pegjs"
            , parsed = ""
            , pegjsaas = "https://pegjaas-udqzruqlye.now.sh"
            , trace = ""
            }
    in
        ( model
        , parse model.pegjsaas model
        )


fetchGrammar : String -> Cmd Msg
fetchGrammar url =
    Http.getString url
        |> Http.send LoadGrammar


type ParseResult
    = ParseResult String String


parseDecoder : JD.Decoder ParseResult
parseDecoder =
    JD.map2 ParseResult
        (JD.field "parsed" JD.string)
        (JD.field "trace" JD.string
            |> JD.map (RE.replace RE.All (RE.regex "\\\\n") (\_ -> "\n"))
            |> JD.map (RE.replace RE.All (RE.regex "^.") (\_ -> ""))
            |> JD.map (RE.replace RE.All (RE.regex ".$") (\_ -> ""))
        )


encodeBody : { r | document : String, grammar : String } -> JE.Value
encodeBody { document, grammar } =
    JE.object
        [ ( "document", JE.string document )
        , ( "grammar", JE.string grammar )
        ]


parse : String -> { r | document : String, grammar : String } -> Cmd Msg
parse url body =
    Http.post url (Http.jsonBody <| encodeBody body) parseDecoder
        |> Http.send UpdateParse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        parseIt =
            parse model.pegjsaas
    in
        case msg of
            FetchGrammar ->
                ( model, fetchGrammar model.grammarUrl )

            LoadGrammar (Ok s) ->
                update (UpdateGrammar s) model

            UpdateDocument s ->
                let
                    next =
                        { model | document = s }
                in
                    ( next, parseIt next )

            UpdateGrammar s ->
                let
                    next =
                        { model | grammar = s }
                in
                    ( next, parseIt next )

            UpdateGrammarUrl s ->
                ( { model | grammarUrl = s }, Cmd.none )

            UpdateParse (Ok (ParseResult parsed trace)) ->
                ( { model | parsed = parsed, trace = trace }, Cmd.none )

            UpdatePegJSaaS s ->
                ( { model | pegjsaas = s }, Cmd.none )

            _ ->
                ( model, Cmd.none )


appStyle =
    [ ( "width", "100%" )
    , ( "height", "100%" )
    , ( "display", "flex" )
    , ( "flex-direction", "column" )
    ]


loaderStyle =
    [ ( "font-size", "16px" )
    , ( "width", "100%" )
    ]


toolStyle =
    [ ( "width", "100%" )
    , ( "height", "100%" )
    , ( "display", "flex" )
    , ( "flex-basis", "fill" )
    , ( "flex-direction", "row" )
    ]


codeStyle =
    [ ( "font-family", "fira-code, consolas, fixed-width" )
    , ( "font-size", "14px" )
    , ( "white-space", "pre" )
    , ( "display", "flex" )
    , ( "flex-grow", "1" )
    ]


grammarStyle =
    codeStyle


documentStyle =
    codeStyle


outputStyle =
    [ ( "display", "flex" )
    , ( "flex-grow", "1" )
    , ( "flex-direction", "column" )
    ]


parsedStyle =
    codeStyle


tracerStyle =
    codeStyle


labelStyle =
    [ ( "display", "inline-block" )
    , ( "width", "9em" )
    , ( "text-align", "right" )
    , ( "margin-right", "0.5em" )
    ]


inputStyle =
    [ ( "width", "80%" )
    , ( "margin-right", "0.5em" )
    ]


view : Model -> Html Msg
view model =
    div [ style appStyle ]
        [ div [ style loaderStyle ]
            [ div []
                [ label [ style labelStyle ] [ text "Parser service URL" ]
                , input
                    [ style inputStyle
                    , type_ "text"
                    , onInput UpdatePegJSaaS
                    , value model.pegjsaas
                    ]
                    []
                ]
            , div []
                [ label [ style labelStyle ] [ text "Grammar to fetch" ]
                , input
                    [ style inputStyle
                    , type_ "text"
                    , onInput UpdateGrammarUrl
                    , value model.grammarUrl
                    ]
                    []
                , button [ onClick <| FetchGrammar ] [ text "Fetch" ]
                ]
            ]
        , div
            [ style toolStyle ]
            [ textarea [ style grammarStyle, onInput UpdateGrammar, value model.grammar ] []
            , textarea [ style documentStyle, onInput UpdateDocument, value model.document ] []
            , div [ style outputStyle ]
                [ textarea [ style parsedStyle, value model.parsed ] []
                , textarea [ style tracerStyle, value model.trace ] []
                ]
            ]
        ]
