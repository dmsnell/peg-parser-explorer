port module GrammarExplorer exposing (main)

import Http
import Html exposing (Html, button, div, input, label, programWithFlags, text, textarea)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Regex as RE


port parse : { grammar : String, document : String, shouldTrace : Bool } -> Cmd msg


port parsed : (ParseResult -> msg) -> Sub msg


type alias Flags =
    {}


type alias Model =
    { document : String
    , grammar : String
    , grammarUrl : String
    , parsed : String
    , trace : String
    , shouldTrace : Bool
    }


type Msg
    = FetchGrammar
    | LoadGrammar (Result Http.Error String)
    | ToggleTracing
    | UpdateGrammar String
    | UpdateGrammarUrl String
    | UpdateDocument String
    | UpdateParse ParseResult


main : Platform.Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    parsed UpdateParse


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { document = ">>>><<>><<<>>"
            , grammar = initialGrammar
            , grammarUrl = "https://raw.githubusercontent.com/WordPress/gutenberg/master/blocks/api/post.pegjs"
            , parsed = ""
            , trace = ""
            , shouldTrace = False
            }
    in
        ( model
        , parse { grammar = model.grammar, document = model.document, shouldTrace = model.shouldTrace }
        )


fetchGrammar : String -> Cmd Msg
fetchGrammar url =
    Http.getString url
        |> Http.send LoadGrammar


type alias ParseResult =
    { parsed : String
    , trace : String
    }


cleanTrace : String -> String
cleanTrace trace =
    trace
        |> RE.replace RE.All (RE.regex "\\\\n") (\_ -> "\n")
        |> String.slice 1 -1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        parseIt { grammar, document } =
            parse { grammar = grammar, document = document, shouldTrace = model.shouldTrace }
    in
        case msg of
            FetchGrammar ->
                ( model, fetchGrammar model.grammarUrl )

            LoadGrammar (Ok s) ->
                update (UpdateGrammar s) model

            ToggleTracing ->
                ( { model | shouldTrace = not model.shouldTrace }, Cmd.none )

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

            UpdateParse { parsed, trace } ->
                ( { model | parsed = parsed, trace = cleanTrace trace }, Cmd.none )

            _ ->
                ( model, Cmd.none )


appStyle =
    [ ( "width", "100%" )
    , ( "height", "100vh" )
    , ( "display", "flex" )
    , ( "flex-basis", "fill" )
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
                [ label [ style labelStyle ] [ text "Grammar to fetch" ]
                , input
                    [ style inputStyle
                    , type_ "text"
                    , onInput UpdateGrammarUrl
                    , value model.grammarUrl
                    ]
                    []
                , button [ onClick FetchGrammar ] [ text "Fetch" ]
                ]
            , div []
                [ label [ style labelStyle ] [ text "Toggle tracing" ]
                , button [ onClick ToggleTracing ]
                    [ text <|
                        if model.shouldTrace then
                            "Disable"
                        else
                            "Enable"
                    ]
                , text " (can be very slow for large documents and grammars) "
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


initialGrammar : String
initialGrammar =
    """
Stack
  = ops:Op*
  { return ops.reduce( ( a, b ) => a + b, 0 ) }

Op "Operation"
  = Push
  / Pop

Push
  = ">"
  { return 1 }

Pop
  = "<"
  { return -1 }
"""
