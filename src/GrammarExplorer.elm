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
            , grammarUrl = "https://raw.githubusercontent.com/WordPress/gutenberg/master/packages/block-serialization-spec-parser/grammar.pegjs"
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
                ( { model | document = initialDocument }, fetchGrammar model.grammarUrl )

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

initialDocument = """<!-- wp:cover {"url":"https://cldup.com/Fz-ASbo2s3.jpg","align":"wide"} -->
<div class="wp-block-cover has-background-dim alignwide" style="background-image:url(https://cldup.com/Fz-ASbo2s3.jpg)"><p class="wp-block-cover-text">Of Mountains &amp; Printing Presses</p></div>
<!-- /wp:cover -->

<!-- wp:paragraph -->
<p>The goal of this new editor is to make adding rich content to WordPress simple and enjoyable. This whole post is composed of <em>pieces of content</em>—somewhat similar to LEGO bricks—that you can move around and interact with. Move your cursor around and you’ll notice the different blocks light up with outlines and arrows. Press the arrows to reposition blocks quickly, without fearing about losing things in the process of copying and pasting.</p>
<!-- /wp:paragraph -->

<!-- wp:paragraph -->
<p>What you are reading now is a <strong>text block</strong> the most basic block of all. The text block has its own controls to be moved freely around the post...</p>
<!-- /wp:paragraph -->

<!-- wp:paragraph {"align":"right"} -->
<p style="text-align:right">... like this one, which is right aligned.</p>
<!-- /wp:paragraph -->

<!-- wp:paragraph -->
<p>Headings are separate blocks as well, which helps with the outline and organization of your content.</p>
<!-- /wp:paragraph -->

<!-- wp:heading -->
<h2>A Picture is Worth a Thousand Words</h2>
<!-- /wp:heading -->

<!-- wp:paragraph -->
<p>Handling images and media with the utmost care is a primary focus of the new editor. Hopefully, you’ll find aspects of adding captions or going full-width with your pictures much easier and robust than before.</p>
<!-- /wp:paragraph -->

<!-- wp:image {"align":"center"} -->
<div class="wp-block-image"><figure class="aligncenter"><img src="https://cldup.com/cXyG__fTLN.jpg" alt="Beautiful landscape"/><figcaption>If your theme supports it, you’ll see the "wide" button on the image toolbar. Give it a try.</figcaption></figure></div>
<!-- /wp:image -->

<!-- wp:paragraph -->
<p>Try selecting and removing or editing the caption, now you don’t have to be careful about selecting the image or other text by mistake and ruining the presentation.</p>
<!-- /wp:paragraph -->

<!-- wp:heading -->
<h2>The <em>Inserter</em> Tool</h2>
<!-- /wp:heading -->

<!-- wp:paragraph -->
<p>Imagine everything that WordPress can do is available to you quickly and in the same place on the interface. No need to figure out HTML tags, classes, or remember complicated shortcode syntax. That’s the spirit behind the inserter—the <code>(+)</code> button you’ll see around the editor—which allows you to browse all available content blocks and add them into your post. Plugins and themes are able to register their own, opening up all sort of possibilities for rich editing and publishing.</p>
<!-- /wp:paragraph -->

<!-- wp:paragraph -->
<p>Go give it a try, you may discover things WordPress can already add into your posts that you didn’t know about. Here’s a short list of what you can currently find there:</p>
<!-- /wp:paragraph -->

<!-- wp:list -->
<ul><li>Text &amp; Headings</li><li>Images &amp; Videos</li><li>Galleries</li><li>Embeds, like YouTube, Tweets, or other WordPress posts.</li><li>Layout blocks, like Buttons, Hero Images, Separators, etc.</li><li>And <em>Lists</em> like this one of course :)</li></ul>
<!-- /wp:list -->

<!-- wp:separator -->
<hr class="wp-block-separator"/>
<!-- /wp:separator -->

<!-- wp:heading -->
<h2>Visual Editing</h2>
<!-- /wp:heading -->

<!-- wp:paragraph -->
<p>A huge benefit of blocks is that you can edit them in place and manipulate your content directly. Instead of having fields for editing things like the source of a quote, or the text of a button, you can directly change the content. Try editing the following quote:</p>
<!-- /wp:paragraph -->

<!-- wp:quote -->
<blockquote class="wp-block-quote"><p>The editor will endeavor to create a new page and post building experience that makes writing rich posts effortless, and has “blocks” to make it easy what today might take shortcodes, custom HTML, or “mystery meat” embed discovery.</p><cite>Matt Mullenweg, 2017</cite></blockquote>
<!-- /wp:quote -->

<!-- wp:paragraph -->
<p>The information corresponding to the source of the quote is a separate text field, similar to captions under images, so the structure of the quote is protected even if you select, modify, or remove the source. It’s always easy to add it back.</p>
<!-- /wp:paragraph -->

<!-- wp:paragraph -->
<p>Blocks can be anything you need. For instance, you may want to add a subdued quote as part of the composition of your text, or you may prefer to display a giant stylized one. All of these options are available in the inserter.</p>
<!-- /wp:paragraph -->

<!-- wp:gallery {"columns":2} -->
<ul class="wp-block-gallery columns-2 is-cropped"><li class="blocks-gallery-item"><figure><img src="https://cldup.com/n0g6ME5VKC.jpg" alt=""/></figure></li><li class="blocks-gallery-item"><figure><img src="https://cldup.com/ZjESfxPI3R.jpg" alt=""/></figure></li><li class="blocks-gallery-item"><figure><img src="https://cldup.com/EKNF8xD2UM.jpg" alt=""/></figure></li></ul>
<!-- /wp:gallery -->

<!-- wp:paragraph -->
<p>You can change the amount of columns in your galleries by dragging a slider in the block inspector in the sidebar.</p>
<!-- /wp:paragraph -->

<!-- wp:heading -->
<h2>Media Rich</h2>
<!-- /wp:heading -->

<!-- wp:paragraph -->
<p>If you combine the new <strong>wide</strong> and <strong>full-wide</strong> alignments with galleries, you can create a very media rich layout, very quickly:</p>
<!-- /wp:paragraph -->

<!-- wp:image {"align":"full"} -->
<figure class="wp-block-image alignfull"><img src="https://cldup.com/8lhI-gKnI2.jpg" alt="Accessibility is important — don’t forget image alt attribute"/></figure>
<!-- /wp:image -->

<!-- wp:paragraph -->
<p>Sure, the full-wide image can be pretty big. But sometimes the image is worth it.</p>
<!-- /wp:paragraph -->

<!-- wp:gallery {"align":"wide"} -->
<ul class="wp-block-gallery alignwide columns-2 is-cropped"><li class="blocks-gallery-item"><figure><img src="https://cldup.com/_rSwtEeDGD.jpg" alt=""/></figure></li><li class="blocks-gallery-item"><figure><img src="https://cldup.com/L-cC3qX2DN.jpg" alt=""/></figure></li></ul>
<!-- /wp:gallery -->

<!-- wp:paragraph -->
<p>The above is a gallery with just two images. It’s an easier way to create visually appealing layouts, without having to deal with floats. You can also easily convert the gallery back to individual images again, by using the block switcher.</p>
<!-- /wp:paragraph -->

<!-- wp:paragraph -->
<p>Any block can opt into these alignments. The embed block has them also, and is responsive out of the box:</p>
<!-- /wp:paragraph -->

<!-- wp:core-embed/vimeo {"url":"https://vimeo.com/22439234","type":"video","providerNameSlug":"vimeo","align":"wide","className":"wp-has-aspect-ratio wp-embed-aspect-16-9"} -->
<figure class="wp-block-embed-vimeo alignwide wp-block-embed is-type-video is-provider-vimeo wp-has-aspect-ratio wp-embed-aspect-16-9"><div class="wp-block-embed__wrapper">
https://vimeo.com/22439234
</div></figure>
<!-- /wp:core-embed/vimeo -->

<!-- wp:paragraph -->
<p>You can build any block you like, static or dynamic, decorative or plain. Here’s a pullquote block:</p>
<!-- /wp:paragraph -->

<!-- wp:pullquote -->
<figure class="wp-block-pullquote"><blockquote><p>Code is Poetry</p><cite>The WordPress community</cite></blockquote></figure>
<!-- /wp:pullquote -->

<!-- wp:paragraph {"align":"center"} -->
<p style="text-align:center">
\t<em>
\t\tIf you want to learn more about how to build additional blocks, or if you are interested in helping with the project, head over to the <a href="https://github.com/WordPress/gutenberg">GitHub repository</a>.\t</em>
</p>
<!-- /wp:paragraph -->

<!-- wp:button {"align":"center"} -->
<div class="wp-block-button aligncenter"><a class="wp-block-button__link" href="https://github.com/WordPress/gutenberg">Help build Gutenberg</a></div>
<!-- /wp:button -->

<!-- wp:separator -->
<hr class="wp-block-separator"/>
<!-- /wp:separator -->

<!-- wp:paragraph {"align":"center"} -->
<p style="text-align:center">Thanks for testing Gutenberg!</p>
<!-- /wp:paragraph -->

<!-- wp:paragraph {"align":"center"} -->
<p style="text-align:center"><img draggable="false" class="emoji" alt="👋" src="https://s.w.org/images/core/emoji/2.3/svg/1f44b.svg"></p>
<!-- /wp:paragraph -->"""
