module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Region
import Env
import Html
import Html.Attributes
import Keyboard
import Lamdera
import List.Extra as List
import QRCode
import Svg
import Svg.Attributes
import SyntaxHighlight
import Task
import Types exposing (..)
import Url
import Url.Parser exposing ((<?>))
import Url.Parser.Query


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Sub.map KeyboardMsg Keyboard.subscriptions
                    , Browser.Events.onResize (\w h -> GotWindowSize { width = w, height = h })
                    ]
        , view = view
        }


urlParser : Url.Parser.Parser (Maybe String -> b) b
urlParser =
    Url.Parser.top <?> Url.Parser.Query.string "password"


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( Loading key Nothing Nothing
    , Cmd.batch
        [ case Url.Parser.parse urlParser url of
            Just (Just password) ->
                Lamdera.sendToBackend (GetDataRequest (Just password))

            _ ->
                Lamdera.sendToBackend (GetDataRequest Nothing)
        , Browser.Navigation.replaceUrl key "/"
        , Task.perform
            (\{ scene } -> GotWindowSize { width = floor scene.width, height = floor scene.height })
            Browser.Dom.getViewport
        ]
    )


slides : Int -> Size -> List ( Element msg, Element msg )
slides participantCount windowSize =
    let
        isMobile =
            windowSize.width < 1000

        ifMobile ifTrue ifFalse =
            if isMobile then
                ifTrue

            else
                ifFalse

        titleFontSize =
            Element.Font.size (ifMobile 24 40)

        secondaryFontSize =
            Element.Font.size (ifMobile 18 36)

        title text =
            Element.paragraph
                [ titleFontSize
                , Element.Region.heading 1
                , Element.spacing 20
                , Element.Font.bold
                ]
                [ Element.text text ]

        waitingOnInteractiveSlide =
            Element.el
                [ Element.centerX, Element.centerY, secondaryFontSize ]
                (Element.text "Nothing to interact with yet!")

        moveInteractiveSlidesToCome =
            Element.el
                [ Element.centerX, Element.centerY, secondaryFontSize ]
                (Element.text "More interactive slides to come!")

        noMoreInteractiveSlides =
            Element.el
                [ Element.centerX, Element.centerY, secondaryFontSize ]
                (Element.text "No more interactive slides :(")

        standardSlide =
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.el [ Element.alignBottom, Element.padding 4 ] (Element.text Env.domain) |> Element.inFront
                , Element.clip
                ]

        iframeSlide : String -> String -> Element msg
        iframeSlide titleText src =
            Element.column
                [ Element.centerX, Element.centerY, Element.spacing 16 ]
                [ ifMobile
                    Element.none
                    (Element.paragraph
                        [ titleFontSize, Element.Font.center ]
                        [ Element.text titleText ]
                    )
                , iframeWithShadow
                    { width = windowSize.width - 80, height = windowSize.height - 140 }
                    src
                ]
                |> standardSlide

        linksSlide : Element msg
        linksSlide =
            Element.column
                columnHelper
                [ title "Links"
                , [ { youtube = Nothing
                    , website = Just "https://lamdera.com"
                    , github = Nothing
                    }
                  , { youtube = Nothing
                    , website = Just "https://dashboard.lamdera.app/shouldnt-use"
                    , github = Nothing
                    }
                  , { youtube = Nothing
                    , website = Just "https://the-best-color.lamdera.app"
                    , github = Just "https://github.com/MartinSStewart/best-color"
                    }
                  , { youtube = Nothing
                    , website = Just "https://question-and-answer.app"
                    , github = Just "https://github.com/MartinSStewart/elm-qna"
                    }
                  , { youtube = Nothing
                    , website = Just "https://moment-of-the-month.app/"
                    , github = Just "https://github.com/MartinSStewart/elm-moment-of-the-month"
                    }
                  , { youtube = Nothing
                    , website = Just "https://ascii-collab.app"
                    , github = Just "https://github.com/MartinSStewart/ascii-collab"
                    }
                  , { youtube = Nothing
                    , website = Just "https://meetdown.app"
                    , github = Just "https://github.com/MartinSStewart/meetdown"
                    }
                  , { youtube = Nothing
                    , website = Just "https://state-of-elm.lamdera.app/"
                    , github = Just "https://github.com/MartinSStewart/state-of-elm"
                    }
                  , { youtube = Nothing
                    , website = Just "https://sheep-game.lamdera.app/join/a217210861"
                    , github = Nothing
                    }
                  , { youtube = Nothing
                    , website = Just "https://realia.se"
                    , github = Nothing
                    }
                  , { youtube = Nothing
                    , website = Just "https://air-hockey-racing.lamdera.app"
                    , github = Just "https://github.com/MartinSStewart/air-hockey-racing"
                    }
                  , { youtube = Nothing
                    , website = Just Env.domain
                    , github = Just "https://github.com/MartinSStewart/lamdera-presentation"
                    }
                  , { youtube = Nothing
                    , website = Nothing
                    , github = Just "https://github.com/MartinSStewart/elm-review-bot"
                    }
                  , { youtube = Just "https://www.youtube.com/watch?v=lw1E9sPbq28"
                    , website = Nothing
                    , github = Nothing
                    }
                  ]
                    |> List.map
                        (\{ youtube, website, github } ->
                            Element.column
                                [ Element.spacing 8, Element.Font.size 16 ]
                                [ case youtube of
                                    Just youtube_ ->
                                        Element.row
                                            [ Element.spacing 4 ]
                                            [ youtubeIcon
                                            , Element.newTabLink
                                                [ Element.Font.color (Element.rgb 0.3 0.4 0.8)
                                                , Element.Font.underline
                                                ]
                                                { url = youtube_, label = Element.paragraph [] [ Element.text (removeHttps youtube_) ] }
                                            ]

                                    Nothing ->
                                        Element.none
                                , case website of
                                    Just website_ ->
                                        Element.row
                                            [ Element.spacing 4 ]
                                            [ Element.el [ Element.Font.size 14 ] (Element.text "🔗")
                                            , Element.newTabLink
                                                [ Element.Font.color (Element.rgb 0.3 0.4 0.8)
                                                , Element.Font.underline
                                                ]
                                                { url = website_, label = Element.paragraph [] [ Element.text (removeHttps website_) ] }
                                            ]

                                    Nothing ->
                                        Element.none
                                , case github of
                                    Just github_ ->
                                        Element.row
                                            [ Element.spacing 4 ]
                                            [ githubLogo
                                            , Element.newTabLink
                                                [ Element.Font.color (Element.rgb 0.3 0.4 0.8)
                                                , Element.Font.underline
                                                ]
                                                { url = github_, label = Element.paragraph [] [ Element.text (removeHttps github_) ] }
                                            ]

                                    Nothing ->
                                        Element.none
                                ]
                        )
                    |> Element.column [ Element.spacing 24 ]
                ]
    in
    [ ( Element.image
            [ Element.width Element.fill
            , Element.centerY
            ]
            { src = "aar-2022-banner-speaker 1.png", description = "" }
      , Element.none
      )
    , ( Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.el [ Element.alignBottom, Element.padding 8, Element.Font.size 36 ] (Element.text Env.domain) |> Element.inFront
            ]
            (Element.column
                [ Element.spacing 32
                , Element.centerX
                , Element.centerY
                , Element.padding 16
                , (if participantCount > 1 then
                    String.fromInt (participantCount - 1)
                        ++ (if participantCount - 1 == 1 then
                                " person has joined"

                            else
                                " people have joined"
                           )
                        |> Element.text
                        |> Element.el [ Element.centerX, secondaryFontSize ]

                   else
                    Element.none
                  )
                    |> Element.below
                ]
                [ title "Hobby scale: making web apps with minimal fuss"
                , Element.column
                    [ Element.spacing 8, Element.centerX ]
                    [ Element.el [ Element.centerX, Element.Font.size 36 ] (Element.text "This presentation is interactive, join here: ")
                    , Element.el [ Element.centerX ] qrCodeElement
                    ]
                ]
            )
      , waitingOnInteractiveSlide
      )
    , ( Element.column
            columnHelper
            [ title "Quick disclaimer"
            , bulletList
                [ Element.text "I'm going to show off a tool called Lamdera."
                , Element.text "I don't have any financial ties but I am friends with the creator of it"
                ]
            ]
            |> standardSlide
      , waitingOnInteractiveSlide
      )
    , ( Element.column
            [ Element.centerX, Element.centerY, titleFontSize, Element.spacing 16 ]
            [ Element.el [ Element.centerX ] (title "A little about me:")
            , Element.text "As a hobby, I make web apps"
            ]
            |> standardSlide
      , waitingOnInteractiveSlide
      )
    , ( iframeSlide "Fight over which color is the best!" "https://the-best-color.lamdera.app"
      , iframe windowSize "https://the-best-color.lamdera.app"
      )
    , ( iframeSlide "Question and answer app" Env.questionAndAnswerHostLink
      , iframe windowSize Env.questionAndAnswerLink
      )
    , ( iframeSlide "Favorite moment this month?" Env.momentOfTheMonthLink
      , iframe windowSize Env.momentOfTheMonthLink
      )
    , ( Element.column
            columnHelper
            [ title "2 years ago I wouldn't have bothered making these apps"
            , bulletList
                [ Element.text "The business logic is simple"
                , Element.text "But a lot of infrastructure work is still needed"
                , Element.text "For a job I might tolerate that, but not in my free time!"
                ]
            ]
            |> standardSlide
      , moveInteractiveSlidesToCome
      )
    , ( Element.column
            [ Element.centerX, Element.centerY, Element.spacing 8, Element.padding 8 ]
            [ title "Web app checklist"
            , Element.column
                [ secondaryFontSize, Element.spacing 16, Element.padding 16 ]
                (List.map
                    (\( _, text ) ->
                        Element.row [ Element.spacing 12 ]
                            [ Element.el [ Element.alignTop ] (checkbox False)
                            , Element.paragraph [] [ Element.text text ]
                            ]
                    )
                    checklist
                )
            ]
            |> standardSlide
      , moveInteractiveSlidesToCome
      )
    , ( Element.column
            columnHelper
            [ title "2 years ago I started using a tool called Lamdera"
            , bulletList
                [ Element.text "Developed by Mario Rogic"
                , Element.text "Opinionated platform for creating and hosting full stack web apps"
                , Element.text "Apps are programmed using the Elm language"
                ]
            ]
            |> standardSlide
      , moveInteractiveSlidesToCome
      )
    , ( Element.column
            columnHelper
            [ title "Web app checklist (for Lamdera)"
            , Element.column
                [ secondaryFontSize, Element.spacing 16, Element.padding 16 ]
                (List.map
                    (\( handledByLamdera, text ) ->
                        Element.row [ Element.spacing 12 ]
                            [ Element.el [ Element.alignTop ]
                                (if handledByLamdera then
                                    checkbox True

                                 else
                                    checkbox False
                                )
                            , Element.paragraph [] [ Element.text text ]
                            ]
                    )
                    checklist
                )
            ]
            |> standardSlide
      , moveInteractiveSlidesToCome
      )
    , ( Element.column
            columnHelper
            [ title "What does this look like in practice?"
            , numberedList
                [ code "lamdera init"
                , code "lamdera live"
                , Element.text "Write the business logic for the app"
                , Element.text "Go to the lamdera dashboard to create an app and give it name"
                , code "lamdera deploy"
                ]
            ]
            |> standardSlide
      , moveInteractiveSlidesToCome
      )
    , ( Element.el
            [ SyntaxHighlight.useTheme SyntaxHighlight.oneDark |> Element.html |> Element.inFront
            , Element.centerX
            , Element.centerY
            , ifMobile
                Element.none
                (Element.image
                    [ Element.width (Element.px 360)
                    , Element.alignRight
                    , Element.moveRight 110
                    , Element.moveUp 50
                    ]
                    { src = "best-color-files.png", description = "Best color files" }
                )
                |> Element.inFront
            ]
            (case bestColorBackend of
                Ok hCode ->
                    Html.div [ Html.Attributes.style "white-space" "pre-wrap" ] [ SyntaxHighlight.toInlineHtml hCode ]
                        |> Element.html
                        |> Element.el
                            [ Element.padding 8
                            , Element.Background.color (Element.rgb255 40 44 52)
                            , Element.Font.size 24
                            , Element.Border.glow (Element.rgba 0 0 0 0.2) 8
                            ]

                Err _ ->
                    Element.none
            )
            |> standardSlide
      , moveInteractiveSlidesToCome
      )
    , ( Element.column
            [ Element.centerX, Element.centerY, Element.spacing 8, Element.padding 16, Element.Font.center ]
            [ title "So it's quick to make simple stuff, but\u{00A0}what\u{00A0}about\u{00A0}more\u{00A0}complicated\u{00A0}use\u{00A0}cases?"
            ]
            |> standardSlide
      , moveInteractiveSlidesToCome
      )
    , ( iframeSlide "Draw ascii art with friends!" Env.asciiCollabLink
      , iframe windowSize Env.asciiCollabLink
      )
    , ( iframeSlide "meetup.com but free!" "https://meetdown.app/"
      , iframe windowSize "https://meetdown.app/"
      )
    , ( iframeSlide "State of Elm survey" "https://state-of-elm.lamdera.app/"
      , iframe windowSize "https://state-of-elm.lamdera.app/"
      )
    , ( iframeSlide "The Sheep Game" "https://sheep-game.lamdera.app/join/a217210861"
      , iframe windowSize "https://sheep-game.lamdera.app/join/a217210861"
      )
    , let
        realiaLink =
            "https://realia.lamdera.app/map?a=AQABAAAAm0VpQkxkVzVuYzJkaGRHRnVJREVzSUZOMGIyTnJhRzlzYlN3Z1UzWmxjbWxuWlNKUUVrNEtOQW95Q2VXMHM1VmNuVjlHRVdPS2Q1WmdVS1lsR2g0TEVPN0I3cUVCR2hRS0VnbkpFbUh5WEoxZlJoRVNWZ1lYOF80QUV3d1FBU29VQ2hJSmZRekZTbWVkWDBZUnlveGxSUUpSMzg0AAAAF0t1bmdzZ2F0YW4gMSwgU3RvY2tob2xtAAAAmkVoOVFiM04wWjJGMFlXNGdNU3dnUjhPMmRHVmliM0puTENCVGRtVnlhV2RsSWxBU1RnbzBDaklKWjhlVUFtZnpUMFlSQkRNMHdMdC1pZ0FhSGdzUTdzSHVvUUVhRkFvU0NibHJfejFuODA5R0VUT1Y5d2Q0a0FFTERCQUJLaFFLRWdtbEtpenJadk5QUmhHVy1XM2MwZjI5cncAAAAWUG9zdGdhdGFuIDEsIEfDtnRlYm9yZwAAAAAABAAD"
      in
      ( iframeSlide "Realia app" realiaLink
      , iframeWithShadow windowSize realiaLink
      )
    , ( iframeSlide "Multiplayer game" "https://air-hockey-racing.lamdera.app"
      , moveInteractiveSlidesToCome
      )
    , ( iframeSlide "Multiplayer game (with the audience?)" "https://air-hockey-racing.lamdera.app"
      , iframe windowSize "https://air-hockey-racing.lamdera.app"
      )
    , ( Element.column
            columnHelper
            [ title "And more!"
            , bulletList
                [ Element.text "New years eve present for friends"
                , Element.text "Reusing ascii-collab for a get-well-soon card"
                , Element.text "Reusing ascii-collab for a goodbye card"
                , Element.text "This presentation!"
                , Element.text "Another presentation!"
                , Element.text "Translation editing app for work"
                , Element.text "Discord bot"
                , Element.text "elm-review Github bot"
                ]
            ]
            |> standardSlide
      , noMoreInteractiveSlides
      )
    , ( Element.column
            columnHelper
            [ title "Tradeoffs"
            , bulletList
                [ Element.text "Elm only"
                , Element.text "Limited CPU and memory resources (not web-scale)"
                , Element.text "Vendor lock-in"
                ]
            ]
            |> standardSlide
      , noMoreInteractiveSlides
      )
    , ( Element.column
            columnHelper
            [ title "In conclusion..."
            , bulletList
                [ Element.text "Lamdera is cool!"
                , Element.text "It's not suitable for all web apps"
                , Element.text "But when you can use it, it can save you a lot of effort"
                ]
            ]
            |> standardSlide
      , noMoreInteractiveSlides
      )
    , ( linksSlide, linksSlide )
    , ( Element.image
            [ Element.width Element.fill
            , Element.centerY
            ]
            { src = "aar-2022-banner-speaker 2.png", description = "" }
      , linksSlide
      )
    ]


columnHelper =
    [ Element.centerX, Element.centerY, Element.spacing 16, Element.padding 16 ]


youtubeIcon =
    Svg.svg
        [ Svg.Attributes.width "17"
        , Svg.Attributes.height "12"
        , Svg.Attributes.viewBox "0 0 159 110"
        ]
        [ Svg.path
            [ Svg.Attributes.d "m154 17.5c-1.82-6.73-7.07-12-13.8-13.8-9.04-3.49-96.6-5.2-122 0.1-6.73 1.82-12 7.07-13.8 13.8-4.08 17.9-4.39 56.6 0.1 74.9 1.82 6.73 7.07 12 13.8 13.8 17.9 4.12 103 4.7 122 0 6.73-1.82 12-7.07 13.8-13.8 4.35-19.5 4.66-55.8-0.1-75z"
            , Svg.Attributes.fill "#f00"
            ]
            []
        , Svg.path
            [ Svg.Attributes.d "m105 55-40.8-23.4v46.8z"
            , Svg.Attributes.fill "#fff"
            ]
            []
        ]
        |> Element.html
        |> Element.el []


checkbox : Bool -> Element msg
checkbox isChecked_ =
    Element.el
        [ Element.width <| Element.px 24
        , Element.height <| Element.px 24
        , Element.Border.color (Element.rgb 0 0 0)
        , Element.Border.width 2
        , Element.moveDown 5
        ]
        (if isChecked_ then
            Element.el [ Element.centerX, Element.centerY ] checkboxCheck

         else
            Element.none
        )


checkboxCheck : Element msg
checkboxCheck =
    Svg.svg
        [ Svg.Attributes.width "22"
        , Svg.Attributes.height "20"
        , Svg.Attributes.viewBox "0 0 12 10"
        , Svg.Attributes.fill "none"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M1.5 5.91734L3.3375 8.5251C3.4072 8.62921 3.50076 8.71517 3.61038 8.77583C3.72001 8.83648 3.84254 8.87008 3.96778 8.87382C4.09301 8.87756 4.21733 8.85134 4.33038 8.79734C4.44344 8.74333 4.54196 8.66311 4.61775 8.56334L10.5 1.12109"
            , Svg.Attributes.stroke "currentColor"
            , Svg.Attributes.strokeWidth "3.5"
            , Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.strokeLinejoin "round"
            ]
            []
        ]
        |> Element.html
        |> Element.el []


removeHttps url =
    if String.startsWith "https://" url then
        String.dropLeft (String.length "https://") url

    else
        url


githubLogo : Element msg
githubLogo =
    Svg.svg
        [ Svg.Attributes.height "18"
        , Svg.Attributes.viewBox "0 0 16 16"
        , Svg.Attributes.version "1.1"
        , Svg.Attributes.width "18"
        ]
        [ Svg.path
            [ Svg.Attributes.fillRule "evenodd"
            , Svg.Attributes.d "M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"
            ]
            []
        ]
        |> Element.html
        |> Element.el []


bulletList : List (Element msg) -> Element msg
bulletList elements =
    Element.column
        [ Element.Font.size 36
        , Element.spacing 16
        , Element.padding 12
        ]
        (List.map
            (\item ->
                Element.row
                    [ Element.spacing 12 ]
                    [ Element.el
                        [ Element.Background.color (Element.rgb 0 0 0)
                        , Element.width (Element.px 12)
                        , Element.height (Element.px 12)
                        , Element.Border.rounded 99
                        , Element.alignTop
                        , Element.moveDown 12
                        ]
                        Element.none
                    , Element.paragraph [] [ item ]
                    ]
            )
            elements
        )


numberedList : List (Element msg) -> Element msg
numberedList elements =
    Element.column
        [ Element.Font.size 36
        , Element.spacing 16
        , Element.padding 12
        ]
        (List.indexedMap
            (\index item ->
                Element.row
                    [ Element.spacing 12 ]
                    [ Element.el [ Element.alignTop ] (Element.text (String.fromInt (index + 1) ++ ". "))
                    , Element.paragraph [] [ item ]
                    ]
            )
            elements
        )


bestColorBackend =
    """module Backend exposing (app)

import ColorIndex exposing (ColorIndex(..))
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)

app =
    Lamdera.backend
        { init =
            ( { currentColor = Blue, changeCount = 0, lastChangedBy = Nothing }
            , Cmd.none
            )
        , update = \\_ model -> ( model, Cmd.none )
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \\_ -> Sub.none
        }

updateFromFrontend sessionId clientId msg model =
    case msg of
        ClientConnected ->
            ( model
            , Lamdera.sendToFrontend clientId (UpdateColor model.currentColor model.changeCount)
            )

        ColorChosen color ->
            let
                changeCount =
                    if model.lastChangedBy == Just sessionId then
                        model.changeCount

                    else
                        model.changeCount + 1
            in
            ( { currentColor = color, lastChangedBy = Just sessionId, changeCount = changeCount }
            , Lamdera.broadcast (UpdateColor color changeCount)
            )
"""
        |> SyntaxHighlight.elm


code : String -> Element msg
code text =
    Element.el
        [ Element.Font.family [ Element.Font.monospace ]
        , Element.Background.color (Element.rgb 0.92 0.92 0.92)
        , Element.Border.rounded 4
        , Element.paddingEach { left = 4, right = 4, top = 2, bottom = 0 }
        ]
        (Element.text text)


checklist : List ( Bool, String )
checklist =
    [ ( True, "Database" )
    , ( True, "Communicate to/from frontend/backend" )
    , ( True, "Hot reloading and running backend locally" )
    , ( True, "CI/CD" )
    , ( True, "Hosting" )
    , ( False, "Write the actual app" )
    ]


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl (Types.getKey model) (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        KeyboardMsg keyMsg ->
            case model of
                Presenter presenter ->
                    let
                        newKeys =
                            Keyboard.update keyMsg presenter.keys

                        keyDown key =
                            List.any ((==) key) presenter.keys

                        keyPressed key =
                            List.any ((==) key) presenter.keys && not (List.any ((==) key) newKeys)

                        newSlide : Int
                        newSlide =
                            (if keyPressed Keyboard.ArrowRight then
                                presenter.currentSlide + 1

                             else if keyPressed Keyboard.ArrowLeft then
                                presenter.currentSlide - 1

                             else
                                presenter.currentSlide
                            )
                                |> clamp 0 (List.length (slides presenter.participants presenter.windowSize))
                    in
                    ( Presenter
                        { presenter
                            | keys = newKeys
                            , currentSlide = newSlide
                        }
                    , if presenter.currentSlide /= newSlide then
                        Lamdera.sendToBackend (ChangeSlideRequest newSlide)

                      else if
                        (keyDown Keyboard.Alt && keyPressed (Keyboard.Character "R"))
                            || keyPressed (Keyboard.Character "®")
                      then
                        Lamdera.sendToBackend ResetPresentation

                      else
                        Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotWindowSize size ->
            ( case model of
                Loading navigationKey _ maybeData ->
                    tryLoading navigationKey (Just size) maybeData

                Presenter presenter ->
                    Presenter { presenter | windowSize = size }

                Viewer viewer ->
                    Viewer { viewer | windowSize = size }
            , Cmd.none
            )

        PressedGotoPreviousSlide ->
            ( case model of
                Loading _ _ _ ->
                    model

                Presenter _ ->
                    model

                Viewer viewer ->
                    Viewer { viewer | currentSlide = viewer.currentSlide - 1 |> max 0 }
            , Cmd.none
            )

        PressedGotoNextSlide ->
            ( case model of
                Loading _ _ _ ->
                    model

                Presenter _ ->
                    model

                Viewer viewer ->
                    Viewer { viewer | currentSlide = viewer.currentSlide + 1 |> min viewer.latestSlide }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        ParticipantCountChanged participants ->
            ( case model of
                Loading _ _ _ ->
                    model

                Presenter presenter ->
                    Presenter { presenter | participants = participants }

                Viewer viewer ->
                    Viewer { viewer | participants = participants }
            , Cmd.none
            )

        GetDataResponse data ->
            ( case model of
                Loading navigationKey maybeWindowSize _ ->
                    tryLoading navigationKey maybeWindowSize (Just data)

                _ ->
                    model
            , Cmd.none
            )

        ChangeSlideNotification slide ->
            case model of
                Viewer viewer ->
                    ( Viewer
                        { viewer
                            | currentSlide =
                                if viewer.currentSlide == viewer.latestSlide then
                                    slide

                                else
                                    viewer.currentSlide
                            , latestSlide = slide
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


tryLoading : Browser.Navigation.Key -> Maybe Size -> Maybe Data -> FrontendModel
tryLoading navigationKey maybeWindowSize maybeData =
    case ( maybeData, maybeWindowSize ) of
        ( Just (PresenterData { participants, latestSlide }), Just windowSize ) ->
            Presenter
                { currentSlide = latestSlide
                , navigationKey = navigationKey
                , participants = participants
                , keys = []
                , windowSize = windowSize
                }

        ( Just (ViewerData { participants, latestSlide }), Just windowSize ) ->
            Viewer
                { currentSlide = latestSlide
                , latestSlide = latestSlide
                , navigationKey = navigationKey
                , participants = participants
                , windowSize = windowSize
                }

        _ ->
            Loading navigationKey maybeWindowSize maybeData


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout
            [ Element.height Element.fill ]
            (case model of
                Loading _ _ _ ->
                    Element.none

                Presenter presenter ->
                    case List.getAt presenter.currentSlide (slides presenter.participants presenter.windowSize) of
                        Just ( slide, _ ) ->
                            slide

                        Nothing ->
                            Element.none

                Viewer viewer ->
                    case List.getAt viewer.currentSlide (slides viewer.participants viewer.windowSize) of
                        Just ( _, slide ) ->
                            slide

                        Nothing ->
                            Element.none
            )
        ]
    }


qrCodeElement : Element msg
qrCodeElement =
    case QRCode.fromString Env.domain of
        Ok qrCode ->
            QRCode.toSvg
                [ Svg.Attributes.width "400px"
                , Svg.Attributes.height "400px"
                ]
                qrCode
                |> Element.html

        Err _ ->
            Element.none


iframe : { width : Int, height : Int } -> String -> Element msg
iframe { width, height } src =
    Html.iframe
        [ Html.Attributes.src src
        , Html.Attributes.width width
        , Html.Attributes.height height
        , Html.Attributes.style "border" "0"
        ]
        []
        |> Element.html


iframeWithShadow : { width : Int, height : Int } -> String -> Element msg
iframeWithShadow size src =
    Element.el
        [ Element.centerX
        , Element.Border.shadow { offset = ( 0, 2 ), blur = 8, size = 0, color = Element.rgba 0 0 0 0.4 }
        ]
        (iframe size src)
