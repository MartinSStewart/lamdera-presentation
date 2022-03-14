module Frontend exposing (..)

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


slides : Bool -> Int -> Size -> List (Element msg)
slides isPresenter participantCount windowSize =
    let
        isMobile =
            windowSize.width < 1000

        ifMobile ifTrue ifFalse =
            if isMobile then
                ifTrue

            else
                ifFalse

        titleFontSize =
            Element.Font.size (ifMobile 24 32)

        secondaryFontSize =
            Element.Font.size (ifMobile 18 24)

        title text =
            Element.paragraph [ titleFontSize, Element.Region.heading 1 ] [ Element.text text ]
    in
    [ Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.column
            [ Element.spacing 64
            , Element.centerX
            , Element.centerY
            , (if participantCount > 1 then
                String.fromInt (participantCount - 1)
                    ++ (if participantCount - 1 == 1 then
                            " person has joined"

                        else
                            " people have joined"
                       )
                    |> Element.text
                    |> Element.el [ Element.centerX, Element.moveDown 24 ]

               else
                Element.none
              )
                |> Element.below
            ]
            [ title "Hobby scale: making web apps with minimal fuss"
            , if isPresenter then
                Element.column
                    [ Element.spacing 8, Element.centerX ]
                    [ Element.el [ Element.centerX ] (Element.text "This presentation is interactive, join here: ")
                    , Element.el [ Element.centerX, secondaryFontSize ] (Element.text Env.domain)
                    , Element.el [ Element.centerX ] qrCodeElement
                    ]

              else
                Element.none
            ]
        )
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ title "Quick disclaimer"
        , bulletList
            isMobile
            [ Element.text "I'm going to show off a tool called Lamdera."
            , Element.text "I don't have any financial ties but I am friends with the creator of it"
            ]
        ]
    , Element.column
        [ Element.centerX, Element.centerY, titleFontSize, Element.spacing 16 ]
        [ Element.el [ Element.centerX ] (title "A little about me:")
        , Element.text "I like making web apps in my free time"
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ ifMobile
            Element.none
            (Element.paragraph
                [ titleFontSize, Element.Font.center ]
                [ Element.text "Fight over which color is the best!" ]
            )
        , iframe
            { width = windowSize.width - ifMobile 0 80, height = windowSize.height - ifMobile 60 120 }
            "https://the-best-color.lamdera.app"
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ ifMobile
            Element.none
            (Element.paragraph
                [ titleFontSize, Element.Font.center ]
                [ Element.text "Question and answer app" ]
            )
        , iframe
            { width = windowSize.width - ifMobile 0 80, height = windowSize.height - ifMobile 60 120 }
            Env.questionAndAnswerLink
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ ifMobile
            Element.none
            (Element.paragraph
                [ titleFontSize, Element.Font.center ]
                [ Element.text "Favorite moment this month?" ]
            )
        , iframe
            { width = windowSize.width - ifMobile 0 80, height = windowSize.height - ifMobile 60 120 }
            Env.momentOfTheMonthLink
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 8 ]
        [ title "2 years ago I wouldn't have bothered making these apps"
        , bulletList
            isMobile
            [ Element.text "The business logic is simple"
            , Element.text "But a lot of infrastructure work is still needed"
            ]
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 8, Element.padding 8 ]
        [ title "Fullstack app checklist"
        , Element.column
            [ secondaryFontSize, Element.spacing 16, Element.padding 16 ]
            (List.map
                (\( _, text ) ->
                    Element.row [ Element.spacing 12 ]
                        [ Element.el [ Element.alignTop ] (Element.text "☐")
                        , Element.paragraph [] [ Element.text text ]
                        ]
                )
                checklist
            )
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 8, Element.padding 8 ]
        [ title "2 years ago I started using a tool called Lamdera"
        , bulletList
            isMobile
            [ Element.text "Developed by Mario Rogic"
            , Element.text "Opinionated platform for creating and hosting full stack web apps"
            , Element.text "Apps are programmed using the Elm language"
            ]
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 8, Element.padding 8 ]
        [ title "Fullstack app checklist (for Lamdera)"
        , Element.column
            [ secondaryFontSize, Element.spacing 16, Element.padding 16 ]
            (List.map
                (\( handledByLamdera, text ) ->
                    Element.row [ Element.spacing 12 ]
                        [ Element.el [ Element.alignTop ]
                            (Element.text
                                (if handledByLamdera then
                                    "☑"

                                 else
                                    "☐"
                                )
                            )
                        , Element.paragraph [] [ Element.text text ]
                        ]
                )
                checklist
            )
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 8, Element.padding 8 ]
        [ title "What does this look like in practice?"
        , numberedList
            isMobile
            [ code "lamdera init"
            , code "lamdera live"
            , Element.text "Write the business logic for the frontend and backend"
            , Element.text "Go to the lamdera dashboard to create an app and give it name"
            , code "lamdera deploy"
            ]
        ]
    , Element.el
        [ SyntaxHighlight.useTheme SyntaxHighlight.oneDark |> Element.html |> Element.inFront, Element.centerX, Element.centerY ]
        (case SyntaxHighlight.elm bestColorBackend of
            Ok hCode ->
                SyntaxHighlight.toInlineHtml hCode
                    |> Element.html
                    |> Element.el
                        [ Element.padding 8
                        , Element.Background.color (Element.rgb255 40 44 52)
                        , Element.Font.size (ifMobile 14 16)
                        ]

            Err _ ->
                Element.none
        )
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 8, Element.padding 16, Element.Font.center ]
        [ title "So Lamdera makes it quick to make simple stuff, but what about more complicated apps?"
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ ifMobile
            Element.none
            (Element.paragraph
                [ titleFontSize, Element.Font.center ]
                [ Element.text "Draw ascii art with friends!" ]
            )
        , iframe
            { width = windowSize.width - ifMobile 0 80, height = windowSize.height - ifMobile 60 140 }
            Env.asciiCollabLink
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ ifMobile
            Element.none
            (Element.paragraph
                [ titleFontSize, Element.Font.center ]
                [ Element.text "meetup.com but free!" ]
            )
        , iframe
            { width = windowSize.width - ifMobile 0 80, height = windowSize.height - ifMobile 60 140 }
            "https://meetdown.app/"
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ ifMobile
            Element.none
            (Element.paragraph
                [ titleFontSize, Element.Font.center ]
                [ Element.text "Realia app" ]
            )
        , Element.column
            [ Element.spacing 16 ]
            [ Element.image [ Element.width Element.fill ] { src = "realia-desktop.png", description = "Desktop screenshot" }
            , Element.row
                []
                [ Element.image [ Element.width Element.fill ] { src = "realia-homepage.png", description = "Mobile homepage screenshot" }
                , Element.image [ Element.width Element.fill ] { src = "realia-mappage.png", description = "Mobile map page screenshot" }
                ]
            ]
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ title "In conclusion..."
        , bulletList isMobile
            [ Element.text "Lamdera is cool!"
            , Element.text "If you are willing to learn Elm I think it's worth trying!"
            , Element.text "It's well suited for complicated apps but it's not well suited for memory/CPU heavy apps"
            ]
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ title "Links"
        , [ { website = "https://the-best-color.lamdera.app"
            , github = "https://github.com/MartinSStewart/best-color"
            }
          , { website = "https://question-and-answer.app"
            , github = "https://github.com/MartinSStewart/elm-qna"
            }
          , { website = "https://moment-of-the-month.lamdera.app"
            , github = "https://github.com/MartinSStewart/elm-moment-of-the-month"
            }
          , { website = "https://ascii-collab.app"
            , github = "https://github.com/MartinSStewart/ascii-collab"
            }
          , { website = "https://meetdown.app"
            , github = "https://github.com/MartinSStewart/meetdown"
            }
          ]
            |> List.map
                (\{ website, github } ->
                    Element.column
                        [ Element.spacing 8 ]
                        [ Element.row
                            [ Element.spacing 8 ]
                            [ Element.text "🔗"
                            , Element.newTabLink
                                [ secondaryFontSize
                                , Element.Font.color (Element.rgb 0.3 0.4 0.8)
                                , Element.Font.underline
                                ]
                                { url = website, label = Element.paragraph [] [ Element.text (removeHttps website) ] }
                            ]
                        , Element.row
                            [ Element.spacing 8 ]
                            [ githubLogo
                            , Element.newTabLink
                                [ secondaryFontSize
                                , Element.Font.color (Element.rgb 0.3 0.4 0.8)
                                , Element.Font.underline
                                ]
                                { url = website, label = Element.paragraph [] [ Element.text (removeHttps github) ] }
                            ]
                        ]
                )
            |> Element.column [ Element.spacing 16 ]
        ]
    ]


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


bulletList : Bool -> List (Element msg) -> Element msg
bulletList isMobile elements =
    Element.column
        [ (if isMobile then
            18

           else
            24
          )
            |> Element.Font.size
        , Element.spacing 16
        , Element.padding 12
        ]
        (List.map
            (\item ->
                Element.row
                    [ Element.spacing 12 ]
                    [ Element.el
                        [ Element.Background.color (Element.rgb 0 0 0)
                        , Element.width (Element.px 8)
                        , Element.height (Element.px 8)
                        , Element.Border.rounded 99
                        , Element.alignTop
                        , Element.moveDown
                            (if isMobile then
                                4

                             else
                                8
                            )
                        ]
                        Element.none
                    , Element.paragraph [] [ item ]
                    ]
            )
            elements
        )


numberedList : Bool -> List (Element msg) -> Element msg
numberedList isMobile elements =
    Element.column
        [ (if isMobile then
            18

           else
            24
          )
            |> Element.Font.size
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
        { init = ( { currentColor = Blue, changeCount = 0, lastChangedBy = Nothing }, Cmd.none )
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
    [ ( True, "Set up database" )
    , ( True, "Write code to query/write to database" )
    , ( True, "Write code to handle sending data to/from frontend/backend" )
    , ( True, "Setup hot reloading for local development" )
    , ( True, "Setup running the backend for local development" )
    , ( True, "Write deploy scripts for database, frontend, and backend" )
    , ( True, "Setup server hosting" )
    , ( False, "Write backend business logic" )
    , ( False, "Write frontend business logic" )
    ]


centered : Element msg -> Element msg
centered element =
    Element.el [ Element.centerX, Element.centerY ] element


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
                                |> clamp 0 (List.length (slides True presenter.participants presenter.windowSize))
                    in
                    ( Presenter
                        { presenter
                            | keys = newKeys
                            , currentSlide = newSlide
                        }
                    , if presenter.currentSlide /= newSlide then
                        Lamdera.sendToBackend (ChangeSlideRequest newSlide)

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
            []
            (case model of
                Loading _ _ _ ->
                    Element.none

                Presenter presenter ->
                    List.getAt presenter.currentSlide (slides True presenter.participants presenter.windowSize)
                        |> Maybe.withDefault Element.none

                Viewer viewer ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        ]
                        [ case List.getAt viewer.currentSlide (slides False viewer.participants viewer.windowSize) of
                            Just slide ->
                                Element.el
                                    [ Element.Region.mainContent
                                    , Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                    slide

                            Nothing ->
                                Element.none
                        , Element.row
                            [ Element.centerX
                            , Element.width <| Element.maximum 800 Element.fill
                            , Element.Region.navigation
                            , Element.spacing 4
                            ]
                            [ Element.Input.button
                                [ Element.padding 16
                                , Element.width Element.fill
                                , Element.Background.color (Element.rgb 0.6 0.6 0.6)
                                , if viewer.currentSlide > 0 then
                                    Element.alpha 1

                                  else
                                    Element.alpha 0.5
                                ]
                                { onPress = Just PressedGotoPreviousSlide
                                , label = Element.el [ Element.centerX ] (Element.text "← Previous")
                                }
                            , Element.Input.button
                                [ Element.padding 16
                                , Element.width Element.fill
                                , Element.Background.color (Element.rgb 0.6 0.6 0.6)
                                , if viewer.currentSlide < viewer.latestSlide then
                                    Element.alpha 1

                                  else
                                    Element.alpha 0.5
                                ]
                                { onPress = Just PressedGotoNextSlide
                                , label = Element.el [ Element.centerX ] (Element.text "Next →")
                                }
                            ]
                        ]
            )
        ]
    }


qrCodeElement =
    case QRCode.fromString Env.domain of
        Ok qrCode ->
            QRCode.toSvg
                [ Svg.Attributes.width "200px"
                , Svg.Attributes.height "200px"
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
        |> Element.el [ Element.centerX, Element.Border.shadow { offset = ( 0, 2 ), blur = 8, size = 0, color = Element.rgba 0 0 0 0.4 } ]
