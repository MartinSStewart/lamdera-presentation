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
import Html.Attributes as Attr
import Keyboard
import Lamdera
import List.Extra as List
import QRCode
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
        [ Element.centerX, Element.centerY, Element.spacing 8 ]
        [ title "2 years ago I wouldn't have bothered making this app"
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
        , Element.paragraph [] [ Element.text "1. ", code "lamdera init" ]
        , Element.paragraph [] [ Element.text "2. ", code "lamdera live" ]
        , Element.paragraph [] [ Element.text "3. Write the business logic for the frontend and backend" ]
        , Element.paragraph [] [ Element.text "4. Go to the lamdera dashboard to create an app and give it name" ]
        , Element.paragraph [] [ Element.text "5. ", code "lamdera deploy", Element.text " (repeat if you make more changes)" ]
        ]
    , Element.el
        [ SyntaxHighlight.useTheme SyntaxHighlight.oneDark |> Element.html |> Element.inFront, Element.centerX ]
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
    ]


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


bestColorBackend =
    """module Backend exposing (app)

import ColorIndex
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)

app =
    Lamdera.backend
        { init = ( { currentColor = ColorIndex.Blue, changeCount = 0, lastChangedBy = Nothing }, Cmd.none )
        , update = \\_ model -> ( model, Cmd.none )
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \\_ -> Sub.none
        }

updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
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
        [ Attr.src src
        , Attr.width width
        , Attr.height height
        , Attr.style "border" "0"
        ]
        []
        |> Element.html
        |> Element.el [ Element.centerX, Element.Border.shadow { offset = ( 0, 2 ), blur = 8, size = 0, color = Element.rgba 0 0 0 0.4 } ]
