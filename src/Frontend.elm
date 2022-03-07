module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Element exposing (Element)
import Element.Font
import Env
import Html
import Html.Attributes as Attr
import Keyboard
import Lamdera
import List.Extra as List
import QRCode
import Svg.Attributes
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


titleFontSize =
    Element.Font.size 32


slides : Bool -> Int -> Size -> List (Element msg)
slides isPresenter participantCount windowSize =
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
                    ++ (if participantCount == 1 then
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
            [ Element.el [ titleFontSize ] (Element.text "Hobby scale: making web apps with minimal fuss")
            , if isPresenter then
                Element.column
                    [ Element.spacing 8, Element.centerX ]
                    [ Element.el [ Element.centerX ] (Element.text "This presentation is interactive, join here: ")
                    , Element.el [ Element.centerX, Element.Font.size 24 ] (Element.text Env.domain)
                    , Element.el [ Element.centerX ] qrCodeElement
                    ]

              else
                Element.none
            ]
        )
    , Element.column
        [ Element.centerX, Element.centerY, titleFontSize, Element.spacing 16 ]
        [ Element.el [ Element.centerX ] (Element.text "A little about me:")
        , Element.text "I like making web apps in my free time"
        ]
    , Element.column
        [ Element.centerX, Element.centerY, Element.spacing 16 ]
        [ Element.paragraph [ titleFontSize, Element.Font.center ] [ Element.text "Here's an app where you can fight over which color is the best" ]
        , iframe
            { width = windowSize.width - 500, height = windowSize.height - 160 }
            "https://the-best-color.lamdera.app"
            |> Element.html
            |> Element.el [ Element.centerX ]
        ]
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
                    ( Viewer { viewer | latestSlide = slide }, Cmd.none )

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
                    List.getAt viewer.currentSlide (slides False viewer.participants viewer.windowSize)
                        |> Maybe.withDefault Element.none
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


iframe : { width : Int, height : Int } -> String -> Html.Html msg
iframe { width, height } src =
    Html.iframe
        [ Attr.src src
        , Attr.width width
        , Attr.height height
        , Attr.style "border" "0"
        ]
        []
