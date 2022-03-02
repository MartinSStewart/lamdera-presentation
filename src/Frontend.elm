module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Element
import Html
import Html.Attributes as Attr
import Keyboard
import Lamdera
import List.Extra as List
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
        , subscriptions = \_ -> Sub.map KeyboardMsg Keyboard.subscriptions
        , view = view
        }


urlParser : Url.Parser.Parser (Maybe String -> b) b
urlParser =
    Url.Parser.top <?> Url.Parser.Query.string "password"


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( Loading key
    , Cmd.batch
        [ case Url.Parser.parse urlParser url of
            Just (Just password) ->
                Lamdera.sendToBackend (GetDataRequest (Just password))

            _ ->
                Lamdera.sendToBackend (GetDataRequest Nothing)
        , Browser.Navigation.replaceUrl key "/"
        ]
    )


slides =
    [ Element.text "Slide 1"
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

        UrlChanged url ->
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
                            if keyPressed Keyboard.ArrowRight then
                                presenter.currentSlide + 1

                            else if keyPressed Keyboard.ArrowLeft then
                                presenter.currentSlide - 1

                            else
                                presenter.currentSlide
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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        ParticipantCountChanged participants ->
            ( case model of
                Loading _ ->
                    model

                Presenter presenter ->
                    Presenter { presenter | participants = participants }

                Viewer viewer ->
                    Viewer { viewer | participants = participants }
            , Cmd.none
            )

        GetDataResponse result ->
            ( case model of
                Loading navigationKey ->
                    case result of
                        PresenterData { participants, latestSlide } ->
                            Presenter
                                { currentSlide = latestSlide
                                , navigationKey = navigationKey
                                , participants = participants
                                , keys = []
                                }

                        ViewerData { participants, latestSlide } ->
                            Viewer
                                { currentSlide = latestSlide
                                , latestSlide = latestSlide
                                , navigationKey = navigationKey
                                , participants = participants
                                }

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


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout
            []
            (case model of
                Loading _ ->
                    Element.none

                Presenter presenter ->
                    List.getAt presenter.currentSlide slides |> Maybe.withDefault Element.none

                Viewer viewer ->
                    List.getAt viewer.currentSlide slides |> Maybe.withDefault Element.none
            )
        ]
    }


iframe src =
    Html.iframe
        [ Attr.src src
        , Attr.width 1000
        , Attr.height 1000
        ]
        []
