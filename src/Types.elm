module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation
import Keyboard
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Url exposing (Url)


type FrontendModel
    = Loading Browser.Navigation.Key (Maybe Size) (Maybe Data)
    | Presenter
        { currentSlide : Int
        , participants : Int
        , navigationKey : Browser.Navigation.Key
        , keys : List Keyboard.Key
        , windowSize : Size
        }
    | Viewer
        { latestSlide : Int
        , currentSlide : Int
        , participants : Int
        , navigationKey : Browser.Navigation.Key
        , windowSize : Size
        }


type alias Size =
    { width : Int, height : Int }


getKey : FrontendModel -> Browser.Navigation.Key
getKey model =
    case model of
        Loading key _ _ ->
            key

        Presenter { navigationKey } ->
            navigationKey

        Viewer { navigationKey } ->
            navigationKey


type alias BackendModel =
    { participants : Set ( SessionId, ClientId )
    , latestSlide : Int
    , presenter : Maybe SessionId
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | KeyboardMsg Keyboard.Msg
    | GotWindowSize Size
    | PressedGotoPreviousSlide
    | PressedGotoNextSlide


type ToBackend
    = GetDataRequest (Maybe String)
    | ChangeSlideRequest Int


type BackendMsg
    = UserConnected SessionId ClientId
    | UserDisconnected SessionId ClientId


type ToFrontend
    = ParticipantCountChanged Int
    | GetDataResponse Data
    | ChangeSlideNotification Int


type Data
    = ViewerData LoadingData
    | PresenterData LoadingData


type alias LoadingData =
    { latestSlide : Int, participants : Int }
