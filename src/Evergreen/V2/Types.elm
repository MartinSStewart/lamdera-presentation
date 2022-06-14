module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Keyboard
import Lamdera
import Set
import Url


type alias Size =
    { width : Int
    , height : Int
    }


type alias LoadingData =
    { currentSlide : Int
    , participants : Int
    }


type Data
    = ViewerData LoadingData
    | PresenterData LoadingData


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


type alias BackendModel =
    { participants : Set.Set ( Lamdera.SessionId, Lamdera.ClientId )
    , currentSlide : Int
    , presenter : Maybe Lamdera.SessionId
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyboardMsg Keyboard.Msg
    | GotWindowSize Size


type ToBackend
    = GetDataRequest (Maybe String)
    | ChangeSlideRequest Int
    | ResetPresentation


type BackendMsg
    = UserConnected Lamdera.SessionId Lamdera.ClientId
    | UserDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = ParticipantCountChanged Int
    | GetDataResponse Data
    | ChangeSlideNotification Int
