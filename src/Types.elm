module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Url exposing (Url)


type FrontendModel
    = Loading Key
    | Presenter
        { currentSlide : Int
        , participants : Int
        }
    | Viewer
        { latestSlide : Int
        , currentSlide : Int
        , participants : Int
        }


type alias BackendModel =
    { participants : Set ( SessionId, ClientId )
    , latestSlide : Int
    , presenter : Maybe SessionId
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = GetViewerDataRequest
    | GetPresenterDataRequest String


type BackendMsg
    = UserConnected SessionId ClientId
    | UserDisconnected SessionId ClientId


type ToFrontend
    = ParticipantCountChanged Int
    | GetViewerDataResponse { latestSlide : Int, participants : Int }
    | GetPresenterDataResponse (Result () { participants : Int })
