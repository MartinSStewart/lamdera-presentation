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
        , key : Key
        }
    | Viewer
        { latestSlide : Int
        , currentSlide : Int
        , participants : Int
        , key : Key
        }


getKey : FrontendModel -> Key
getKey model =
    case model of
        Loading key ->
            key

        Presenter { key } ->
            key

        Viewer { key } ->
            key


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
    = GetDataRequest (Maybe String)


type BackendMsg
    = UserConnected SessionId ClientId
    | UserDisconnected SessionId ClientId


type ToFrontend
    = ParticipantCountChanged Int
    | GetDataResponse Data


type Data
    = ViewerData { latestSlide : Int, participants : Int }
    | PresenterData { latestSlide : Int, participants : Int }
