module Backend exposing (..)

import Env
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Set
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Lamdera.onConnect UserConnected
                    , Lamdera.onDisconnect UserDisconnected
                    ]
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { participants = Set.empty, latestSlide = 0, presenter = Nothing }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        UserConnected sessionId clientId ->
            let
                newModel =
                    { model | participants = Set.insert ( sessionId, clientId ) model.participants }
            in
            ( newModel, handleParticipantsChanged newModel model )

        UserDisconnected sessionId clientId ->
            let
                newModel =
                    { model | participants = Set.remove ( sessionId, clientId ) model.participants }
            in
            ( newModel, handleParticipantsChanged newModel model )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        GetViewerDataRequest ->
            let
                participants =
                    participantCount model
            in
            ( model
            , Lamdera.sendToFrontend
                clientId
                (GetViewerDataResponse { latestSlide = model.latestSlide, participants = participants })
            )

        GetPresenterDataRequest password ->
            if password == Env.password then
                let
                    participants =
                        participantCount model
                in
                ( { model | presenter = Just sessionId }
                , Lamdera.sendToFrontend clientId (GetPresenterDataResponse (Ok { participants = participants }))
                )

            else
                ( model, Lamdera.sendToFrontend clientId (GetPresenterDataResponse (Err ())) )


participantCount : BackendModel -> Int
participantCount model =
    Set.toList model.participants |> List.gatherEqualsBy Tuple.first |> List.length


handleParticipantsChanged : BackendModel -> BackendModel -> Cmd BackendMsg
handleParticipantsChanged newModel oldModel =
    let
        count =
            participantCount newModel
    in
    if count /= participantCount oldModel then
        Lamdera.broadcast (ParticipantCountChanged count)

    else
        Cmd.none
