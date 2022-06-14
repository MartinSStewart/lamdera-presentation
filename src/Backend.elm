module Backend exposing (..)

import Env
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
    ( { participants = Set.empty, currentSlide = 0, presenter = Nothing }
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
        GetDataRequest maybePassword ->
            let
                participants =
                    participantCount model
            in
            if maybePassword == Just Env.password || model.presenter == Just sessionId then
                ( { model | presenter = Just sessionId }
                , { participants = participants, currentSlide = model.currentSlide }
                    |> PresenterData
                    |> GetDataResponse
                    |> Lamdera.sendToFrontend clientId
                )

            else
                ( model
                , { participants = participants, currentSlide = model.currentSlide }
                    |> ViewerData
                    |> GetDataResponse
                    |> Lamdera.sendToFrontend clientId
                )

        ChangeSlideRequest slide ->
            if Just sessionId == model.presenter then
                ( { model | currentSlide = slide }
                , Lamdera.broadcast (ChangeSlideNotification slide)
                )

            else
                ( model, Cmd.none )

        ResetPresentation ->
            ( { model | currentSlide = 0, participants = Set.empty }, Cmd.none )


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
