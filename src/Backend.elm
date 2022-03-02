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
        GetDataRequest maybePassword ->
            let
                participants =
                    participantCount model
            in
            if maybePassword == Just Env.password || model.presenter == Just sessionId then
                ( { model | presenter = Just sessionId }
                , { participants = participants, latestSlide = model.latestSlide }
                    |> PresenterData
                    |> GetDataResponse
                    |> Lamdera.sendToFrontend clientId
                )

            else
                ( model
                , { participants = participants, latestSlide = model.latestSlide }
                    |> ViewerData
                    |> GetDataResponse
                    |> Lamdera.sendToFrontend clientId
                )

        ChangeSlideRequest slide ->
            if Just sessionId == model.presenter then
                let
                    newLatestSlide =
                        max model.latestSlide slide
                in
                ( { model | latestSlide = newLatestSlide }
                , Set.toList model.participants
                    |> List.filterMap
                        (\( _, clientId_ ) ->
                            if clientId_ == clientId then
                                Nothing

                            else
                                Lamdera.sendToFrontend clientId (ChangeSlideNotification slide) |> Just
                        )
                    |> Cmd.batch
                )

            else
                ( model, Cmd.none )


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
