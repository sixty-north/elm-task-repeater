module Poll exposing (..)

import Http
import Json.Decode
import Platform.Cmd
import Task
import Time


type URL
    = String


type Msg result
    = Continue
    | RequestResponse result
    | RequestError Http.Error


type alias Model extmsg result =
    { url : URL
    , period : Time.Time
    , on_success : result -> extmsg
    , on_error : Http.Error -> extmsg
    , msg_wrapper : Msg result -> extmsg
    , result_decoder : Json.Decode.Decoder result
    , continue : result -> Bool
    }


send : msg -> Platform.Cmd.Cmd msg
send =
    Task.succeed >> (Task.perform identity identity)


update : Msg result -> Model extmsg result -> ( Model extmsg result, Platform.Cmd.Cmd extmsg )
update msg model =
    case msg of
        Continue ->
            -- TODO: wait, end next request to URL
            ( model, Platform.Cmd.none )

        RequestResponse result ->
            ( model
            , send (model.on_success result)
            )

        RequestError err ->
            ( model
            , send (model.on_error err)
            )


start : Model extmsg result -> Platform.Cmd.Cmd extmsg
start model =
    send (model.msg_wrapper Continue)
