module Poll exposing (..)

import Cmd.Extra exposing (message)
import Http
import Json.Decode
import Platform.Cmd
import Task
import Task.Extra exposing (delay)
import Time


type alias URL =
    String


type Msg extmsg result
    = Poll
    | Multi (List extmsg)


type alias Model extmsg result =
    { url : URL
    , period : Time.Time
    , on_response : result -> extmsg
    , on_error : Http.Error -> extmsg
    , msg_wrapper : Msg extmsg result -> extmsg
    , result_decoder : Json.Decode.Decoder result
    , continue : result -> Bool
    }


update : Msg extmsg result -> Model extmsg result -> ( Model extmsg result, Platform.Cmd.Cmd extmsg )
update msg model =
    case msg of
        Poll ->
            let
                get =
                    Http.get model.result_decoder model.url

                task =
                    delay model.period get

                msgs result =
                    if (model.continue result) then
                        [ model.on_response result, model.msg_wrapper Poll ]
                    else
                        [ model.on_response result ]

                cmd =
                    Task.perform
                        model.on_error
                        (msgs >> Multi >> model.msg_wrapper)
                        task
            in
                ( model, cmd )

        Multi msgs ->
            model ! List.map message msgs


start : Model extmsg result -> Platform.Cmd.Cmd extmsg
start model =
    message (model.msg_wrapper Poll)
