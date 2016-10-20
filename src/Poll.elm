module Poll exposing (..)

import Cmd.Extra exposing (message)
import Platform.Cmd
import Task
import Task.Extra exposing (delay)
import Time


type Msg extmsg result
    = Poll
    | Multi (List extmsg)


type alias Model extmsg error result =
    { task : Task.Task error result
    , period : Time.Time
    , on_success : result -> extmsg
    , on_error : error -> extmsg
    , msg_wrapper : Msg extmsg result -> extmsg
    , continue : result -> Bool
    }


update : Msg extmsg result -> Model extmsg error result -> ( Model extmsg error result, Platform.Cmd.Cmd extmsg )
update msg model =
    case msg of
        Poll ->
            let
                task =
                    delay model.period model.task

                msgs result =
                    if (model.continue result) then
                        [ model.on_success result, model.msg_wrapper Poll ]
                    else
                        [ model.on_success result ]

                cmd =
                    Task.perform
                        model.on_error
                        (msgs >> Multi >> model.msg_wrapper)
                        task
            in
                ( model, cmd )

        Multi msgs ->
            model ! List.map message msgs


start : Model extmsg error result -> Platform.Cmd.Cmd extmsg
start model =
    message (model.msg_wrapper Poll)
