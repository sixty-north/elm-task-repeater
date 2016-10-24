module TaskRepeater exposing (model, Model, Msg, scheduler, start, update)

{-| Provides a repeatedly running a Task and communicating the results (success
or error) to other parts of a program.

You provide a few pieces of information:
 - the task to run
 - a scheduler for determining when to run the task
 - messages for communicating success and error
 - a message for wrapping internal messages that should come back to the task
   repeater framework
 - a function determining when to continue running the task

Based on these TaskRepeater will repeated execute the task, sending back the
results via the messages you configure it with.

# Factories
@docs model, scheduler

# Execution
@docs start, update

TODO: Example
-}

import Cmd.Extra exposing (message)
import Platform.Cmd
import Task
import Task.Extra exposing (delay)
import Time


{-| Messages used for internal operations.
-}
type Msg extmsg
    = Poll
    | Multi (List extmsg)


type alias Scheduler m =
    { model : m
    , next : m -> ( m, Time.Time )
    }


{-| Create a schedule for task execution.

`model` is whatever data the scheduler needs to do its job.

`next` determines how long to wait before the next task execution. Given the
current model, it produces the next model and the delay time.
-}
scheduler : m -> (m -> ( m, Time.Time )) -> Scheduler m
scheduler =
    Scheduler


type alias ModelRecord error result s extmsg =
    { task : Task.Task error result
    , scheduler : Scheduler s
    , onSuccess : result -> extmsg
    , onError : error -> extmsg
    , msgWrapper : Msg extmsg -> extmsg
    , continue : result -> Bool
    }


type Model error result s extmsg
    = Model (ModelRecord error result s extmsg)


{-| Create a new model.

`task` is the task to be periodically executed.
`scheduler` determines when the task will be executed.
`onResult` creates the message that will be issued when the task succeeds.
`onError` creates the message that will be issued when the task fails.
`msgWrapper` creates a message that wraps a Msg so that users can route messages to `update`.
`continue` determines if execution should continue when a result is received.
-}
model : Task.Task e r -> Scheduler s -> (r -> m) -> (e -> m) -> (Msg m -> m) -> (r -> Bool) -> Model e r s m
model task scheduler onResult onError msgWrapper continue =
    Model (ModelRecord task scheduler onResult onError msgWrapper continue)


{-| Process a Msg to update a Model.

A standard update function.
-}
update : Msg m -> Model e r s m -> ( Model e r s m, Platform.Cmd.Cmd m )
update msg model =
    case model of
        Model rec ->
            case msg of
                Poll ->
                    let
                        ( schedulerModel, period ) =
                            rec.scheduler.next rec.scheduler.model

                        scheduler =
                            Scheduler schedulerModel rec.scheduler.next

                        task =
                            delay period rec.task

                        msgs result =
                            if (rec.continue result) then
                                [ rec.onSuccess result, rec.msgWrapper Poll ]
                            else
                                [ rec.onSuccess result ]

                        cmd =
                            Task.perform
                                rec.onError
                                (msgs >> Multi >> rec.msgWrapper)
                                task
                    in
                        Model { rec | scheduler = scheduler } ! [ cmd ]

                Multi msgs ->
                    model ! List.map message msgs


{-| Start executing a task on a schedule.

This will return a Cmd that should be routed normally by the caller.
-}
start : Model e r s m -> Platform.Cmd.Cmd m
start model =
    case model of
        Model rec ->
            message (rec.msgWrapper Poll)
