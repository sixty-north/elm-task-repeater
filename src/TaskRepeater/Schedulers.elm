module TaskRepeater.Schedulers exposing (exponentialBackoff, uniform)

{-| A collection of schedulers for task repeaters.

# Schedulers
@docs uniform, exponentialBackoff
-}

import TaskRepeater exposing (scheduler)
import Time


{-| Create scheduler that puts a uniform time between each performance of the
task.

To create a schedule that runs a task every 10 seconds:

  uniform (Time.seconds * 10)
-}
uniform : Time.Time -> Scheduler Time.Time
uniform period =
    scheduler period (\last -> ( last, last ))


type alias ExponentialParams =
    { period : Time.Time
    , factor : Float
    , max : Time.Time
    }


{-| Create a scheduler that puts exponentially increasing time between each
execution of the task.

To create a scheduler that starts with a 2 second wait, doubles the wait time
for each subsequent execution, and maxes out at 60 seconds:

    exponentialBackoff (Time.second 2) 2 (Time.second 60)
-}
exponentialBackoff : Time.Time -> Float -> Time.Time -> Scheduler ExponentialParams
exponentialBackoff period factor maxPeriod =
    let
        params =
            ExponentialParams period factor maxPeriod
    in
        { model = params
        , next =
            \p ->
                ( { p | period = max (p.period * p.factor) p.max }
                , p.period
                )
        }
