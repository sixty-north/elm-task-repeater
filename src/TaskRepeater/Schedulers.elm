module TaskRepeater.Schedulers exposing (exponentialBackoff, uniform)

import TaskRepeater exposing (Scheduler)
import Time

{- A scheduler that puts a uniform time between each performance of the task. -}


uniform : Time.Time -> Scheduler Time.Time
uniform period =
    { model = period
    , next = \last -> ( last, last )
    }


type alias ExponentialParams =
    { period : Time.Time
    , factor : Float
    , max : Time.Time
    }


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
