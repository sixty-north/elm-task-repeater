module Tests exposing (..)

-- import Expect

import Platform.Cmd
import Task
import TaskRepeater as TR
import Test exposing (..)
import Time


type Msg
    = Success Int
    | Error Int
    | Repeater (TR.Msg Msg)


type alias Model =
    { repeater : TR.Model Msg Int Int
    , success : Int
    , error : Int
    }


model : Model
model =
    { repeater =
        { task = Task.succeed 42
        , period = Time.second
        , on_success = Success
        , on_error = Error
        , msg_wrapper = Repeater
        , continue = \_ -> False
        }
    , success = 0
    , error = 0
    }


update : Msg -> Model -> ( Model, Platform.Cmd.Cmd Msg )
update msg model =
    case msg of
        Success val ->
            { model | success = val } ! []

        Error val ->
            { model | error = val } ! []

        Repeater trmsg ->
            let
                ( trm, cmd ) =
                    TR.update trmsg model.repeater
            in
                { model | repeater = trm } ! [ cmd ]


all : Test
all =
    describe "A Test Suite"
        [-- test "Addition" <|
         --     \() ->
         --         Expect.equal (3 + 7) 10
         -- , test "String.left" <|
         --     \() ->
         --         Expect.equal "a" (String.left 1 "abcdefg")
         -- , test "This test should fail" <|
         --     \() ->
         --         Expect.fail "failed as expected!"
        ]
