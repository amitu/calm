module Acko exposing (..)

import Client
import Html exposing (h1, text)
import Server
import Task
import Time


-- models


type alias Model =
    { request : Server.Request
    , now : Maybe Time.Time
    }


init : Server.Request -> ( Model, Cmd Msg )
init request =
    ( { request = request, now = Nothing }
    , Time.now |> Task.perform GotTime
    )



-- update


type Msg
    = GotTime Time.Time


update : Msg -> Model -> ( Model, Cmd msg )
update (GotTime time) model =
    ( { model | now = Just time }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- response


response : Model -> Maybe (Server.Response Msg)
response model =
    model.now
        |> Maybe.map
            (\x -> Server.HTMLResponse <| h1 [] [ text "hello world" ])


main =
    Server.program
        { init = init
        , update = update
        , response = response
        , subscriptions = subscriptions
        }
