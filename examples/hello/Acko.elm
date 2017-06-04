module Acko exposing (..)

import Client
import Server


-- models


type alias Model =
    { request : Server.Request }


init : Server.Request -> ( Model, Cmd Msg )
init request =
    ( { request = request }, Cmd.none )



-- update


type Msg
    = NoOp


update : Msg -> model -> ( model, Cmd msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- response


response : Model -> Maybe Server.Response
response model =
    Just <| Server.HTMLResponse "<h1>hello world</h1>"


main =
    Server.program
        { init = init
        , update = update
        , response = response
        , subscriptions = subscriptions
        }
