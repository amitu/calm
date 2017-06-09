module Acko exposing (..)

import Calm.Server as Server
import Client
import Html exposing (div, h1, text)
import Task
import Time
import UrlParser as Url


-- models


type alias Model =
    { request : Server.Request
    , now : Maybe Time.Time
    , route : Client.Route
    }


init : Server.Request -> Result Server.Response ( Model, Cmd Msg )
init request =
    let
        route =
            Url.parsePath Client.route request.location
    in
    case route of
        Just r ->
            Ok
                ( { request = request
                  , now = Nothing
                  , route = r
                  }
                , Time.now |> Task.perform GotTime
                )

        Nothing ->
            Err
                (Client.http404
                    |> Html.map ClientMsg
                    |> Server.notFound
                )



-- update


type Msg
    = GotTime Time.Time
    | ClientMsg Client.Msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotTime time ->
            ( { model | now = Just time }, Cmd.none )

        ClientMsg imsg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- response


response : Model -> Maybe Server.Response
response model =
    model.now
        |> Maybe.map
            (\_ ->
                Server.htmlResponse <|
                    div []
                        [ h1 [] [ text "hello world" ]
                        , text (toString model)
                        ]
            )


main : Program Never (Server.Model Model Msg) (Server.Msg Msg)
main =
    Server.program
        { init = init
        , update = update
        , response = response
        , subscriptions = subscriptions
        }
