module Server exposing (..)

--import HtmlToString

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Encode as JE


-- http utilities


type Method
    = GET
    | POST
    | DELETE
    | PUT
    | PATCH
    | HEAD


type StatusCode
    = StatusCode Int


type alias MultiDict =
    Dict String (List String)


type alias Cookie =
    { name : String
    , value : String
    }


type alias Request =
    { path : String
    , method : Method
    , get : MultiDict
    , post : MultiDict
    , headers : MultiDict
    , cookies : Dict String Cookie
    }


type Response
    = HTMLResponse String
    | JSONResponse JE.Value
    | Response StatusCode String



-- model


type alias ServerSpec model msg =
    { init : Request -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , response : model -> Maybe Response
    , subscriptions : model -> Sub msg
    }


type alias Model model msg =
    { requests : Dict String model
    , spec : ServerSpec model msg
    }


init : ServerSpec model msg -> ( Model model msg, Cmd (Msg msg) )
init spec =
    ( { requests = Dict.empty, spec = spec }, Cmd.none )



-- update


type Msg msg
    = NoOp
    | RMsg String msg


update : Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
update msg model =
    ( model, Cmd.none )


subscriptions : Model model msg -> Sub (Msg msg)
subscriptions model =
    model.requests
        |> Dict.toList
        |> List.map
            (\( id, req ) ->
                model.spec.subscriptions req |> Sub.map (RMsg id)
            )
        |> Sub.batch



-- boilerplate


program : ServerSpec model msg -> Program Never (Model model msg) (Msg msg)
program spec =
    Platform.program
        { init = init spec
        , update = update
        , subscriptions = subscriptions
        }
