port module Server exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as JD
import Json.Decode.Extra as JD exposing ((|:))
import Json.Encode as JE


-- http utilities


type Method
    = GET
    | POST
    | DELETE
    | PUT
    | PATCH
    | HEAD


type MimeType
    = MimeType String


plain : MimeType
plain =
    MimeType "text/plain"


html : MimeType
html =
    MimeType "text/html"


json : MimeType
json =
    MimeType "application/json"


type StatusCode
    = StatusCode Int


type alias MultiDict =
    Dict String (List String)


type alias Cookie =
    { name : String
    , value : String
    }


type alias Request =
    { id : String
    , path : String
    , method : Method

    -- , get : MultiDict
    -- , post : MultiDict
    , headers : Dict String String
    , cookies : Dict String String
    }


request : JD.Decoder Request
request =
    JD.succeed Request
        |: JD.field "id" JD.string
        |: JD.field "path" JD.string
        |: JD.field "method"
            (JD.string
                |> JD.andThen
                    (\v ->
                        case v of
                            "GET" ->
                                JD.succeed GET

                            "POST" ->
                                JD.succeed POST

                            invalid ->
                                JD.fail ("invalid method" ++ invalid)
                    )
            )
        |: JD.field "headers" (JD.dict JD.string)
        |: JD.field "cookies" (JD.dict JD.string)


type Response
    = TextResponse String
    | HTMLResponse String
    | JSONResponse JE.Value
    | Response StatusCode MimeType String


respond_ : String -> StatusCode -> MimeType -> JE.Value -> Cmd (Msg msg)
respond_ id (StatusCode code) (MimeType mime) body =
    responses <|
        JE.object
            [ ( "id", JE.string id )
            , ( "code", JE.int code )
            , ( "body", body )
            , ( "mimetype", JE.string mime )
            ]


respond : String -> Response -> Cmd (Msg msg)
respond id response =
    case response of
        HTMLResponse resp ->
            respond_ id (StatusCode 200) html (JE.string resp)

        TextResponse resp ->
            respond_ id (StatusCode 200) plain (JE.string resp)

        JSONResponse resp ->
            respond_ id (StatusCode 200) json resp

        Response code mime body ->
            respond_ id code mime (JE.string body)



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
    = NewRequest JD.Value
    | RMsg String msg


new : Request -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
new request model =
    let
        ( m, cmd ) =
            model.spec.init request

        resp =
            model.spec.response m

        requests =
            model.requests
                |> Dict.insert request.id m
    in
    case resp of
        Nothing ->
            ( { model | requests = requests }
            , cmd |> Cmd.map (RMsg request.id)
            )

        Just resp ->
            ( model, respond request.id resp )


forward :
    String
    -> msg
    -> Model model msg
    -> ( Model model msg, Cmd (Msg msg) )
forward id msg model =
    case Dict.get id model.requests of
        Nothing ->
            Debug.log
                ("unknown id: " ++ toString id ++ " msg: " ++ toString msg)
                ( model, Cmd.none )

        Just m ->
            let
                ( m2, cmd ) =
                    model.spec.update msg m

                model2 =
                    { model | requests = Dict.insert id m2 model.requests }

                resp =
                    model.spec.response m2
            in
            case resp of
                Nothing ->
                    ( model2, cmd |> Cmd.map (RMsg id) )

                Just resp ->
                    ( { model | requests = Dict.remove id model.requests }
                    , respond id resp
                    )


update : Msg msg -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
update msg model =
    case Debug.log "msg" msg of
        NewRequest val ->
            case JD.decodeValue request val of
                Ok request ->
                    new request model

                Err msg ->
                    Debug.crash msg

        RMsg id msg ->
            forward id msg model


subscriptions : Model model msg -> Sub (Msg msg)
subscriptions model =
    model.requests
        |> Dict.toList
        |> List.map
            (\( id, req ) ->
                model.spec.subscriptions req |> Sub.map (RMsg id)
            )
        |> (::) (requests NewRequest)
        |> Sub.batch



-- boilerplate


program : ServerSpec model msg -> Program Never (Model model msg) (Msg msg)
program spec =
    Platform.program
        { init = init spec
        , update = update
        , subscriptions = subscriptions
        }


port requests : (JE.Value -> msg) -> Sub msg


port responses : JD.Value -> Cmd msg
