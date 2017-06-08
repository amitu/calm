port module Calm.Server
    exposing
        ( Cookie
        , Method(..)
        , MimeType(..)
        , MultiDict
        , Request
        , Response(..)
        , ServerSpec
        , StatusCode(..)
        , getParam
        , html
        , json
        , plain
        , program
        )

import Calm.FS as FS
import Dict exposing (Dict)
import Html exposing (Html)
import HtmlToString exposing (htmlToString)
import Json.Decode as JD
import Json.Decode.Extra as JD exposing ((|:))
import Json.Encode as JE
import Navigation
import UrlParser as Url


-- http utilities


type Method
    = GET
    | POST
    | DELETE
    | PUT
    | PATCH
    | HEAD


method : JD.Decoder Method
method =
    JD.string
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


multidict : JD.Decoder MultiDict
multidict =
    JD.dict (JD.list JD.string)


getParam : String -> MultiDict -> Maybe String
getParam key mdict =
    Nothing


type alias Cookie =
    { name : String
    , value : String
    }


type alias Request =
    { id : String
    , location : Navigation.Location
    , method : Method
    , get : MultiDict

    -- , post : MultiDict
    , headers : Dict String String
    , cookies : Dict String String
    }


location : JD.Decoder Navigation.Location
location =
    JD.succeed Navigation.Location
        |: JD.field "href" JD.string
        |: JD.field "host" JD.string
        |: JD.field "hostname" JD.string
        |: JD.field "protocol" JD.string
        |: JD.field "origin" JD.string
        |: JD.field "port" JD.string
        |: JD.field "pathname" JD.string
        |: JD.field "search" JD.string
        |: JD.field "hash" JD.string
        |: JD.field "username" JD.string
        |: JD.field "password" JD.string


request : JD.Decoder Request
request =
    JD.succeed Request
        |: JD.field "id" JD.string
        |: JD.field "location" location
        |: JD.field "method" method
        |: JD.field "get" multidict
        |: JD.field "headers" (JD.dict JD.string)
        |: JD.field "cookies" (JD.dict JD.string)


type Location
    = RelativeLocation String
    | AbsoluteLocation String


type Response msg
    = TextResponse String
    | HTMLResponse (Html msg)
    | JSONResponse JE.Value
    | Response StatusCode MimeType (Html msg)
    | ServeFile StatusCode MimeType FS.FileName
    | ServerDir FS.DirName FS.RelativeFile
      -- 201
    | Created (Html Msg)
      -- MovedPermanently(301): This and all future requests should be
      -- directed to the given URI.
    | MovedPermanently Location
      -- Found (302): This is an example of industry practice contradicting
      -- the standard. The HTTP/1.0 specification (RFC 1945) required the
      -- client to perform a temporary redirect (the original describing
      -- phrase was "Moved Temporarily"),[20] but popular browsers
      -- implemented 302 with the functionality of a 303 See Other.
      -- Therefore, HTTP/1.1 added status codes 303 and 307 to distinguish
      -- between the two behaviours.[21] However, some Web applications and
      -- frameworks use the 302 status code as if it were the 303
    | Found Location
      -- SeeOther(303): The response to the request can be found under
      -- another URI using a GET method. When received in response to a
      -- POST (or PUT/DELETE), the client should presume that the server
      -- has received the data and should issue a redirect with a separate
      -- GET message.
    | SeeOther Location
      -- NotModified(304): Indicates that the resource has not been
      -- modified since the version specified by the request headers
      -- If-Modified-Since or If-None-Match. In such case, there is no need
      -- to retransmit the resource since the client still has a
      -- previously-downloaded copy.
    | NotModified
      -- TemporaryRedirect(307): In this case, the request should be
      -- repeated with another URI; however, future requests should still
      -- use the original URI. In contrast to how 302 was historically
      -- implemented, the request method is not allowed to be changed when
      -- reissuing the original request. For example, a POST request should
      -- be repeated using another POST request.
    | TemporaryRedirect Location
      -- PermanentRedirect(308): The request and all future requests should
      -- be repeated using another URI. 307 and 308 parallel the behaviors
      -- of 302 and 301, but do not allow the HTTP method to change. So,
      -- for example, submitting a form to a permanently redirected
      -- resource may continue smoothly.
    | PermanentRedirect Location
      -- 400, say json was malformed for API
    | BadRequest (Html msg)
      -- 401, say you are not logged in, and ask for a protected resource
    | Unauthorized (Html msg)
      -- 403 say you are logged in, but do not have access to this resource
    | Forbidden (Html msg)
      -- 404
    | NotFound (Html msg)
      -- 405
    | MethodNotAllowed (List Method) (Html msg)
      -- 500
    | InternalServerError (Html msg)
      -- 503, site is overloaded
    | ServiceUnavailable (Html msg)


respond_ : String -> StatusCode -> MimeType -> JE.Value -> Cmd (Msg msg)
respond_ id (StatusCode code) (MimeType mime) body =
    responses <|
        JE.object
            [ ( "id", JE.string id )
            , ( "code", JE.int code )
            , ( "body", body )
            , ( "mimetype", JE.string mime )
            ]


respond : String -> Response msg -> Cmd (Msg msg)
respond id response =
    case response of
        HTMLResponse resp ->
            respond_ id
                (StatusCode 200)
                html
                (JE.string (htmlToString resp))

        TextResponse resp ->
            respond_ id (StatusCode 200) plain (JE.string resp)

        JSONResponse resp ->
            respond_ id (StatusCode 200) json resp

        Response code mime body ->
            respond_ id code mime (JE.string (htmlToString body))

        NotFound resp ->
            respond_ id
                (StatusCode 404)
                html
                (JE.string (htmlToString resp))

        ServeFile code mime file ->
            Debug.crash "not implemented"

        ServerDir dir file ->
            Debug.crash "not implemented"

        Created resp ->
            Debug.crash "not implemented"

        MovedPermanently location ->
            Debug.crash "not implemented"

        Found location ->
            Debug.crash "not implemented"

        SeeOther location ->
            Debug.crash "not implemented"

        NotModified ->
            Debug.crash "not implemented"

        TemporaryRedirect location ->
            Debug.crash "not implemented"

        PermanentRedirect location ->
            Debug.crash "not implemented"

        BadRequest body ->
            Debug.crash "not implemented"

        Unauthorized body ->
            Debug.crash "not implemented"

        Forbidden body ->
            Debug.crash "not implemented"

        MethodNotAllowed methods body ->
            Debug.crash "not implemented"

        InternalServerError body ->
            Debug.crash "not implemented"

        ServiceUnavailable body ->
            Debug.crash "not implemented"



-- model


type alias ServerSpec model msg =
    { init : Request -> Result (Response msg) ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , response : model -> Maybe (Response msg)
    , subscriptions : model -> Sub msg
    }


type alias RoutedSpec route model msg =
    { route : Url.Parser (route -> a) a
    , init : route -> Request -> Result (Response msg) ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , response : model -> Maybe (Response msg)
    , subscriptions : model -> Sub msg
    }


type alias Model model msg =
    { requests : Dict String model
    , spec : ServerSpec model msg
    }


init : spec -> ( Model model route msg, Cmd (Msg msg) )
init spec =
    ( { requests = Dict.empty, spec = spec }, Cmd.none )



-- update


type Msg msg
    = NewRequest JD.Value
    | RMsg String msg


new : Request -> Model model msg -> ( Model model msg, Cmd (Msg msg) )
new request model =
    case model.spec.init request of
        Ok ( m, cmd ) ->
            let
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

        Err resp ->
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


programWithRoute : RoutedSpec model route msg -> Program Never (Model model msg) (Msg msg)
programWithRoute spec =
    Platform.program
        { init = init spec
        , update = update
        , subscriptions = subscriptions
        }


port requests : (JE.Value -> msg) -> Sub msg


port responses : JD.Value -> Cmd msg
