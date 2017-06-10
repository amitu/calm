port module Calm.Server
    exposing
        ( Cookie
        , Method(..)
        , MimeType(..)
        , Model
        , Msg
        , MultiDict
        , Request
        , Response
        , ServerSpec
        , StatusCode(..)
        , getParam
        , html
        , htmlResponse
        , json
        , notFound
        , plain
        , program
        )

import Dict exposing (Dict)
import Formatting as F exposing ((<>), premap)
import Html exposing (Html)
import Http
import Json.Decode as JD
import Json.Decode.Extra as JD exposing ((|:))
import Json.Encode as JE
import Native.Helpers
import Navigation
import Time


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
                        JD.fail ("invalid method: " ++ invalid)
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


type Expiry
    = NotSpecified
    | MaxAge Int
    | Expires Time.Time


type alias Cookie =
    { name : String
    , value : String
    , domain : Maybe String
    , path : Maybe String
    , expiry : Expiry
    , secure : Bool
    , httpOnly : Bool
    }


mformat : (a -> String) -> F.Format r (Maybe a -> r)
mformat fn =
    premap
        (Maybe.map fn
            >> Maybe.withDefault ""
        )
        F.string


bformat : String -> F.Format r (Bool -> r)
bformat val =
    premap
        (\b ->
            if b then
                val
            else
                ""
        )
        F.string


formatCookie : Cookie -> String
formatCookie { name, value, domain, path, expiry, secure, httpOnly } =
    let
        format =
            F.uriFragment
                <> F.s "="
                <> F.uriFragment
                <> mformat (\d -> "; Domain=" ++ Http.encodeUri d)
                <> mformat (\d -> "; Path=" ++ d)
                <> bformat "; Secure"
                <> bformat "; HttpOnly"
    in
    F.print format name value domain path secure httpOnly


cookie : String -> String -> Cookie
cookie name value =
    Cookie name value Nothing Nothing NotSpecified False False


type alias Request =
    { id : String
    , location : Navigation.Location
    , method : Method
    , get : MultiDict
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


insert : Html msg -> JE.Value
insert =
    Native.Helpers.insert >> JE.int


type Response msg
    = Response
        { code : StatusCode
        , mime : MimeType
        , body : Html msg
        , cmd : Command
        , headers : MultiDict
        , cookies : Dict String Cookie
        }


respond : String -> Response msg -> Cmd (Msg msg)
respond id (Response { code, mime, body, cmd, headers, cookies }) =
    responses <|
        JE.object
            [ ( "id", JE.string id )
            , ( "code", code |> (\(StatusCode c) -> c) |> JE.int )
            , ( "mime", mime |> (\(MimeType m) -> m) |> JE.string )
            , ( "cmd", cmd |> (\(Command m) -> m) |> JE.string )
            , ( "body", insert body )
            , ( "headers"
              , headers
                    |> Dict.toList
                    |> List.map
                        (\( k, v ) -> ( k, JE.list (List.map JE.string v) ))
                    |> JE.object
              )
            , ( "cookies"
              , cookies
                    |> Dict.values
                    |> List.map (formatCookie >> JE.string)
                    |> JE.list
              )
            ]


type Command
    = Command String


serve : Command
serve =
    Command "serve"


serveFile : Command
serveFile =
    Command "serve-file"


serveDir : Command
serveDir =
    Command "serve-dir"


notFound : Html msg -> Response msg
notFound body =
    Response
        { code = StatusCode 404
        , mime = html
        , cmd = serve
        , body = body
        , headers = Dict.empty
        , cookies = Dict.empty
        }


htmlResponse : Html msg -> Response msg
htmlResponse body =
    Response
        { code = StatusCode 200
        , mime = html
        , cmd = serve
        , body = body
        , headers = Dict.empty
        , cookies = Dict.empty
        }



--type Response msg
--    = TextResponse String
--    | HTMLResponse (Html msg)
--    | JSONResponse JE.Value
--    | Response StatusCode MimeType (Html msg)
--    | ServeFile StatusCode MimeType FS.FileName
--    | ServeDir FS.DirName FS.RelativeFile
--      -- 201
--    | Created (Html msg)
--      -- MovedPermanently(301): This and all future requests should be
--      -- directed to the given URI.
--    | MovedPermanently Location
--      -- Found (302): This is an example of industry practice contradicting
--      -- the standard. The HTTP/1.0 specification (RFC 1945) required the
--      -- client tox perform a temporary redirect (the original describing
--      -- phrase was "Moved Temporarily"),[20] but popular browsers
--      -- implemented 302 with the functionality of a 303 See Other.
--      -- Therefore, HTTP/1.1 added status codes 303 and 307 to distinguish
--      -- between the two behaviours.[21] However, some Web applications and
--      -- frameworks use the 302 status code as if it were the 303
--    | Found Location
--      -- SeeOther(303): The response to the request can be found under
--      -- another URI using a GET method. When received in response to a
--      -- POST (or PUT/DELETE), the client should presume that the server
--      -- has received the data and should issue a redirect with a separate
--      -- GET message.
--    | SeeOther Location
--      -- NotModified(304): Indicates that the resource has not been
--      -- modified since the version specified by the request headers
--      -- If-Modified-Since or If-None-Match. In such case, there is no need
--      -- to retransmit the resource since the client still has a
--      -- previously-downloaded copy.
--    | NotModified
--      -- TemporaryRedirect(307): In this case, the request should be
--      -- repeated with another URI; however, future requests should still
--      -- use the original URI. In contrast to how 302 was historically
--      -- implemented, the request method is not allowed to be changed when
--      -- reissuing the original request. For example, a POST request should
--      -- be repeated using another POST request.
--    | TemporaryRedirect Location
--      -- PermanentRedirect(308): The request and all future requests should
--      -- be repeated using another URI. 307 and 308 parallel the behaviors
--      -- of 302 and 301, but do not allow the HTTP method to change. So,
--      -- for example, submitting a form to a permanently redirected
--      -- resource may continue smoothly.
--    | PermanentRedirect Location
--      -- 400, say json was malformed for API
--    | BadRequest (Html msg)
--      -- 401, say you are not logged in, and ask for a protected resource
--    | Unauthorized (Html msg)
--      -- 403 say you are logged in, but do not have access to this resource
--    | Forbidden (Html msg)
--      -- 404
--    | NotFound (Html msg)
--      -- 405
--    | MethodNotAllowed (List Method) (Html msg)
--      -- 500
--    | InternalServerError (Html msg)
--      -- 503, site is overloaded
--    | ServiceUnavailable (Html msg)
--
--
--
--
--respond__ : String -> Int -> Html msg -> Cmd (Msg msg)
--respond__ id code body =
--    respond_ id (StatusCode code) html (JE.string (htmlToString body))
--
--
--respond : String -> Response msg -> Cmd (Msg msg)
--respond id response =
--    case response of
--        HTMLResponse resp ->
--            respond_ id
--                (StatusCode 200)
--                html
--                (JE.string (htmlToString resp))
--
--        TextResponse resp ->
--            respond_ id (StatusCode 200) plain (JE.string resp)
--
--        JSONResponse resp ->
--            respond_ id (StatusCode 200) json resp
--
--        Response code mime body ->
--            respond_ id code mime (JE.string (htmlToString body))
--
--        NotFound resp ->
--            respond__ id 404 resp
--
--        ServeFile code mime file ->
--            Debug.crash "not implemented"
--
--        ServeDir dir file ->
--            Debug.crash "not implemented"
--
--        Created resp ->
--            respond__ id 201 resp
--
--        MovedPermanently location ->
--            Debug.crash "not implemented"
--
--        Found location ->
--            Debug.crash "not implemented"
--
--        SeeOther location ->
--            Debug.crash "not implemented"
--
--        NotModified ->
--            Debug.crash "not implemented"
--
--        TemporaryRedirect location ->
--            Debug.crash "not implemented"
--
--        PermanentRedirect location ->
--            Debug.crash "not implemented"
--
--        BadRequest body ->
--            respond__ id 400 body
--
--        Unauthorized body ->
--            respond__ id 401 body
--
--        Forbidden body ->
--            respond__ id 403 body
--
--        MethodNotAllowed methods body ->
--            Debug.crash "not implemented"
--
--        InternalServerError body ->
--            respond__ id 500 body
--
--        ServiceUnavailable body ->
--            respond__ id 503 body
-- model


type alias ServerSpec model msg =
    { init : Request -> Result (Response msg) ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , response : model -> Maybe (Response msg)
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


port requests : (JE.Value -> msg) -> Sub msg


port responses : JD.Value -> Cmd msg
