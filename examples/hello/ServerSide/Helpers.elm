module ServerSide.Helpers exposing (..)

import Dict exposing (Dict)
import Html
import Json.Decode
import Native.ServerSideHelpers
import Regex
import Result
import ServerSide.Constants exposing (..)
import String


nativeAddThings : Html.Attribute msg -> Html.Html msg -> Html.Html msg
nativeAddThings =
    Native.ServerSideHelpers.addAttribute


addAttribute : Html.Attribute msg -> Html.Html msg -> Html.Html msg
addAttribute attribute htmlThing =
    nativeAddThings attribute htmlThing


triggerEvent : String -> Json.Decode.Value -> Html.Html msg -> Result String msg
triggerEvent =
    Native.ServerSideHelpers.triggerEvent


stringify : a -> String
stringify =
    Native.ServerSideHelpers.stringify


filterKnownKeys : Dict String a -> Dict String a
filterKnownKeys =
    Dict.filter (\key _ -> not (List.member key knownKeys))



-- UNUSED
-- Hacky text approach


wrapWithQuotesAndColons : String -> String -> String
wrapWithQuotesAndColons left right =
    "\"" ++ String.trim left ++ "\"" ++ ":" ++ right


replaceInternalStructure : String -> String
replaceInternalStructure =
    Regex.replace Regex.All (Regex.regex "<internal structure>") (\_ -> "null")


replaceChildren : String -> String
replaceChildren =
    Native.ServerSideHelpers.replaceChildren


wrapFieldsWithQuotes : String -> String
wrapFieldsWithQuotes =
    let
        rejoiner parts =
            case parts of
                left :: right :: _ ->
                    if String.startsWith "{" left then
                        "{" ++ wrapWithQuotesAndColons (String.dropLeft 1 left) right
                    else
                        wrapWithQuotesAndColons left right

                _ ->
                    String.join ":" parts
    in
    String.split ","
        >> List.map (String.split ":" >> Debug.log "parts" >> rejoiner)
        >> String.join ","
