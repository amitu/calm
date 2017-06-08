module Client exposing (..)

import Html exposing (Html)
import UrlParser as Url
    exposing
        ( (</>)
        , (<?>)
        , Parser
        , intParam
        , parseHash
        , s
        , string
        , top
        )


type Page
    = Page Int


type PostId
    = PostId String


type Category
    = Category String


type Route
    = BlogList Page
    | PostPage PostId
    | CategoryPage Category


route : Parser (Route -> a) a
route =
    Url.oneOf
        [ -- Url.map (\_ -> Just (BlogList <| Page 1)) top
          -- , Url.map (Page >> BlogList) (s "top" <?> intParam "page")
          Url.map (PostId >> PostPage) (s "post" </> string)
        , Url.map (Category >> CategoryPage) (s "category" </> string)
        ]


type Msg
    = NoOp


http404 : Html Msg
http404 =
    Html.h1 [] [ Html.text "page not found" ]


main =
    Html.text "hello world"
