module Client exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)
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
    = BlogList (Maybe Page)
    | PostPage PostId
    | CategoryPage Category


route : Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map (Maybe.map Page >> BlogList) (top <?> intParam "page")
        , Url.map (PostId >> PostPage) (s "post" </> string)
        , Url.map (Category >> CategoryPage) (s "category" </> string)
        ]


type Msg
    = NoOp


http404 : Html Msg
http404 =
    Html.h1 [class "foo-class"] [ Html.text "page not found" ]


main : Html msg
main =
    Html.text "hello world"
