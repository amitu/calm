module Client exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, href, id, style)
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
    Html.h1
        [ id "someid"
        , href "#foo"
        , class "foo-class"
        , style [ ( "color", "red" ) ]
        ]
        [ Html.text "page not found "
        , Html.small [] [ Html.text "small" ]
        ]


main : Html msg
main =
    Html.text "hello world"
