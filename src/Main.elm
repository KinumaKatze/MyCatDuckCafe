module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Html exposing (Html, div, text, img)
import Html.Attributes exposing (checked, coords, for, href, id, name, shape, src, style, title, type_, usemap)
import Json.Decode as Decode


-- MODEL

type alias Model =
    { width : Int
    , height : Int
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 0, Cmd.none )

-- UPDATE

type Msg
    = WindowResized Int Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized width height ->
            ( { model | width = width, height = height }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize WindowResized

-- VIEW

view : Model -> Html Msg
view model =
    div[][img [src  "Tavern.jpeg", style "width" (String.fromInt model.width), style "height" (String.fromInt model.height)] []]

-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }