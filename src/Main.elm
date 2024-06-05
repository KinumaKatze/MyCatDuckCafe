module Main exposing (main)

import Browser
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode



-- MODEL


type alias Model =
    { width : Int
    , height : Int
    , hidden : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 10 10 True, Cmd.none )



-- UPDATE


type Msg
    = WindowResized Int Int
    | AddNPC


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized width height ->
            ( { model | width = width, height = height }, Cmd.none )

        AddNPC ->
            ( {model | hidden = False}, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize WindowResized



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ img [ src "Tavern.jpeg", style "width" (String.fromInt model.width), style "height" (String.fromInt model.height) ] []
        , button [ Html.Events.onClick AddNPC ] [ text "Call for Bartender" ]
        , img [ src "bartender.png", if model.hidden == True then hidden True else hidden False] []
        ]


-- FUNCTIONS



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
