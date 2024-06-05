port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)


-- MODEL

type alias Model =
    { width : Int
    , height : Int
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 10 10, Cmd.none )

-- UPDATE

type Msg
    = WindowResized (List Int)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResized liste ->
                case (head liste, head (reverse liste)) of 
                    (Just a, Just b) ->
                        ( { model | width = a, height = b }, Cmd.none )
                    (Just a, Nothing) -> 
                        ( { model | width = a, height = 0 }, Cmd.none )
                    (Nothing, Just b) ->
                        ( { model | width = 0, height = b }, Cmd.none )
                    (Nothing, Nothing) ->    
                        ( { model | width = 0, height = 0 }, Cmd.none )

-- SUBSCRIPTIONS

port windowSize : (List Int -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
    windowSize WindowResized

-- VIEW

view : Model -> Html Msg
view model =
     div [ style "width" "100vw", style "height" "100vh", style "overflow" "hidden" ]
        [ img
            [ src "Tavern.jpeg"
            , style "width" <| String.fromInt model.width ++ "px"
            , style "height" <| String.fromInt model.height ++ "px"
            , style "object-fit" "cover"
            ]
            []
        ]
        

-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }