port module Main exposing (main)

import Browser

import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List exposing (..)




-- MODEL


type alias Model =
    { width : Int
    , height : Int
    , hidden : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 10 10, Cmd.none )


-- UPDATE


type Msg

    = WindowResized (List Int)
    | AddNPC
    | RemoveNPC

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

        AddNPC ->
            ( {model | hidden = False}, Cmd.none )
        RemoveNPC ->
            ( {model | hidden = True}, Cmd.none )



-- SUBSCRIPTIONS


port windowSize : (List Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    windowSize WindowResized



-- VIEW


view : Model -> Html Msg
view model =

    div [style "width" "100vw", style "height" "100vh", style "overflow" "hidden"]
        [ img [ src "Tavern.jpeg", style "width" (String.fromInt model.width), style "height" (String.fromInt model.height) ] []
        , button [ Html.Events.onClick AddNPC , if model.hidden == True then hidden False else hidden True] [ text "Call for Bartender" ]
        , button [ Html.Events.onClick RemoveNPC , if model.hidden == True then hidden True else hidden False] [ text "Tell him to leave" ]
        , img [ src "bartender.png", if model.hidden == True then hidden True else hidden False] []
        , img
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
