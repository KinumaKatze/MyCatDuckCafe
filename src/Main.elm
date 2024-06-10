port module Main exposing (main)

import Browser
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List exposing (..)
import Random exposing (..)
import List.Extra exposing (getAt)
import Tuple
import Basics exposing (..)


-- MODEL

type alias Model =
    { width : Int
    , height : Int
    , hidden : Bool 
    , seat1 : (String, Bool)
    , seat2 : (String, Bool)
    , seat3 : (String, Bool)
    , seat4 : (String, Bool)
    , seat5 : (String, Bool)
    , nextSeat : Int
    , person_list : List String -- Liste an möglichen Gästen
    , randomString : Maybe String -- Next Guest
    }

first : (a, b) -> a
first tuple =
    case tuple of
        (firstElement, _) ->
            firstElement

last : (a, b) -> b
last tuple =
    case tuple of
        (_, lastElement) ->
            lastElement

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 10 10 True ("Random_Person.png", True) ("Random_Person.png", True) ("Random_Person.png", True) ("Random_Person.png", True) ("Random_Person.png", True) 0 ["Person1.png","Person2.png"] Nothing
    , Cmd.none )


type alias RandomValues =
    { randomIndex : Int
    , randomSeat : Int
    }

generateRandomValues : Int -> Generator RandomValues
generateRandomValues listLength =
    Random.map2 RandomValues
        (Random.int 0 (listLength - 1))
        (Random.int 0 4)

-- UPDATE

type Msg
    = WindowResized (List Int)
    | AddNPC
    | PrepNextNPC
    | NPCClicked
    | GotRandomValues RandomValues

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

        PrepNextNPC -> --Random Sitzplatz und Random person ermitteln
            let
                listLength = List.length model.person_list
                randomValuesGenerator = generateRandomValues listLength
            in
            ( model, Random.generate GotRandomValues randomValuesGenerator )

        AddNPC -> -- Silhouhette ins Bild laden
            case model.nextSeat of 
                0 ->
                    ( { model | seat1 = ("Random_Person.png", False) }, Cmd.none )
                1 ->
                    ( { model | seat2 = ("Random_Person.png", False) }, Cmd.none )
                2 ->
                    ( { model | seat3 = ("Random_Person.png", False) }, Cmd.none )
                3 ->
                    ( { model | seat4 = ("Random_Person.png", False) }, Cmd.none )
                4 ->
                    ( { model | seat5 = ("Random_Person.png", False) }, Cmd.none )
                _ -> 
                    ( { model | seat1 = ("Random_Person.png", False) }, Cmd.none )


        NPCClicked ->   --Echte Person zeigen
                case model.nextSeat of 
                0 ->
                    case model.randomString of 
                        Just a -> 
                            ( { model | seat1 = (a, False) }, Cmd.none )
                        Nothing ->
                            ( { model | seat1 = ("Random_Person.png", True) }, Cmd.none )
                1 ->
                    case model.randomString of 
                        Just a -> 
                            ( { model | seat2 = (a, False) }, Cmd.none )
                        Nothing ->
                            ( { model | seat2 = ("Random_Person.png", True) }, Cmd.none )
                2 ->
                    case model.randomString of 
                        Just a -> 
                            ( { model | seat3 = (a, False) }, Cmd.none )
                        Nothing ->
                            ( { model | seat3 = ("Random_Person.png", True) }, Cmd.none )
                3 ->
                    case model.randomString of 
                        Just a -> 
                            ( { model | seat4 = (a, False) }, Cmd.none )
                        Nothing ->
                            ( { model | seat4 = ("Random_Person.png", True) }, Cmd.none )
                4 ->
                    case model.randomString of 
                        Just a -> 
                            ( { model | seat5 = (a, False) }, Cmd.none )
                        Nothing ->
                            ( { model | seat5 = ("Random_Person.png", True) }, Cmd.none )
                _ -> 
                    case model.randomString of 
                        Just a -> 
                            ( { model | seat1 = (a, False) }, Cmd.none )
                        Nothing ->
                            ( { model | seat1 = ("Random_Person.png", True) }, Cmd.none )
        GotRandomValues randomValues ->
            let
                randomStr = List.Extra.getAt randomValues.randomIndex model.person_list
            in
            ( { model | randomString = randomStr, nextSeat = randomValues.randomSeat}, Cmd.none )


-- SUBSCRIPTIONS

port windowSize : (List Int -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
    windowSize WindowResized


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ img
            [ src "Theke.gif"
            , style "height" <| String.fromInt model.height ++ "px" 
            , style "width" <| String.fromInt model.width ++ "px" 
            ]
            []
        , button --Seat1
            [ Html.Events.onClick NPCClicked
            , if last (model.seat1) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "11.2%"  -- 11.2% vom unteren Rand
            , style "left" "2%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            ] 
            [ img 
                [ src (first model.seat1)
                , if last (model.seat1) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , button --Seat2
            [ Html.Events.onClick NPCClicked
            , if last (model.seat2) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "11.2%"  -- 11.2% vom unteren Rand
            , style "left" "22.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            ] 
            [ img 
                [ src (first model.seat2)
                , if last (model.seat2) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , button --Seat3
            [ Html.Events.onClick NPCClicked
            , if last (model.seat3) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "11.2%"  -- 11.2% vom unteren Rand
            , style "right" "40.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            ] 
            [ img 
                [ src (first model.seat3)
                , if last (model.seat3) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , button --Seat4
            [ Html.Events.onClick NPCClicked
            , if last (model.seat4) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "11.2%"  -- 11.2% vom unteren Rand
            , style "right" "20.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            ] 
            [ img 
                [ src (first model.seat4)
                , if last (model.seat4) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , button --Seat5
            [ Html.Events.onClick NPCClicked
            , if last (model.seat5) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "11.2%"  -- 11.2% vom unteren Rand
            , style "right" "0.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            ] 
            [ img 
                [ src (first model.seat5)
                , if last (model.seat5) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , button 
            [ Html.Events.onClick PrepNextNPC
            , if model.hidden == True then hidden False else hidden True
            , style "position" "absolute"
            , style "top" "50px"  -- Anpassung der vertikalen Position
            , style "left" "50px" ] [ text "PrepNPC" ] -- Anpassung der horizontalen Position
        , button 
            [ Html.Events.onClick AddNPC
            , if model.hidden == True then hidden False else hidden True
            , style "position" "absolute"
            , style "top" "50px"  -- Anpassung der vertikalen Position
            , style "left" "400px" ] [ text "AddNPC" ] -- Anpassung der horizontalen Position
        ]


-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }