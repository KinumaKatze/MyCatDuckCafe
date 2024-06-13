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


-- Typ-Deklarationen

type alias Seat = 
    { name : String 
    , hidden : Bool
    , modal : Bool
    , id : Int
    }

type alias RandomValues =
    { randomIndex : Int
    , randomSeat : Int
    }

type alias Model =
    { width : Int
    , height : Int
    , hidden : Bool 
    , seat1 : Seat
    , seat2 : Seat
    , seat3 : Seat
    , seat4 : Seat
    , seat5 : Seat
    , nextSeat : Int
    , person_list : List String -- Liste an möglichen Gäste
    , seat_list : List String --Liste an Stühlen
    , randomString : Maybe String -- Next Guest
    , showModal : Bool
    }

--Initialisierung

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 
    10 
    10 
    True 
    {name = "Random_Person.png", hidden = True, modal = False, id = 0} 
    {name = "Random_Person.png", hidden = True, modal = False, id = 1} 
    {name = "Random_Person.png", hidden = True, modal = False, id = 2} 
    {name = "Random_Person.png", hidden = True, modal = False, id = 3} 
    {name = "Random_Person.png", hidden = True, modal = False, id = 4} 
    0 
    ["Person1.png","Person2.png","Person3.png","Person4.png"] 
    ["0","1","2","3","4"] 
    Nothing 
    False
    , Cmd.none )

-- Message Typen

type Msg
    = WindowResized (List Int)
    | AddNPC
    | PrepNextNPC
    | NPCClicked Seat
    | GotRandomValues RandomValues

--Hilfsfunktionen

extractName : Seat -> String 
extractName seat = 
        seat.name 

extractHidden : Seat -> Bool 
extractHidden seat = 
        seat.hidden 

extractModal : Seat -> Bool 
extractModal seat = 
        seat.modal 

removeWord : Maybe String -> List String -> List String --Entfernt Eintrag aus einer Liste
removeWord word liste = 
    case word of 
        Just a ->
            List.filter ((/=) a) liste
        Nothing ->
            liste

generateRandomValues : Int -> Int -> Generator RandomValues
generateRandomValues listPeople listSeats =
    Random.map2 RandomValues
        (Random.int 0 (listPeople - 1))
        (Random.int 0 (listSeats - 1))



--Update Funktionen

update : Msg -> Model -> ( Model, Cmd Msg ) --Es darf erst der nächste NPC gepreppt werden, wenn der erste abgearbeitet wurde
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
                listPeople = List.length model.person_list
                listSeats = List.length model.seat_list
                randomValuesGenerator = generateRandomValues listPeople listSeats
            in
            ( model, Random.generate GotRandomValues randomValuesGenerator)

        AddNPC -> -- Silhouhette ins Bild laden
            case model.nextSeat of 
                0 ->
                    ( { model | seat1 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat1.id} }, Cmd.none )
                1 ->
                    ( { model | seat2 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat2.id} }, Cmd.none )
                2 ->
                    ( { model | seat3 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat3.id} }, Cmd.none )
                3 ->
                    ( { model | seat4 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat4.id} }, Cmd.none )
                4 ->
                    ( { model | seat5 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat5.id} }, Cmd.none )
                _ -> 
                    ( { model | seat1 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat1.id} }, Cmd.none )


        NPCClicked seat ->   --Echte Person zeigen

                if seat.name == "Random_Person.png" then --Person wird zum enthülle angeklickt
                    case seat.id of 
                        0 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat1 = {name = a, hidden = False, modal = False, id = model.seat1.id} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat1 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat1.id} }, Cmd.none )
                        1 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat2 = {name = a, hidden = False, modal = False, id = model.seat2.id} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat2 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat2.id} }, Cmd.none )
                        2 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat3 = {name = a, hidden = False, modal = False, id = model.seat3.id} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat3 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat3.id} }, Cmd.none )
                        3 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat4 = {name = a, hidden = False, modal = False, id = model.seat4.id} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat4 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat4.id} }, Cmd.none )
                        4 ->
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat5 = {name = a, hidden = False, modal = False, id = model.seat5.id} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat5 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat5.id} }, Cmd.none )
                        _ -> 
                            case model.randomString of 
                                Just a -> 
                                    ( { model | seat1 = {name = a, hidden = False, modal = False, id = model.seat1.id} }, Cmd.none )
                                Nothing ->
                                    ( { model | seat1 = {name = "Random_Person.png", hidden = False, modal = False, id = model.seat1.id} }, Cmd.none )

                    else --Person wird zum sprechen angeklickt

                         case seat.id of 
                        0 ->
                                ( { model | seat1 = {name = model.seat1.name, hidden = model.seat1.hidden, modal = not model.seat1.modal, id = model.seat1.id}}, Cmd.none )
                        1 ->
                                ( { model | seat2 = {name = model.seat2.name, hidden = model.seat2.hidden, modal = not model.seat2.modal, id = model.seat2.id}}, Cmd.none )
                        2 ->
                                ( { model | seat3 = {name = model.seat3.name, hidden = model.seat3.hidden, modal = not model.seat3.modal, id = model.seat3.id}}, Cmd.none )
                                
                        3 ->
                                ( { model | seat4 = {name = model.seat4.name, hidden = model.seat4.hidden, modal = not model.seat4.modal, id = model.seat4.id}}, Cmd.none )
                                
                        4 ->
                                ( { model | seat5 = {name = model.seat5.name, hidden = model.seat5.hidden, modal = not model.seat5.modal, id = model.seat5.id}}, Cmd.none )
                                
                        _ -> 
                                ( { model | seat1 = {name = model.seat1.name, hidden = model.seat1.hidden, modal = not model.seat1.modal, id = model.seat1.id}}, Cmd.none )

        GotRandomValues randomValues ->
            let
                randomStr = List.Extra.getAt randomValues.randomIndex model.person_list
                randomSeat = List.Extra.getAt randomValues.randomSeat model.seat_list
            in
                case randomSeat of 
                    Just a -> 
                        case String.toInt a of 
                            Just b ->
                                ({ model | randomString = randomStr, nextSeat = b, person_list = removeWord randomStr model.person_list, seat_list = removeWord randomSeat model.seat_list}, Cmd.none )
                            Nothing ->
                                ({ model | randomString = randomStr, nextSeat = 1, person_list = removeWord randomStr model.person_list}, Cmd.none )
                    Nothing ->
                        ( { model | randomString = randomStr, nextSeat = 1, person_list = removeWord randomStr model.person_list}, Cmd.none )
           


-- SUBSCRIPTIONS

port windowSize : (List Int -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
    windowSize WindowResized


-- VIEW

view : Model -> Html Msg
view model =
    div [ style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        ]
        [ img
            [ src "Theke.gif"
            , style "height" <| String.fromInt model.height ++ "px" 
            , style "width" <| String.fromInt model.width ++ "px" 
            ]
            []
        , button --Seat1
            [ Html.Events.onClick (NPCClicked model.seat1)
            , if extractHidden model.seat1 == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "left" "1.4%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (extractName model.seat1)
                , if extractHidden model.seat1 == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , if model.seat1.modal then --Seat1 Talking Scene
                div [ style "position" "fixed"
                    , style "top" "0"
                    , style "left" "0"
                    , style "width" "100%"
                    , style "height" "100%"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "2"
                ]
                    [ div
                        [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                        , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                        , style "position" "absolute"
                        , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                        , style "left" "1.4%"  -- 0.5% vom rechten Rand
                        ]
                        [ 
                        button
                            [ Html.Events.onClick (NPCClicked model.seat1)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            , style "background" "none"
                            , style "border" "none"
                            , style "padding" "0"
                            , style "cursor" "pointer"
                            ] 
                            [ img 
                                [ src (extractName model.seat1)
                                , style "width" "100%"
                                , style "height" "100%"
                                ] 
                                []
                            ]
                        ]
                    ]
                  else
                    text ""
        , button --Seat2
            [ Html.Events.onClick (NPCClicked model.seat2)
            , if (extractHidden model.seat2) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "left" "21.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (extractName model.seat2)
                , if (extractHidden model.seat2) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
       , if model.seat2.modal then --Seat2 Talking Scene
                div [ style "position" "fixed"
                    , style "top" "0"
                    , style "left" "0"
                    , style "width" "100%"
                    , style "height" "100%"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "2"
                ]
                    [ div
                        [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                        , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                        , style "position" "absolute"
                        , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                        , style "left" "21.5%"  -- 0.5% vom rechten Rand
                        ]
                        [ 
                        button
                            [ Html.Events.onClick (NPCClicked model.seat2)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            , style "background" "none"
                            , style "border" "none"
                            , style "padding" "0"
                            , style "cursor" "pointer"
                            ] 
                            [ img 
                                [ src (extractName model.seat2)
                                , style "width" "100%"
                                , style "height" "100%"
                                ] 
                                []
                            ]
                        ]
                    ]
                  else
                    text ""
        , button --Seat3
            [ Html.Events.onClick (NPCClicked model.seat3)
            , if (extractHidden model.seat3) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "right" "41.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (extractName model.seat3)
                , if (extractHidden model.seat3) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , if model.seat3.modal then --Seat3 Talking Scene
                div [ style "position" "fixed"
                    , style "top" "0"
                    , style "left" "0"
                    , style "width" "100%"
                    , style "height" "100%"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "2"
                ]
                    [ div
                        [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                        , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                        , style "position" "absolute"
                        , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                        , style "right" "41.5%"  -- 0.5% vom rechten Rand
                        ]
                        [ 
                        button
                            [ Html.Events.onClick (NPCClicked model.seat3)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            , style "background" "none"
                            , style "border" "none"
                            , style "padding" "0"
                            , style "cursor" "pointer"
                            ] 
                            [ img 
                                [ src (extractName model.seat3)
                                , style "width" "100%"
                                , style "height" "100%"
                                ] 
                                []
                            ]
                        ]
                    ]
                  else
                    text ""
        , button --Seat4
            [ Html.Events.onClick (NPCClicked model.seat4)
            , if (extractHidden model.seat4) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "right" "21.5%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (extractName model.seat4)
                , if (extractHidden model.seat4) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
         , if model.seat4.modal then --Seat4 Talking Scene
                div [ style "position" "fixed"
                    , style "top" "0"
                    , style "left" "0"
                    , style "width" "100%"
                    , style "height" "100%"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "2"
                ]
                    [ div
                        [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                        , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                        , style "position" "absolute"
                        , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                        , style "right" "21.5%"  -- 0.5% vom rechten Rand
                        ]
                        [ 
                        button
                            [ Html.Events.onClick (NPCClicked model.seat4)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            , style "background" "none"
                            , style "border" "none"
                            , style "padding" "0"
                            , style "cursor" "pointer"
                            ] 
                            [ img 
                                [ src (extractName model.seat4)
                                , style "width" "100%"
                                , style "height" "100%"
                                ] 
                                []
                            ]
                        ]
                    ]
                  else
                    text ""
        , button --Seat5
            [ Html.Events.onClick (NPCClicked model.seat5)
            , if (extractHidden model.seat5) == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0.17 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0.3 scaling)
            , style "position" "absolute"
            , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
            , style "right" "1.4%"  -- 0.5% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            , style "zIndex" "1"
            ] 
            [ img 
                [ src (extractName model.seat5)
                , if (extractHidden model.seat5) == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , if model.seat5.modal then --Seat5 Talking Scene
                div [ style "position" "fixed"
                    , style "top" "0"
                    , style "left" "0"
                    , style "width" "100%"
                    , style "height" "100%"
                    , style "background-color" "rgba(0, 0, 0, 0.5)"  -- Halbtransparentes Overlay
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "zIndex" "2"
                ]
                    [ div
                        [ style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px"
                        , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px"
                        , style "position" "absolute"
                        , style "bottom" "13.8%"  -- 11.2% vom unteren Rand
                        , style "right" "1.4%"  -- 0.5% vom rechten Rand
                        ]
                        [ 
                        button
                            [ Html.Events.onClick (NPCClicked model.seat5)
                            , style "width" "100%"
                            , style "height" "100%"
                            , style "zIndex" "1"
                            , style "background" "none"
                            , style "border" "none"
                            , style "padding" "0"
                            , style "cursor" "pointer"
                            ] 
                            [ img 
                                [ src (extractName model.seat5)
                                , style "width" "100%"
                                , style "height" "100%"
                                ] 
                                []
                            ]
                        ]
                    ]
                  else
                    text ""
        , button 
            [ Html.Events.onClick PrepNextNPC
            , style "position" "absolute"
            , style "top" "50px"  -- Anpassung der vertikalen Position
            , style "left" "50px" ] [ text "PrepNPC" ] -- Anpassung der horizontalen Position
        , button 
            [ Html.Events.onClick AddNPC
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