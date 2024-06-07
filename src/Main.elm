port module Main exposing (main)

import Browser

import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List exposing (..)
import Random
import List.Extra exposing (getAt)




-- MODEL


type alias Model =
    { width : Int
    , height : Int
    , hidden : Bool
    , seat5 : String -- Person die in Seat5 Sitzen soll 
    , person_list : List String --Liste an möglichen Gästen
    , randomString : Maybe String --Next Guest
    }


init : () -> ( Model, Cmd Msg )
init _ =
    (Model 10 10 True "Random_Person.png" ["Person1.png","Person2.png"] Nothing, Cmd.none )


-- UPDATE


type Msg

    = WindowResized (List Int)
    | AddNPC
    | RemoveNPC
    | NPCClicked
    | GenerateRandomString 
    | GotRandomString Int

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
            let
                listLength = List.length model.person_list
                randomIndex = Random.int 0 (listLength - 1)
            in
            ( {model | hidden = False}, Random.generate GotRandomString randomIndex) --Um Random NPC zu erhalten 
        RemoveNPC ->
            ( {model | hidden = True}, Cmd.none )
        NPCClicked ->
            let
                listLength = List.length model.person_list
                randomIndex = Random.int 0 (listLength - 1)
            in

            case model.randomString of 
                Just a -> 
                    ( {model | seat5 = a}, Cmd.none )
                Nothing ->
                    ( {model | seat5 = "Random_Person.png"}, Cmd.none )

        GenerateRandomString ->
            let
                listLength = List.length model.person_list
                randomIndex = Random.int 0 (listLength - 1)
            in
            ( model, Random.generate GotRandomString randomIndex )

        GotRandomString index ->
            let
                randomStr = List.Extra.getAt index model.person_list
            in
            ( { model | randomString = randomStr }, Cmd.none )


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
        , button 
            [ Html.Events.onClick NPCClicked
            , if model.hidden == True then hidden True else hidden False
            , style "width" <| String.fromFloat (toFloat model.width * 0.17) ++ "px" -- Anpassung der Groeße (0,16 scaling)
            , style "height" <| String.fromFloat (toFloat model.height * 0.3) ++ "px" -- Anpassung der Groeße (0,3 scaling)
            , style "position" "absolute"
            , style "bottom" "11.2%"  -- 10% vom unteren Rand
            , style "right" "0.5%"  -- 10% vom rechten Rand
            , style "background" "none"
            , style "border" "none"
            , style "padding" "0"
            , style "cursor" "pointer"
            ] 
            [img 
                [ src model.seat5
                , if model.hidden == True then hidden True else hidden False
                , style "width" "100%"
                , style "height" "100%"
                ] 
                []
            ]
        , button 
            [ Html.Events.onClick AddNPC , if model.hidden == True then hidden False else hidden True
            , style "position" "absolute"
            , style "top" "50px"  -- Anpassung der vertikalen Position
            , style "left" "50px" ] [ text "Call for Bartender" ] -- Anpassung der horizontalen Position
        , button [ Html.Events.onClick RemoveNPC , if model.hidden == True then hidden True else hidden False] [ text "Tell him to leave" ]
        , img [ src "bartender.png", if model.hidden == True then hidden True else hidden False] []
        ]


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
