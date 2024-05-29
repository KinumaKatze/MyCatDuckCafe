module Main exposing (main)

import Browser
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , fieldset
        , img
        , input
        , label
        , node
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (checked, coords, for, href, id, name, shape, src, style, title, type_, usemap)
import Html.Events exposing (onClick, onInput)
import List exposing (append, length, map, map2, range)

type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg  
view model =
        div[][img [src  "Tavern.jpeg", style "width" "1000px", style "height" "1000px"] []]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
