module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { count : Int
    , color : String
    , defaultColor : String
    }


model : Model
model =
    { count = 0
    , color = "black"
    , defaultColor = "black"
    }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset
    | SetColor String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

        Reset ->
            { model | count = 0, color = model.defaultColor }

        SetColor string ->
            { model | color = string }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [ style [ ( "color", model.color ) ] ] [ text (toString model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick (SetColor "red") ] [ text "Red" ]
        , button [ onClick (SetColor "blue") ] [ text "Blue" ]
        ]
