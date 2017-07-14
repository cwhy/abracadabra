module Main exposing (..)

import Html exposing (Html, body, button, div, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Tuple
import Random
import Mouse
import Keyboard


-- MODEL


type DiceFace
    = Unset
    | SetDiceFace Int


type alias Model =
    { count : Int
    , color : String
    , defaultColor : String
    , diceFaces : { left : DiceFace, right : DiceFace }
    }


model : Model
model =
    { count = 0
    , color = "black"
    , defaultColor = "black"
    , diceFaces = { left = Unset, right = Unset }
    }



-- MESSAGES


type Msg
    = Increment
    | Decrement
    | Reset
    | SetColor String
    | RollDices
    | UpdateFaces ( Int, Int )
    | MouseMsg Mouse.Position
    | KeyMsg Keyboard.KeyCode



-- UPDATE


randFaceIntGen =
    Random.int 1 6


randFacesIntGen =
    Random.pair randFaceIntGen randFaceIntGen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Increment ->
                    { model | count = model.count + 1 }

                Decrement ->
                    { model | count = model.count - 1 }

                Reset ->
                    { model | count = 0, color = model.defaultColor }

                SetColor string ->
                    { model | color = string }

                RollDices ->
                    model

                UpdateFaces ( valL, valR ) ->
                    { model
                        | diceFaces =
                            { left = SetDiceFace valL
                            , right = SetDiceFace valR
                            }
                    }

                MouseMsg pos ->
                    model

                KeyMsg code ->
                    model

        command =
            case msg of
                RollDices ->
                    Random.generate UpdateFaces randFacesIntGen

                others ->
                    Cmd.none
    in
        ( newModel, command )



-- VIEW


view : Model -> Html Msg
view model =
    body []
        [ div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [ style [ ( "color", model.color ) ] ]
                [ model.count |> toString |> text ]
            , button [ onClick Increment ] [ text "+" ]
            , button [ onClick Reset ] [ text "Reset" ]
            , button [ onClick (SetColor "red") ] [ text "Red" ]
            , button [ onClick (SetColor "blue") ] [ text "Blue" ]
            ]
        , div []
            [ viewDice model.diceFaces.left
            , viewDice model.diceFaces.right
            ]
        , button [ onClick RollDices ] [ text "Roll" ]
        ]


viewDice : DiceFace -> Html Msg
viewDice face =
    div
        [ style
            [ ( "border", "solid" )
            , ( "width", "1em" )
            ]
        , class "dice"
        ]
        [ case face of
            SetDiceFace faceInt ->
                faceInt |> toString |> text

            Unset ->
                "?" |> text
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        , Keyboard.downs KeyMsg
        ]



-- INIT


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- MAIN


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
