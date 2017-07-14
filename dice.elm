module Main exposing (..)

import Html exposing (Html, body, button, div, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Tuple
import Random
import Mouse
import Keyboard
import Maybe


-- MODEL


type DiceFace
    = NoFace
    | SetDiceFace Int


getDiceFaceVal : DiceFace -> Maybe Int
getDiceFaceVal face =
    case face of
        NoFace ->
            Nothing

        SetDiceFace val ->
            Just val


type DiceResult
    = Big
    | Small
    | NoResult


type alias Model =
    { count : Int
    , color : String
    , defaultColor : String
    , diceFaces : { left : DiceFace, right : DiceFace }
    , score : Int
    }


model : Model
model =
    { count = 0
    , color = "black"
    , defaultColor = "black"
    , diceFaces = { left = NoFace, right = NoFace }
    , score = 0
    }



-- MESSAGES


type Msg
    = RollDices
    | UpdateFaces ( Int, Int )
    | Guess DiceResult
    | MouseMsg Mouse.Position
    | KeyMsg Keyboard.KeyCode



-- UPDATE


resultOf : Model -> DiceResult
resultOf model =
    let
        valL =
            getDiceFaceVal model.diceFaces.left

        valR =
            getDiceFaceVal model.diceFaces.right

        sum =
            Maybe.map2 (+) valL valR
    in
        case Maybe.map (\x -> x > 7) sum of
            Just True ->
                Big

            Just False ->
                Small

            Nothing ->
                NoResult


randFaceIntGen =
    Random.int 1 6


randFacesIntGen =
    Random.pair randFaceIntGen randFaceIntGen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                RollDices ->
                    model

                UpdateFaces ( valL, valR ) ->
                    { model
                        | diceFaces =
                            { left = SetDiceFace valL
                            , right = SetDiceFace valR
                            }
                    }

                Guess resultGuess ->
                    case (resultGuess == resultOf model) of
                        True ->
                            { model | score = model.score + 1 }

                        False ->
                            { model | score = model.score - 1 }

                MouseMsg pos ->
                    model

                KeyMsg code ->
                    model

        command =
            case msg of
                RollDices ->
                    Random.generate UpdateFaces randFacesIntGen

                Guess _ ->
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
            [ viewDice model.diceFaces.left
            , viewDice model.diceFaces.right
            ]
        , button [ onClick (Guess Big) ] [ text "Big" ]
        , button [ onClick (Guess Small) ] [ text "Small" ]
        , model.score |> toString |> text
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

            NoFace ->
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
