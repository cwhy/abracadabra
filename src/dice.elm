module Main exposing (..)

import Html exposing (Html, body, button, div, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Tuple
import Random
import Mouse
import Keyboard
import Maybe
import Time exposing (Time)
import Process
import Task


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


type VisualRewardStatus
    = Positive
    | Negative
    | Neutral


type alias DiceFaces =
    { left : DiceFace
    , right : DiceFace
    }


type alias Model =
    { count : Int
    , color : String
    , defaultColor : String
    , diceFaces : DiceFaces
    , score : Int
    , visualReward : VisualRewardStatus
    , resultGuess : Maybe DiceResult
    , ifRequestScore : Bool
    }


model : Model
model =
    { count = 0
    , color = "black"
    , defaultColor = "black"
    , diceFaces = { left = NoFace, right = NoFace }
    , score = 0
    , visualReward = Neutral
    , resultGuess = Nothing
    , ifRequestScore = False
    }



-- MESSAGES


type Msg
    = RollDices
    | UpdateFaces ( Int, Int )
    | ResetFaces
    | Guess DiceResult
    | EvaluateResult ( Int, Int )
    | MouseMsg Mouse.Position
    | KeyMsg Keyboard.KeyCode
    | UpdateRequestScoreStatus Bool



-- UPDATE


resultOf : Model -> Maybe DiceResult
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
                Just Big

            Just False ->
                Just Small

            Nothing ->
                Nothing


randFaceIntGen =
    Random.int 1 6


randFacesIntGen =
    Random.pair randFaceIntGen randFaceIntGen


delay : Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                RollDices ->
                    model

                UpdateRequestScoreStatus status ->
                    { model | ifRequestScore = status }

                UpdateFaces ( valL, valR ) ->
                    { model
                        | diceFaces =
                            { left = SetDiceFace valL
                            , right = SetDiceFace valR
                            }
                    }

                ResetFaces ->
                    { model
                        | diceFaces =
                            { left = NoFace
                            , right = NoFace
                            }
                        , visualReward = Neutral
                    }

                Guess resultGuess ->
                    { model
                        | resultGuess = Just resultGuess
                        , visualReward = Neutral
                    }

                EvaluateResult ( valL, valR ) ->
                    let
                        newModel =
                            { model
                                | diceFaces =
                                    { left = SetDiceFace valL
                                    , right = SetDiceFace valR
                                    }
                            }

                        ( newScore, newVisualReward ) =
                            case (model.resultGuess == resultOf newModel) of
                                True ->
                                    ( model.score + 1
                                    , Positive
                                    )

                                False ->
                                    ( model.score - 1
                                    , Negative
                                    )
                    in
                        { newModel
                            | score = newScore
                            , visualReward = newVisualReward
                        }

                MouseMsg pos ->
                    model

                KeyMsg code ->
                    model

        command =
            case msg of
                RollDices ->
                    Random.generate UpdateFaces randFacesIntGen

                EvaluateResult _ ->
                    delay (Time.millisecond * 600) <| ResetFaces

                Guess _ ->
                    Random.generate EvaluateResult randFacesIntGen

                others ->
                    Cmd.none
    in
        ( newModel, command )



-- VIEW


rewardColor : VisualRewardStatus -> String
rewardColor visualReward =
    case visualReward of
        Positive ->
            "#BCED91"

        Negative ->
            "#B20000"

        Neutral ->
            "#EEEEFF"


view : Model -> Html Msg
view model =
    body
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            , ( "height", "100vh" )
            , ( "background-color", rewardColor model.visualReward )
            ]
        ]
        [ viewDices model.diceFaces <| rewardColor model.visualReward
        , case model.visualReward of
            Positive ->
                div [] []

            Negative ->
                div [] []

            Neutral ->
                viewControlGroups
                    [ viewGuessButton Big
                    , viewGuessButton Small
                    , viewScore model
                    ]
        ]


viewControlGroups : List (Html Msg) -> Html Msg
viewControlGroups buttons =
    div
        [ style
            [ ( "justify-content", "space-around" )
            , ( "height", "10vh" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            ]
        ]
        buttons


viewScore : Model -> Html Msg
viewScore model =
    div
        [ onMouseEnter (UpdateRequestScoreStatus True)
        , onMouseLeave (UpdateRequestScoreStatus False)
        , style
            [ ( "font-size", "5vh" )
            , ( "color", "gold" )
            , ( "width", "5em" )
            , ( "font-weight", "bold" )
            , ( "padding", "0.5em" )
            , ( "user-select", "none" )
            ]
        ]
        [ case model.ifRequestScore of
            True ->
                model.score
                    |> toString
                    |> text

            False ->
                "My Score" |> text
        ]


viewGuessButton : DiceResult -> Html Msg
viewGuessButton resultGuess =
    div
        [ onClick (Guess resultGuess)
        , style
            [ ( "font-size", "5vh" )
            , ( "cursor", "pointer" )
            , ( "color", "#4885ed" )
            , ( "font-weight", "bold" )
            , ( "text-decoration", "underline" )
            , ( "padding", "0.5em" )
            , ( "user-select", "none" )
            ]
        ]
        [ text <|
            case resultGuess of
                Big ->
                    "Big"

                Small ->
                    "Small"
        ]


viewDices : DiceFaces -> String -> Html Msg
viewDices faces bgcolor =
    div
        [ style
            [ ( "display", "flex" )
            , ( "justify-content", "space-around" )
            , ( "width", "100%" )
            , ( "height", "30vh" )
            , ( "margin-top", "10vh" )
            , ( "margin-bottom", "10vh" )
            ]
        ]
        [ viewDice faces.left bgcolor
        , viewDice faces.right bgcolor
        ]


viewDice : DiceFace -> String -> Html Msg
viewDice face bgcolor =
    div
        [ style
            [ ( "border", "solid" )
            , ( "width", "20vh" )
            , ( "height", "20vh" )
            , ( "font-size", "15vh" )
            , ( "line-height", "20vh" )
            , ( "text-align", "center" )
            , ( "border-radius", "4vh" )
            , ( "background-color", bgcolor )
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
