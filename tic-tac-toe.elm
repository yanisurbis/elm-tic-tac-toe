module RealApp exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import String exposing (length, toInt)
import Array exposing (Array, fromList, push, get, set, indexedMap, map, slice, append, foldr)
import List 

-- ALIASES



-- BOARD AND METHODS

-- path to check to understand if the gase is ended
pathsToCheck : List (List Int)
pathsToCheck =
    -- horizontal
    [ [0, 1, 2]
    , [3, 4, 5]
    , [6, 7, 8]
    -- vertical
    , [0, 3, 6]
    , [1, 4, 7]
    , [2, 5, 8]
    -- cross
    , [0, 4, 8]
    , [6, 4, 2]
    ]


type alias Board =
    Array.Array (Maybe Bool)

board : Board
board =
    fromList
        [ Just True, Just False, Nothing
        , Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing]

-- check : Board -> List List Int -> Cell -> Cell

-- isFinised : Board -> Bool

-- MODEL

type alias Model =
    { board : Board
    }

initModel : Model
initModel =
    { board = board
    }

-- UPDATE

type Msg
    = Empty

-- update : Msg -> Model -> Model

update msg model =
    case msg of
        Empty ->
            model

-- VIEW

cellToString : Bool -> String
cellToString boolValue =
    if boolValue == True then
        "X "
    else
        "0 "

displayRow : Board -> Html msg
displayRow board = 
    board
    |> indexedMap
        (\ index cell ->
            case cell of
                Just value ->
                    if (%) index 3 == 0 then
                        "\n" ++ (cellToString value)
                    else
                        cellToString value
                Nothing ->
                    "_ "
        )
    |> foldr
        (++)
        ""
    |> text

view : Model -> Html msg
view model = 
    pre []
        [ div []
            [ displayRow
                ( slice
                    0 3
                    board
                )
            ]
        , div []
            [ displayRow
                ( slice
                    3 6
                    board
                )
            ]
        , div []
            [ displayRow
                ( slice
                    6 9
                    board
                )
            ]          
        ]
            

main =
    Html.beginnerProgram { model = initModel, view = view, update = update }


