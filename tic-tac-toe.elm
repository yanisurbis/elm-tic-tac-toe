module RealApp exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import String exposing (length, toInt)
import Array exposing (Array, fromList, push, get, set, indexedMap, map, slice, append, foldr)
import List
import Mouse
import Random

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
    , list : List Bool
    }

initModel : Model
initModel =
    { board = board
    , list = []
    }

init : (Model, Cmd Msg)
init =
    (initModel, Cmd.none)

-- UPDATE

type Msg
    = Empty
    | GenerateNewBoard
    | MouseMsg Mouse.Position
    | CreateBoard (List Bool)

-- update : Msg -> Model -> Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Empty ->
            (model, Cmd.none)

        GenerateNewBoard ->
            (model, Random.generate CreateBoard (Random.list 18 Random.bool))

        CreateBoard list ->
            (Model (createBoard list) list, Cmd.none)

        MouseMsg position ->
            (model, Cmd.none)

createBoard : List Bool -> Board
createBoard list =
    let 
        justOrNothing = list
                        |> List.take 9
                        
        crossOrZero =  list
                        |> List.drop 9
                        |> List.take 9
            
    in
        fromList
            ( List.map2
                (\ just cross ->
                    if just == True && cross == True then
                        Just True
                    else if just == True && cross == False then
                        Just False
                    else
                        Nothing
                )
                justOrNothing
                crossOrZero
            )
        

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        ]


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

view : Model -> Html Msg
view model = 
    pre []
        [ div []
            [ displayRow
                ( slice
                    0 3
                    model.board
                )
            ]
        , div []
            [ displayRow
                ( slice
                    3 6
                    model.board
                )
            ]
        , div []
            [ displayRow
                ( slice
                    6 9
                    model.board
                )
            ]
        , div []
            [ text (model.list
                        |> List.map
                            (\ elm ->
                                if elm == True then
                                    "T"
                                else
                                    "F"
                            )
                        |> List.foldr
                            (++)
                            ""
                    )
            ]
        , button
            [ onClick GenerateNewBoard ]
            [ text "Generate Random Board" ]           
        ]            

main =
    Html.program { init = init
                    , view = view
                    , update = update
                    , subscriptions = subscriptions
                }


