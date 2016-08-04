module RealApp exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import String
import Array exposing (Array)
import List
import Mouse
import Random

-- ALIASES



-- BOARD AND METHODS

type alias Board =
    Array.Array (Maybe Bool)

board : Board
board =
    Array.fromList
        [ Just True, Just False, Nothing
        , Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing
        ]

getElementsFromBoard : Board -> List Int -> List (Maybe (Maybe Bool))
getElementsFromBoard board pathToCheck =
    let
        elements: List (Maybe (Maybe Bool))
        elements =
            pathToCheck
                |> List.map
                    (\elm -> 
                        Array.get elm board
                    )
    in
        elements

-- check one path from pathsToCheck
checkPath : Board -> List Int -> Maybe Bool
checkPath board pathToCheck =
    -- list of indexes
    let
        -- get elements from board that are complied with path
        elementsFromBoard : List (Maybe (Maybe Bool))
        elementsFromBoard =
            getElementsFromBoard
                board
                pathToCheck
        
        -- we don't want maybe around maybe 
        unwrappedElementsFromBoard : List (Maybe Bool)
        unwrappedElementsFromBoard =
            elementsFromBoard
            |> List.map
                (\ elm ->
                    case elm of
                        Just value ->
                            value
                        
                        Nothing ->
                            Nothing
                )

        hasNothing =
            unwrappedElementsFromBoard
            |> List.any
                (\ elm ->
                    case elm of
                        Nothing ->
                            True
                        
                        Just value ->
                            False
                ) 

        allCrosses = 
            unwrappedElementsFromBoard
            |> List.all
                (\elm ->
                    case elm of
                        Just value ->
                            if value == True then
                                True
                            else
                                False
                        
                        Nothing ->
                            False
                )

        allZeros = 
            unwrappedElementsFromBoard
            |> List.all
                (\elm ->
                    case elm of
                        Just value ->
                            if value == True then
                                False
                            else
                                True
                        
                        Nothing ->
                            False
                )
    in
        if hasNothing == True then
            Nothing
        else if allCrosses == True then
            Just True
        else if allZeros == True then
            Just False
        else
            Nothing










checkBoard : Board -> List (List Int) -> Bool -> Bool
checkBoard board pathsToCheck winner =
    let 
        rezultOfCheck : List (Maybe Bool)
        rezultOfCheck = 
            pathsToCheck
            -- check every path
            |> List.map 
                (checkPath board)
    in
        rezultOfCheck
        |> List.any
            (\ elm ->
                case elm of
                    Just value ->
                        if value == winner then
                            True
                        else
                            False
                    Nothing ->
                        False                    
            )
       

-- is game finished?
isFinished : Board -> (Bool, Bool)
isFinished board =
    let 
        -- path to check to understand if the game is ended
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
        
        -- to understand if cross is winner current board we neeed
        --                                      board
        --                                      paths to check
        --                                      corresponding values for cross = True

        crossIsWinner : Bool
        crossIsWinner = checkBoard
                            board
                            pathsToCheck
                            True
        
        zeroIsWinner : Bool
        zeroIsWinner = checkBoard
                            board
                            pathsToCheck
                            False
    in
        (crossIsWinner, zeroIsWinner)

-- MODEL

type alias Model =
    { board : Board
    , list : List Bool
    , winner : Maybe Bool
    }

initModel : Model
initModel =
    { board = board
    , list = []
    , winner = Nothing
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
            (Model (createBoard list) list model.winner, Cmd.none)

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
        Array.fromList
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
    |> Array.indexedMap
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
    |> Array.foldr
        (++)
        ""
    |> text

view : Model -> Html Msg
view model = 
    pre []
        [ div []
            [ displayRow
                ( Array.slice
                    0 3
                    model.board
                )
            ]
        , div []
            [ displayRow
                ( Array.slice
                    3 6
                    model.board
                )
            ]
        , div []
            [ displayRow
                ( Array.slice
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
        , h1 []
            [ case isFinished model.board of
                (True, True) ->
                    text ("Cross and Zeros")
                (True, False) ->
                    text ("Cross only")
                (False, True) ->
                    text ("Zeros only")
                (False, False) ->
                    text ("Nobody")
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


