-- module RealApp exposing (..)

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
import Debug

-- ALIASES



-- BOARD AND METHODS


type alias Board =
    Array.Array (Maybe Bool)

board : Board
board =
    Array.fromList
        [ Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing
        ]

fakeBoard : Board
fakeBoard =
    Array.fromList
        [ Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing
        ] 

-- get elements from board 
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


-- all elements should be non empty
boardIsFull : Board -> Bool
boardIsFull board =
    board
        |> Array.toList
        |> List.all
            (\ elm ->
                case elm of
                    Just value ->
                        True
                
                    Nothing ->
                        False
            )

type alias GameStatus =
    { boardIsFull : Bool
    , xIsWinner : Bool
    , oIsWinner : Bool
    }

getGameStatus : Board -> GameStatus
getGameStatus board =
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

        xIsWinner : Bool
        xIsWinner = checkBoard
                        board
                        pathsToCheck
                        True
        
        oIsWinner : Bool
        oIsWinner = checkBoard
                        board
                        pathsToCheck
                        False

        -- is there any availible moves?
        isFull = boardIsFull
                    board
    in
        { boardIsFull = isFull
        , xIsWinner = xIsWinner
        , oIsWinner = oIsWinner
        }


getAllAvailableMoves : Board -> Bool -> List Board
getAllAvailableMoves board currentPlayer =
    board
    |> Array.toList
    |> List.indexedMap
        (\ index elm ->
            case elm of
                Just value ->
                    Nothing
                Nothing ->
                    Just (changeBoard board index currentPlayer)
        )
    |> List.filterMap
        (\ elm ->
            case elm of
                Just value ->
                    Just value
                
                Nothing ->
                    Nothing
        )


changeBoard : Board -> Int -> Bool -> Board
changeBoard board index currentPlayer =
    case currentPlayer of
        True ->
            Array.set 
                index
                (Just True)
                board
        
        False ->
            Array.set
                index
                (Just False)
                board

-- AI

changePlayer : Bool -> Bool
changePlayer player =
    if player == True then
        False
    else
        True


miniMax : Board -> Bool -> (Board, Int)
miniMax board xShouldWin =
    let
        {xIsWinner, oIsWinner, boardIsFull} = getGameStatus board
    in
        -- error
        if xIsWinner == True 
            && oIsWinner == True then
            -1

        -- the end, smbd is the winner
        else if xIsWinner == True 
                && xShouldWin == True then
            10
        else if xIsWinner == True 
                && xShouldWin == False then
            -10
        else if oIsWinner == True 
                && xShouldWin == True then
            -10
        else if oIsWinner == True 
                && xShouldWin == False then
            10

        -- the end, nobody is the winner
        else if boardIsFull == True 
                && oIsWinner == False 
                && xIsWinner == False then
            0

        -- NOT the end, run minimax
        else if boardIsFull == False
                && oIsWinner == False
                && xIsWinner == False then
            let 
                boardsWithAvailibleMoves = getAllAvailableMoves
                                                board 
                                                (changePlayer xShouldWin)

                miniMaxResults = boardsWithAvailibleMoves
                                    |> List.map
                                        (\ boardWithNewMove ->
                                            -- boar
                                            ( boardWithNewMove
                                            -- snd because miniMax returns tuple
                                            , snd 
                                                miniMax
                                                    boardWithNewMove
                                                    (changePlayer xShouldWin)
                                            )  
                                        )

                maximum = findMaxFromBoards
                            miniMaxResults
                            (board, -1)                                    
            in
                maximum
        
        -- error
        else 
            -1

   

findMaxFromBoards : List (Board, Int) -> (Board, Int) -> (Board, Int)
findMaxFromBoards boards startElement =
    let 

        maxElement
            = List.foldl
                (\ elm startValue ->
                    if snd elm > snd startValue then
                        elm
                    else
                        startValue
                )
                startElement
                boards

    in
        maxElement



-- MODEL

type alias Model =
    { board : Board
    , list : List Bool
    , winner : Maybe Bool
    , currentMove : Bool
    }

initModel : Model
initModel =
    { board = board
    , list = []
    , winner = Nothing
    , currentMove = True
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
    | Choose Board
    | AImove

-- update : Msg -> Model -> Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Empty ->
            (model, Cmd.none)

        GenerateNewBoard ->
            (model, Random.generate CreateBoard (Random.list 18 Random.bool))

        CreateBoard list ->
            (Model (createBoard list) list model.winner model.currentMove, Cmd.none)

        MouseMsg position ->
            (model, Cmd.none)

        Choose board ->
            ({model | board = board
                    , currentMove = False
                    , list = model.list
                    , winner = model.winner
                }
                , Cmd.none
            )

        AImove ->
            ({model | board
                        = fst
                            miniMax
                                model.board
                                model.currentMove
                    , currentMove = True
                    , list = model.list
                    , winner = model.winner
            }, Cmd.none)
            -- (model, Cmd.none)
        

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

numberOfElementInRow = 3

displayBoard : Board -> Html Msg
displayBoard board =
    let
        numberOfRows = Array.length board // numberOfElementInRow

        rowsOfBoard =
            (List.repeat numberOfRows 0)
            |> List.indexedMap
                (\ index _ -> 
                    div []
                        [ displayRow
                            ( Array.slice
                                (index * numberOfRows)
                                ((index + 1) * numberOfRows)
                                board
                            )
                        ]
                )
    in
        div []
            [   div []
                    [ text "++++++++++++++++++++++++++++++++++++++=="]
                , div []
                    rowsOfBoard
                , button 
                    [ onClick (Choose board) ]
                    [ text "choose this board" ]
            ]

view : Model -> Html Msg
view model = 
    pre []
    (
        
        ((showAllAvalibleMoves 
                    model.board
                    True
                    )
                |> List.map
                    displayBoard
            )
        |> List.append
            [ h1 []
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
        |> List.append
        ([ displayBoard model.board
        , button 
            [ onClick AImove ]
            [ text "AI move" ]
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
        ])
                  
    )
main =
    Html.program { init = init
                    , view = view
                    , update = update
                    , subscriptions = subscriptions
                }


