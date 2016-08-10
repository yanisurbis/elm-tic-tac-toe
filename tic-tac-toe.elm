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

-- start with empty board
board : Board
board =
    Array.fromList
        [ Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing
        ]

-- fake board as start parametr for several functions
fakeBoard : Board
fakeBoard =
    Array.fromList
        [ Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing
        , Nothing, Nothing, Nothing
        ]

-- test board 
testBoard : Board
testBoard =
    Array.fromList
        [ Just False, Nothing, Just True
        , Just True,  Nothing, Nothing
        , Just True, Just False, Just False 
        ]         

-- get elements from board by indexes
getElementsFromBoard : Board -> List Int -> List (Maybe Bool)
getElementsFromBoard board indexesOfElements =
    indexesOfElements
        -- get elements
        |> List.map
            (\elm -> 
                Array.get elm board
            )
        -- unwrap elements from maybe
        |> List.map
            (\ elm ->
                case elm of
                    Just value ->
                        value
                    
                    Nothing ->
                        Nothing
            )

-- checking if all of the elements of path are the same
-- use this function to detect the winner of the game
checkPathWinner : Board -> List Int -> Maybe Bool
checkPathWinner board pathToCheck =
    -- list of indexes
    let
        -- get elements from board that are complied with path
        elementsFromBoard : List (Maybe Bool)
        elementsFromBoard =
            getElementsFromBoard
                board
                pathToCheck
            

        hasNothing =
            elementsFromBoard
            |> List.any
                (\ elm ->
                    case elm of
                        Nothing ->
                            True
                        
                        Just value ->
                            False
                ) 

        allCrosses = 
            elementsFromBoard
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
            elementsFromBoard
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
            -- path is full
            Nothing


isXwinner : List (Maybe Bool) -> Bool
isXwinner pathCheckRezults =
    List.any
        (\ elm ->
            case elm of
                Just value ->
                    if value == True then
                        True
                    else
                        False
                Nothing ->
                    False                    
        )
        pathCheckRezults



isOwinner : List (Maybe Bool) -> Bool
isOwinner pathCheckRezults =
    List.any
        (\ elm ->
            case elm of
                Just value ->
                    if value == False then
                        True
                    else
                        False
                Nothing ->
                    False                    
        )
        pathCheckRezults


-- Just True if X is the winner
-- Just False if O is the winner
-- Nothing if we don't have the winner
checkBoardWinner : Board -> List (List Int) -> Maybe Bool
checkBoardWinner board pathsToCheck =
    let 
        
        pathsCheckRezults = 
            pathsToCheck
            -- check every path
            |> List.map 
                (checkPathWinner board)
    in
        if isXwinner pathsCheckRezults == True then
            Just True
        else if isOwinner pathsCheckRezults == True then
            Just False
        else 
            Nothing
       

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
        winner : Maybe Bool
        winner = checkBoardWinner
                        board
                        pathsToCheck
        
        xIsWinner =
                case winner of
                    Just True ->
                        True
                    Just False ->
                        False
                    Nothing ->
                        False
        
        oIsWinner =
                case winner of
                    Just True ->
                        False
                    Just False ->
                        True
                    Nothing ->
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

changeTurnTaker : Bool -> Bool
changeTurnTaker player =
    if player == True then
        False
    else
        True

isGameOver : GameStatus -> Bool
isGameOver {xIsWinner, oIsWinner, boardIsFull} =
    if boardIsFull == True 
        || xIsWinner == True 
        || oIsWinner == True 
    then
        True
    else 
        False

getPlayerScore : GameStatus -> Bool -> Int
getPlayerScore {xIsWinner, oIsWinner, boardIsFull} startTurnTaker =
    if xIsWinner == True && startTurnTaker == True then
        10
    else if oIsWinner == True && startTurnTaker == False then
        10 
    else if xIsWinner == True && startTurnTaker == False then
        -10
    else if oIsWinner == True && startTurnTaker == False then
        -10
    else
        0

miniMax : Board -> Bool -> Bool -> (Board, Int)
miniMax board startTurnTaker currentTurnTaker =
    let
        gameStatus = getGameStatus board
    in
        if (isGameOver gameStatus) == True then
            (board, getPlayerScore gameStatus startTurnTaker)
        else
            let 
                boardsWithAvailibleMoves = getAllAvailableMoves
                                                board 
                                                currentTurnTaker

                miniMaxResults = boardsWithAvailibleMoves
                                    |> List.map
                                        (\ boardWithNewMove ->
                                            -- boar
                                            ( boardWithNewMove
                                            -- snd because miniMax returns tuple
                                            , snd 
                                                ( miniMax
                                                    boardWithNewMove
                                                    startTurnTaker
                                                    (changeTurnTaker currentTurnTaker)
                                                )
                                            )  
                                        )
                minimax =
                    if not (startTurnTaker == currentTurnTaker) then
                        findMinFromBoards
                            miniMaxResults
                            (fakeBoard, 100)
                    else
                        findMaxFromBoards
                            miniMaxResults
                            (fakeBoard, -100)                                    
            in
                -- Debug.log "7" 
                minimax
   

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

findMinFromBoards : List (Board, Int) -> (Board, Int) -> (Board, Int)
findMinFromBoards boards startElement =
    let 

        maxElement
            = List.foldl
                (\ elm startValue ->
                    if snd elm < snd startValue then
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
    -- delete in the future
    , list : List Bool
    -- winner if we have: True = X, False = O, Nothing = we have no winner
    , winner : Maybe Bool
    -- True = X, False = O
    , currentMove : Bool
    }

initModel : Model
initModel =
    { board = board
    , list = []
    , winner = Nothing
    , currentMove = False
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
                            ( miniMax
                                model.board
                                model.currentMove
                                model.currentMove
                            )
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

getTextStatus : Board -> String
getTextStatus board =
    let
        {xIsWinner, oIsWinner, boardIsFull} = getGameStatus board

        winner = 
            if xIsWinner == True then
                "X IS WINNER"
            else if oIsWinner == True then
                "O IS WINNER"
            else "WINNER IS UNDEFINED"

        isFull =
            if boardIsFull == True then
                "BOARD IS FULL"
            else
                "BOARD IS NOT FULL"
    
    in  
        winner ++ " " ++ isFull

view : Model -> Html Msg
view model = 
    pre []
    (
        if Debug.log "test" model.currentMove == True then
            ((getAllAvailableMoves
                        model.board
                        True
                        )
                    |> List.map
                        displayBoard
                )
        else
            []
        |> List.append
            ([ div []
                [ 
                    text(getTextStatus model.board)                    
                ]
            ])      
        |> List.append
            ([ displayBoard model.board
            , if model.currentMove == False then
                button 
                    [ onClick AImove ]
                    [ text "AI move" ]
              else
                div [] []
            ])                  
    )
main =
    Html.program { init = init
                    , view = view
                    , update = update
                    , subscriptions = subscriptions
                }


