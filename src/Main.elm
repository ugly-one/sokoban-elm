import Browser
import Html exposing (Html, button, div, text, table, tr, td)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = Left | Right | Up | Down

type alias Coord = { x : Int, y : Int  }

type alias Model = {
  mWalls : List Coord,
  mCrates : List Coord,
  mStorages : List Coord,
  mWorker : Coord,
  mMax : Coord,
  mSteps : Int
  } 

update : Msg -> Model -> Model
update msg model = 
  if isValid msg model then 
    modifyWorldNoValidation model msg
  else model    

-- can be called only with valid input --
modifyWorldNoValidation : Model -> Msg -> Model
modifyWorldNoValidation model input = 
  let
    newWorkerPos = add model.mWorker input
    worldWithMovedWorker = { model | mWorker = newWorkerPos, mSteps = model.mSteps + 1 }
  in if isCrate model newWorkerPos then 
      { worldWithMovedWorker | mCrates = [(add newWorkerPos input)] ++ (remove newWorkerPos model.mCrates)}
    else
      worldWithMovedWorker

remove : Coord -> List Coord -> List Coord
remove toRemove list = 
  List.foldl (\x acc -> if x == toRemove then acc else acc ++ [x]) [] list

isValid : Msg -> Model -> Bool
isValid msg model = 
  let 
    newWorkerPos = add (model.mWorker) msg
    newCratePos = add newWorkerPos msg
  in
    if isWall model newWorkerPos then
      False

    else if isCrate model newWorkerPos then
      not (isWall model newCratePos) && not (isCrate model newCratePos)

    else if isFinished model then
      False
    else 
      True

isWorker : Model -> Coord -> Bool
isWorker model coord = coord == model.mWorker

isStorage : Model -> Coord -> Bool
isStorage model coord = List.member coord model.mStorages

isWall : Model -> Coord -> Bool
isWall model coord = List.member coord model.mWalls

isCrate : Model -> Coord -> Bool
isCrate model coord = List.member coord model.mCrates

isFinished : Model -> Bool
isFinished model = (List.sortWith compareCoords model.mCrates) == (List.sortWith compareCoords model.mStorages)

compareCoords : Coord -> Coord -> Order
compareCoords c1 c2 =
  if c1.x == c2.x && c1.y == c2.y then EQ
  else if c1.x > c2.x then GT
  else if c1.x == c2.x && c1.y > c2.y then GT
  else LT

add : Coord -> Msg -> Coord
add coord input =
    case input of
    Up -> {x = coord.x, y = coord.y-1}
    Down -> {x = coord.x, y = coord.y+1}
    Left -> {x = coord.x - 1, y = coord.y}
    Right -> {x = coord.x + 1, y = coord.y}

init : Model
init = { 
  mWalls = [
    {x=0,y=0}, {x=1,y=0}, {x=2,y=0}, {x=3,y=0}, {x=4,y=0},
    {x=0,y=1}, {x=4,y=1},
    {x=0,y=2}, {x=1,y=2}, {x=2,y=2}, {x=3,y=2}, {x=4,y=2}], 
  mCrates = [{x=2,y=1}], 
  mStorages = [{x=3,y=1}], 
  mWorker = {x=1,y=1}, 
  mMax = {x=4,y=2},
  mSteps = 0
  }

view : Model -> Html Msg
view model =
  div []
    [ table [ ] [ 
        getRow model 0,
        getRow model 1,
        getRow model 2
    ]
    , button [ onClick Up ] [ text "up" ]
    , button [ onClick Down ] [ text "down" ]
    , button [ onClick Left ] [ text "left" ]
    , button [ onClick Right ] [ text "right" ]
    ]

getRow : Model -> Int -> Html Msg
getRow model number = 
  let 
    cells = List.map (\x -> {x=x,y=number}) (List.range 0 model.mMax.x)

    orderedItems = List.map (\x -> toString x model) cells
    -- walls = List.map (\x -> (x,"#")) (List.filter (\x -> x.y == number) model.mWalls)
    -- creates = List.map (\x -> (x,"c")) (List.filter (\x -> x.y == number) model.mCrates)
    -- storages = List.map (\x -> (x,"x")) (List.filter (\x -> x.y == number) model.mStorages)
    -- worker = List.map (\x -> (x,"w")) (List.filter (\x -> x.y == number) [model.mWorker])
    -- items = List.concat [walls, creates, storages, worker] 
    -- orderedItems = List.map (\x -> Tuple.second x) (List.sortBy (\x -> (Tuple.first x).x) items )
    orderedItemsHtml = List.map stringToTableCell orderedItems
    result = tr[] orderedItemsHtml
  in result

toString : Coord -> Model -> String
toString coord model = 
  if isCrate model coord then "c"
  else if isStorage model coord then "s"
  else if isWall model coord then "#"
  else if isWorker model coord then "w"
  else " "

stringToTableCell : String -> Html Msg
stringToTableCell s = td [] [text s]