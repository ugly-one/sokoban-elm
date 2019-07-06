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

firstLevel : List String
firstLevel = [
  "    #####          ",
  "    #   #          ",
  "    #o  #          ",
  "  ###  o##         ",
  "  #  o o #         ",
  "### # ## #   ######",
  "#   # ## #####  ..#",
  "# o  o          ..#",
  "##### ### #@##  ..#",
  "    #     #########",
  "    #######        "
  ]

emptyWorld : Model
emptyWorld = {
  mWalls = [],
  mCrates = [],
  mStorages = [],
  mWorker = {x=0,y=0},
  mMax = {x=0,y=0},
  mSteps = 0
  }

updateWorld : Model -> (Coord, Char) -> Model
updateWorld world (coord, c) =
    case c of   '#' -> {world | mWalls = [coord] ++ world.mWalls}
                '@' -> {world | mWorker = coord}
                'o' -> {world | mCrates = [coord] ++ world.mCrates}
                '.' -> {world | mStorages = [coord] ++ world.mStorages}
                _ -> world

generateCoordinates : Int -> Int -> List Coord
generateCoordinates a b = 
  List.concat (List.map (\q -> List.map (\w -> {x=w,y=q}) (List.range 0 a)) (List.range 0 b))

parseLevel : List String -> Model
parseLevel input = 
    let
      a = List.head input
      t = case a of
            Just x -> x
            Nothing -> ""
      width = List.length (String.toList t)
      height = List.length input
      coordinates = generateCoordinates (width-1) (height-1)
      maybeLastCoordinate = List.head ( List.reverse coordinates)
      lastCoordinate = case maybeLastCoordinate of 
                        Just x -> x
                        Nothing -> {x=0,y=0} -- TODO return empty model
      inputCharList = List.map String.toList input
      flatInput = List.concat inputCharList
      elements = List.map2 (\char coord -> (coord,char)) flatInput coordinates
      world = List.foldl (\x y -> updateWorld y x) emptyWorld elements
    in {world | mMax = lastCoordinate}

init : Model
init = parseLevel firstLevel

view : Model -> Html Msg
view model =
  div []
    [ table [ ] (getRows model)
    , button [ onClick Up ] [ text "up" ]
    , button [ onClick Down ] [ text "down" ]
    , button [ onClick Left ] [ text "left" ]
    , button [ onClick Right ] [ text "right" ]
    ]

getRows : Model -> List (Html Msg)
getRows model = List.map (\i -> getRow model i) (List.range 0 model.mMax.y)

getRow : Model -> Int -> Html Msg
getRow model number = 
  let 
    cells = List.map (\x -> {x=x,y=number}) (List.range 0 model.mMax.x)
    stringRepresentations = List.map (\x -> toString x model) cells
    htmlRepresentations = List.map stringToTableCell stringRepresentations
  in tr[] htmlRepresentations

toString : Coord -> Model -> String
toString coord model = 
  -- order matters here. If the worker is on a storage - show worker
  -- so the check if given coordinate is worker has to be done earlier
  if isCrate model coord then "c"
  else if isWorker model coord then "w"
  else if isStorage model coord then "s"
  else if isWall model coord then "#"
  else " "

stringToTableCell : String -> Html Msg
stringToTableCell s = td [] [text s]