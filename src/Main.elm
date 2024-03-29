import Browser
import Json.Decode as Decode
import Html exposing (Html, button, div, img, text, table, tr, td)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, src)
import Browser.Events exposing (onKeyDown)

main =
  Browser.element 
    { init = init
    , view = view
    , update = update 
    , subscriptions = subscriptions 
    }

type Msg
  = Left
  | Right
  | Down
  | Up
  | Other


type alias Coord = { x : Int, y : Int  }

type alias Model = {
  mWalls : List Coord,
  mCrates : List Coord,
  mStorages : List Coord,
  mWorker : Coord,
  mMax : Coord,
  mSteps : Int
  } 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  if isValid msg model then 
    (modifyWorldNoValidation model msg, Cmd.none)
  else (model, Cmd.none)    

init : () -> (Model, Cmd Msg)
init _ = (parseLevel firstLevel, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [
      table [ ] (getRows model)
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onKeyDown keyDecoder
  ] 



keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Msg
toDirection string =
  case string of
    "ArrowLeft" ->
      Left

    "ArrowRight" ->
      Right

    "ArrowUp" ->
      Up

    "ArrowDown" ->
      Down

    _ ->
      Other

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
isFinished model = 
  (List.sortWith compareCoords model.mCrates) == (List.sortWith compareCoords model.mStorages)

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
    Other -> coord

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



getRows : Model -> List (Html Msg)
getRows model = List.map (\i -> getRow model i) (List.range 0 model.mMax.y)

getRow : Model -> Int -> Html Msg
getRow model number = 
  let 
    cells = List.map (\x -> {x=x,y=number}) (List.range 0 model.mMax.x)
    imagePaths = List.map (\x -> toImagePath x model) cells
    htmlRepresentations = List.map imagePathToTableCell imagePaths
  in tr[] htmlRepresentations

toImagePath : Coord -> Model -> String
toImagePath coord model = 
  -- order matters here. If the worker is on a storage - show worker
  -- so the check if given coordinate is a worker has to be done earlier
  if isCrate model coord && isStorage model coord then "\\images\\crateStorage.png"
  else if isCrate model coord then "\\images\\crate.png"
  else if isWorker model coord then "\\images\\penguin.png"
  else if isStorage model coord then "\\images\\dot.png"
  else if isWall model coord then "\\images\\wall.png"
  else "\\images\\empty.png"

imagePathToTableCell : String -> Html Msg
imagePathToTableCell path = td [] [ img [ 
        style "width" "30px",
        style "height" "30px",
        src path ] []]