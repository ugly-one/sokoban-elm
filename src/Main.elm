import Browser
import Html exposing (Html, button, div, text, table, tr, td)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = Left | Right | Up | Down

type alias Coord = {
  x : Int, 
  y : Int
  }

type alias Model = {
    mWalls : List Coord,
    mCrates : List Coord,
    mStorages : List Coord,
    mWorker : Coord,
    mMax : Coord,
    mSteps : Int
    } 

update : Msg -> Model -> Model
update msg model = model

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
    walls = List.map (\x -> (x,"#")) (List.filter (\x -> x.y == number) model.mWalls)
    creates = List.map (\x -> (x,"c")) (List.filter (\x -> x.y == number) model.mCrates)
    storages = List.map (\x -> (x,"x")) (List.filter (\x -> x.y == number) model.mStorages)
    worker = List.map (\x -> (x,"w")) (List.filter (\x -> x.y == number) [model.mWorker])
    items = List.concat [walls, creates, storages, worker] 
    orderedItems = List.map (\x -> Tuple.second x) (List.sortBy (\x -> (Tuple.first x).x) items )
    orderedItemsHtml = List.map stringToTableCell orderedItems
    result = tr[] orderedItemsHtml
  in result

stringToTableCell : String -> Html Msg
stringToTableCell s = td [] [text s]