module Main exposing (main)

import Browser
import Chart
import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (..)


type alias Model =
    { title : String
    , chartWidth : Int
    , chartHeight : Int
    , xAxis : Chart.XAxis
    , yAxes : List Chart.YAxis
    , newValue : String
    }


type Msg
    = NoOp
    | ChartMsg Chart.Msg



-- | InputValue String
-- | AddValue String


init : Model
init =
    { title = "Chart title"
    , chartWidth = 1300
    , chartHeight = 700
    , xAxis =
        { label = "X"
        , values = [ "12月", "11月", "10月", "9月", "8月", "7月", "6月", "5月", "4月", "3月", "2月", "1月" ]
        }
    , yAxes =
        [ { label = "yellow chart"
          , color = "yellow"
          , values = [ 21, 19, 18, 20, 21, 22, 20, 19, 18, 18, 18, 17 ]
          , visible = False
          }
        , { label = "blue chart"
          , color = "blue"
          , values = [ 1, 4, 5, 4, 7, 8, 7, 11, 11, 20, 11, 12 ]
          , visible = True
          }
        , { label = "green chart"
          , color = "green"
          , values = [ 201, 195, 197, 203, 200, 201, 198, 190, 187, 188, 185, 180 ]
          , visible = True
          }
        ]
    , newValue = ""
    }



-- setYAxis : Chart.YAxis -> Model -> Model
-- setYAxis yAxis model =
--     { model | yAxis = yAxis }
-- asYAxisIn : Model -> Chart.YAxis -> Model
-- asYAxisIn =
--     flip setYAxis
-- setValues : List Int -> Chart.YAxis -> Chart.YAxis
-- setValues newlist yAxis =
--     { yAxis | values = newlist }
-- asValuesIn : Chart.YAxis -> List Int -> Chart.YAxis
-- asValuesIn =
--     flip setValues


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ChartMsg chartmsg ->
            Chart.update chartmsg model



-- InputValue value ->
--     { model | newValue = value } ! [ Cmd.none ]
-- AddValue value ->
--     case String.toInt value of
--         Err _ ->
--             model ! [ Cmd.none ]
--         Ok intVal ->
--             ((intVal :: model.yAxis.values)
--                 |> asValuesIn model.yAxis
--                 |> asYAxisIn { model | newValue = "" }
--             )
--                 ! [ Cmd.none ]


view : Model -> Html Msg
view model =
    div []
        -- [ div [] [ text (viewList model.yAxes) ]
        -- , form [ onSubmit (AddValue model.newValue) ]
        --     [ input
        --         [ onInput InputValue
        --         , placeholder "Insert a new value"
        --         , value model.newValue
        --         ]
        --         []
        --     , button [] [ text "OK" ]
        --     ]
        [ Html.map ChartMsg (Chart.view model)
        ]


viewList : List Int -> String
viewList list =
    list
        |> List.map (\num -> String.fromInt num)
        |> String.join ", "


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
