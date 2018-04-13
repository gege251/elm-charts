module Chart exposing (viewChart, XAxis, YAxis)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)


type alias ChartModel r =
    { r
        | chartWidth : Int
        , chartHeight : Int
        , xAxis : XAxis
        , yAxis : YAxis
    }


type alias Chart =
    { dots : List Position
    , lines : List ( Position, Position )
    , yGridVals : List Int
    , yGridPos : List ( Position, Position )
    }


type alias XAxis =
    { label : String
    , values : List String
    }


type alias YAxis =
    { label : String
    , color : String
    , values : List Int
    }


type alias Position =
    ( Int, Int )


mkChart : ChartModel r -> Chart
mkChart { chartHeight, chartWidth, xAxis, yAxis } =
    let
        displayedYVals : List Int
        displayedYVals =
            List.take xGridCount yAxis.values

        calcX : Float -> Int -> Int
        calcX xScale grid =
            scale xScale grid - (round (xScale / 2))

        calcY : Float -> Int -> Int
        calcY yScale val =
            (chartHeight - (scale yScale val)) + yOffset

        yMax : Maybe Int
        yMax =
            List.maximum displayedYVals

        yMin : Maybe Int
        yMin =
            List.minimum displayedYVals

        ySpan : Maybe Int
        ySpan =
            Maybe.map2 (-) yMax yMin

        xGridCount : Int
        xGridCount =
            List.length xAxis.values

        xScale : Float
        xScale =
            chartWidth
                |> toFloat
                |> flip (/) (toFloat xGridCount)

        yScale : Float
        yScale =
            Maybe.withDefault 0 ySpan
                |> toFloat
                |> (/) (toFloat (chartHeight - 40))

        yGrids : List Int
        yGrids =
            let
                yGridUnit : Int
                yGridUnit =
                    ySpan
                        |> Maybe.withDefault 0
                        |> toFloat
                        |> (logBase) 10
                        |> floor
                        |> (^) 10
            in
                Maybe.map2 (\min max -> List.range (min // yGridUnit) (max // yGridUnit)) yMin yMax
                    |> Maybe.withDefault []
                    |> List.map ((*) yGridUnit)

        yOffset : Int
        yOffset =
            Maybe.withDefault 0 yMin |> scale yScale |> flip (-) 20

        mkYGrid : Int -> ( Position, Position )
        mkYGrid gridVal =
            let
                y =
                    calcY yScale gridVal
            in
                ( ( 0, y ), ( chartWidth, y ) )

        mkDots : List Int -> Int -> List Position
        mkDots values gridCount =
            List.map2 (\val grid -> ( calcX xScale grid, calcY yScale val ))
                yAxis.values
                (List.reverse <| List.range 1 gridCount)
    in
        { dots = mkDots yAxis.values xGridCount
        , lines = zipChain <| mkDots yAxis.values xGridCount
        , yGridVals = yGrids
        , yGridPos = List.map mkYGrid yGrids
        }


viewChart : ChartModel r -> Html msg
viewChart model =
    let
        showGrid =
            List.map (showLine "grey")

        showDots =
            List.map (showDot model.yAxis.color)

        showLines =
            List.map (showLine model.yAxis.color)

        showXLabels =
            List.map2 (showXLabel model.chartHeight)

        showYLabels =
            List.map2 (showYLabel model.chartWidth)

        chart1 =
            mkChart model

        chart2 =
            mkChart model
    in
        svg [ width (toString model.chartWidth), height (toString model.chartHeight) ]
            (showGrid chart1.yGridPos
                ++ showDots chart1.dots
                ++ showLines chart1.lines
                ++ showXLabels model.xAxis.values chart1.dots
                ++ showYLabels chart1.yGridVals chart1.yGridPos
            )


showDot : String -> Position -> Svg msg
showDot color ( x, y ) =
    circle
        [ cx <| toString x
        , cy <| toString y
        , r "5px"
        , stroke color
        , fill color
        ]
        []


showLine : String -> ( Position, Position ) -> Svg msg
showLine color ( ( x1_, y1_ ), ( x2_, y2_ ) ) =
    line
        [ x1 <| toString x1_
        , y1 <| toString y1_
        , x2 <| toString x2_
        , y2 <| toString y2_
        , stroke color
        ]
        []


showXLabel : Int -> String -> Position -> Svg msg
showXLabel yPos label ( x_, _ ) =
    text_
        [ x <| toString x_
        , y <| toString yPos
        , textAnchor "middle"
        , fill "black"
        ]
        [ text label ]


showYLabel : Int -> Int -> ( Position, Position ) -> Svg msg
showYLabel chartWidth gridVal ( _, ( _, y_ ) ) =
    text_
        [ x <| toString chartWidth
        , y <| toString y_
        , textAnchor "end"
        , fill "black"
        ]
        [ text <| toString gridVal ]


zipChain : List a -> List ( a, a )
zipChain list =
    case list of
        [] ->
            []

        [ a ] ->
            []

        a :: b :: rest ->
            ( a, b ) :: zipChain (b :: rest)


scale : Float -> Int -> Int
scale float int =
    (toFloat int) * float |> round
