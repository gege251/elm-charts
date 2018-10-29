module Chart exposing (ChartModel, Msg, XAxis, YAxis, update, view)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias ChartModel r =
    { r
        | chartWidth : Int
        , chartHeight : Int
        , xAxis : XAxis
        , yAxes : List YAxis
    }


type alias Chart =
    { xGridPositions : List Int
    , xLabels : List String
    , ySets : List YSet
    , chartHeight : Int
    , chartWidth : Int
    , yLabels : List { label : String, visible : Bool }
    }


type alias YSet =
    { dots : List Position
    , lines : List Line
    , yGridVals : List Int
    , yGridPos : List Line
    , color : String
    }


type alias XAxis =
    { label : String
    , values : List String
    }


type alias YAxis =
    { label : String
    , color : String
    , values : List Int
    , visible : Bool
    }


type alias Position =
    ( Int, Int )


type alias Line =
    ( Position, Position )


type Msg
    = ToggleVisibility String


calcX : Float -> Int -> Int
calcX xScale grid =
    scale xScale grid - round (xScale / 2)


calcY : ChartModel r -> YHelperValues -> Int -> Int
calcY { chartHeight } { yScale, yMin } val =
    let
        yOffset =
            Maybe.withDefault 0 yMin |> scale yScale |> (\a -> (-) a 20)
    in
    (chartHeight - scale yScale val) + yOffset


type alias YHelperValues =
    { visibleYVals : List Int
    , yMax : Maybe Int
    , yMin : Maybe Int
    , ySpan : Maybe Int
    , yScale : Float
    , yGrids : List Int
    , color : String
    }


update : Msg -> ChartModel r -> ChartModel r
update msg model =
    case msg of
        ToggleVisibility label ->
            let
                newYAxes =
                    List.map
                        (\yAxis ->
                            if yAxis.label == label then
                                { yAxis | visible = not yAxis.visible }

                            else
                                yAxis
                        )
                        model.yAxes
            in
            { model | yAxes = newYAxes }


mkHelper : ChartModel r -> YAxis -> YHelperValues
mkHelper { xAxis, chartHeight } yAxis =
    let
        -- List of the visible Y values only
        visibleYVals =
            List.take xGridCount yAxis.values

        -- Maximum of y values
        yMax =
            List.maximum visibleYVals

        -- Minimum of y values
        yMin =
            List.minimum visibleYVals

        -- Span between min and max of y values
        ySpan =
            Maybe.map2 (-) yMax yMin

        -- Rate of scaling for y values to real pixels
        yScale =
            Maybe.withDefault 0 ySpan
                |> toFloat
                |> (/) (toFloat (chartHeight - 40))

        -- Number of values displayed on the x axis
        xGridCount =
            List.length xAxis.values

        -- The spacing value of horizontal gridlines
        yGridUnit =
            ySpan
                |> Maybe.withDefault 0
                |> toFloat
                |> logBase 10
                |> floor
                |> (^) 10

        -- Exact values of horizontal gridlines
        yGrids =
            Maybe.map2 (\min max -> List.range (min // yGridUnit) (max // yGridUnit)) yMin yMax
                |> Maybe.withDefault []
                |> List.map ((*) yGridUnit)
    in
    { visibleYVals = visibleYVals
    , yMax = yMax
    , yMin = yMin
    , ySpan = ySpan
    , yScale = yScale
    , yGrids = yGrids
    , color = yAxis.color
    }


mkChart : ChartModel r -> Chart
mkChart ({ chartHeight, chartWidth, xAxis, yAxes } as chartModel) =
    let
        -- Number of values displayed on the x axis
        xGridCount : Int
        xGridCount =
            List.length xAxis.values

        -- Rate of scaling for x values to real pixels
        xScale : Float
        xScale =
            chartWidth
                |> toFloat
                |> (\a -> (/) a (toFloat xGridCount))

        -- Vertical grid positions
        xGridPositions : List Int
        xGridPositions =
            List.map (\grid -> calcX xScale grid) (List.reverse <| List.range 1 xGridCount)

        -- Calculates horizontal gridline positions in pixels
        mkYGridLines : YHelperValues -> List ( Position, Position )
        mkYGridLines ({ yGrids } as helper) =
            let
                mkYGridLine yGrid =
                    let
                        y =
                            calcY chartModel helper yGrid
                    in
                    ( ( 0, y ), ( chartWidth, y ) )
            in
            List.map mkYGridLine yGrids

        -- Calculates y value dot position in pizels
        mkDots : YHelperValues -> List Position
        mkDots ({ visibleYVals } as helper) =
            List.map2 (\val xPos -> ( xPos, calcY chartModel helper val ))
                visibleYVals
                xGridPositions
    in
    { xGridPositions = xGridPositions
    , chartHeight = chartHeight
    , chartWidth = chartWidth
    , xLabels = xAxis.values
    , yLabels = List.map (\{ label, visible } -> { label = label, visible = visible }) yAxes
    , ySets =
        List.filter .visible yAxes
            |> List.map
                (mkHelper chartModel
                    >> (\helper ->
                            { dots = mkDots helper
                            , lines = (zipChain << mkDots) helper
                            , yGridVals = helper.yGrids
                            , yGridPos = mkYGridLines helper
                            , color = helper.color
                            }
                       )
                )
    }


view : ChartModel r -> Html Msg
view =
    mkChart >> viewChart


viewChart : Chart -> Html Msg
viewChart chart =
    let
        showGrid =
            List.map (showLine "grey" 0.1)

        showDots color =
            List.map (showDot color)

        showLines color =
            List.map (showLine color 2)

        showXLabels =
            List.map2 (showXLabel chart.chartHeight)

        showYLabels =
            List.map2 (showYLabel chart.chartWidth)

        showChartLabels =
            List.indexedMap
                (\index { label, visible } ->
                    text_
                        [ x <| String.fromInt (100 * index)
                        , y "20"
                        , onClick (ToggleVisibility label)
                        , fill <|
                            if visible then
                                "black"

                            else
                                "#ddd"
                        ]
                        [ text label ]
                )
    in
    svg [ width (String.fromInt chart.chartWidth), height (String.fromInt chart.chartHeight) ]
        (showChartLabels chart.yLabels
            ++ showXLabels chart.xLabels chart.xGridPositions
            ++ List.concat
                (List.map
                    (\ySet ->
                        showGrid ySet.yGridPos
                            ++ showYLabels ySet.yGridVals ySet.yGridPos
                            ++ showDots ySet.color ySet.dots
                            ++ showLines ySet.color ySet.lines
                    )
                    chart.ySets
                )
        )


showDot : String -> Position -> Svg msg
showDot color ( x, y ) =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r "5px"
        , stroke color
        , fill color
        ]
        []


showLine : String -> Float -> ( Position, Position ) -> Svg msg
showLine color lineWidth ( ( x1_, y1_ ), ( x2_, y2_ ) ) =
    line
        [ x1 <| String.fromInt x1_
        , y1 <| String.fromInt y1_
        , x2 <| String.fromInt x2_
        , y2 <| String.fromInt y2_
        , stroke color
        , strokeWidth <| String.fromFloat lineWidth
        ]
        []


showXLabel : Int -> String -> Int -> Svg msg
showXLabel yPos label x_ =
    text_
        [ x <| String.fromInt x_
        , y <| String.fromInt yPos
        , textAnchor "middle"
        , fill "black"
        ]
        [ text label ]


showYLabel : Int -> Int -> ( Position, Position ) -> Svg msg
showYLabel labelXPos gridVal ( _, ( _, y_ ) ) =
    text_
        [ x <| String.fromInt labelXPos
        , y <| String.fromInt y_
        , textAnchor "end"
        , fill "black"
        ]
        [ text <| String.fromInt gridVal ]


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
    toFloat int * float |> round
