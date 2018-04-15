module Chart exposing (view, XAxis, YAxis)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)


type alias ChartModel r =
    { r
        | chartWidth : Int
        , chartHeight : Int
        , xAxis : XAxis
        , yAxis : List YAxis
    }


type alias Chart =
    { xGridPositions : List Int
    , xLabels : List String
    , ySets : List YSet
    , chartHeight : Int
    , chartWidth : Int
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


calcX : Float -> Int -> Int
calcX xScale grid =
    scale xScale grid - (round (xScale / 2))


calcY : ChartModel r -> YHelperValues -> Int -> Int
calcY { chartHeight } { yScale, yMin } val =
    let
        yOffset =
            Maybe.withDefault 0 yMin |> scale yScale |> flip (-) 20
    in
        (chartHeight - (scale yScale val)) + yOffset


type alias YHelperValues =
    { visibleYVals : List Int
    , yMax : Maybe Int
    , yMin : Maybe Int
    , ySpan : Maybe Int
    , yScale : Float
    , yGrids : List Int
    , color : String
    }


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
                |> (logBase) 10
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
mkChart ({ chartHeight, chartWidth, xAxis, yAxis } as chartModel) =
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
                |> flip (/) (toFloat xGridCount)

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
        mkDots : Int -> YHelperValues -> List Position
        mkDots xGridCount ({ visibleYVals } as helper) =
            List.map2 (\val xPos -> ( xPos, calcY chartModel helper val ))
                visibleYVals
                xGridPositions
    in
        { xGridPositions = xGridPositions
        , chartHeight = chartHeight
        , chartWidth = chartWidth
        , xLabels = xAxis.values
        , ySets =
            List.filter .visible yAxis
                |> List.map
                    (mkHelper chartModel
                        >> (\helper ->
                                { dots = mkDots xGridCount helper
                                , lines = (zipChain << mkDots xGridCount) helper
                                , yGridVals = helper.yGrids
                                , yGridPos = mkYGridLines helper
                                , color = helper.color
                                }
                           )
                    )
        }


view : ChartModel r -> Html msg
view =
    mkChart >> viewChart


viewChart : Chart -> Html msg
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
    in
        svg [ width (toString chart.chartWidth), height (toString chart.chartHeight) ]
            (showXLabels chart.xLabels chart.xGridPositions
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
        [ cx <| toString x
        , cy <| toString y
        , r "5px"
        , stroke color
        , fill color
        ]
        []


showLine : String -> Float -> ( Position, Position ) -> Svg msg
showLine color lineWidth ( ( x1_, y1_ ), ( x2_, y2_ ) ) =
    line
        [ x1 <| toString x1_
        , y1 <| toString y1_
        , x2 <| toString x2_
        , y2 <| toString y2_
        , stroke color
        , strokeWidth <| toString lineWidth
        ]
        []


showXLabel : Int -> String -> Int -> Svg msg
showXLabel yPos label x_ =
    text_
        [ x <| toString x_
        , y <| toString yPos
        , textAnchor "middle"
        , fill "black"
        ]
        [ text label ]


showYLabel : Int -> Int -> ( Position, Position ) -> Svg msg
showYLabel labelXPos gridVal ( _, ( _, y_ ) ) =
    text_
        [ x <| toString labelXPos
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
