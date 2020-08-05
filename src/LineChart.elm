module LineChart exposing (lineChart)

{-| This module takes care of drawing line charts


# Create line charts

@docs lineChart

-}

import Color
import DataFrame exposing (DataFrame, XValueMapper, YValueMapper)
import InternalHelper exposing (DataScale(..), createXScale, createYScale, indexedColor, paddingX, paddingY, xAxis, yAxis)
import Path
import Scale exposing (ContinuousScale)
import Shape
import SubPath
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, strokeWidth, transform, viewBox, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Length(..), Transform(..))


type alias LineType =
    List ( Float, Float ) -> SubPath.SubPath


type alias LineConfig a =
    { yFunc : YValueMapper a
    , label : Maybe String
    , color : Maybe Color.Color
    }


{-| Creates a line chart with multiple lines
-}
lineChart :
    { dimensions : ( Float, Float )
    , lineType : LineType
    , xFunc : XValueMapper a
    , lines : List (LineConfig a)
    , dataFrame : DataFrame a
    , xAxisLabel : Maybe String
    , yAxisLabel : Maybe String
    }
    -> Svg msg
lineChart data =
    let
        w =
            Tuple.first data.dimensions

        h =
            Tuple.second data.dimensions

        xScale =
            createXScale w data.xFunc data.dataFrame

        yScale =
            createYScale h (List.map (\e -> e.yFunc) data.lines) data.dataFrame

        lineCount =
            List.length data.lines
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (paddingX - 1) (h - paddingY) ] ] [ xAxis xScale ]
        , g [ transform [ Translate (paddingX - 1) paddingY ] ] [ yAxis yScale ]
        , g [ transform [ Translate paddingX paddingY ] ] <|
            List.indexedMap (series xScale yScale data.lineType data.dataFrame) data.lines
        , g [ transform [ Translate (w - paddingX * 4) (h - paddingY * (toFloat lineCount + 1)) ] ] <|
            List.indexedMap (drawLabel lineCount) data.lines
        ]


series : DataScale a -> ContinuousScale Float -> LineType -> DataFrame a -> Int -> LineConfig a -> Svg msg
series xScale yScale lineType df index lineConfig =
    g [ class [ "series" ] ]
        [ line index xScale yScale lineConfig lineType df

        --, g [] <| List.map (drawCircle xScale yScale) items
        ]


line : Int -> DataScale a -> ContinuousScale Float -> LineConfig a -> LineType -> DataFrame a -> Svg msg
line index xScale yScale lineConfig lineType df =
    let
        color =
            getColor index lineConfig
    in
    g [] [ drawCurve xScale yScale lineConfig.yFunc lineType color df ]


drawCurve : DataScale a -> ContinuousScale Float -> YValueMapper a -> LineType -> Color.Color -> DataFrame a -> Svg msg
drawCurve xScale yScale yValueMapper lineType color df =
    case xScale of
        TimeScale ( scale, mapper ) ->
            reallyDrawCurve scale yScale mapper yValueMapper lineType color df

        ValueScale ( scale, mapper ) ->
            reallyDrawCurve scale yScale mapper yValueMapper lineType color df


reallyDrawCurve : ContinuousScale b -> ContinuousScale Float -> (a -> b) -> YValueMapper a -> LineType -> Color.Color -> DataFrame a -> Svg msg
reallyDrawCurve xScale yScale xValueMapper yValueMapper lineType color df =
    df.data
        |> List.map (\item -> ( Scale.convert xScale (xValueMapper item), Scale.convert yScale (yValueMapper item) ))
        |> List.map Just
        |> Shape.line lineType
        |> (\path -> Path.element path [ stroke color, fill FillNone, strokeWidth (Px 2) ])


drawLabel : Int -> Int -> LineConfig a -> Svg msg
drawLabel lineCount index lineConfig =
    case lineConfig.label of
        Nothing ->
            g [] []

        Just label ->
            let
                color =
                    getColor index lineConfig

                maxHeight =
                    toFloat lineCount * paddingY
            in
            g [ transform [ Translate 0 (maxHeight - toFloat (lineCount - index) * paddingY) ] ]
                [ TypedSvg.line [ stroke color, strokeWidth (Px 3), x1 (Px 0), y1 (Px -3), x2 (Px 10), y2 (Px -3) ] []
                , text_ [ x (Px 13) ] [ text label ]
                ]


getColor : Int -> LineConfig a -> Color.Color
getColor index lineConfig =
    case lineConfig.color of
        Nothing ->
            indexedColor index

        Just c ->
            c
