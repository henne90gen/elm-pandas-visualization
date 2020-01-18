module LineChart exposing (singleLineChart, lineChart)

{-| This module takes care of drawing line charts


# Create line charts

@docs singleLineChart, lineChart


# Mappers

@docs XValueMapper, valueMapper, timeMapper

-}

import Color
import DataFrame exposing (DataFrame, XValueMapper, YValueMapper)
import InternalHelper exposing (DataScale(..), createXScale, createYScale, indexedColor, paddingX, paddingY, xAxis, yAxis)
import Path
import Scale exposing (ContinuousScale)
import Shape
import SubPath
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, strokeWidth, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Length(..), Transform(..))


type alias LineType =
    List ( Float, Float ) -> SubPath.SubPath


{-| Creates a line chart with a single line
-}
singleLineChart : ( Float, Float ) -> LineType -> XValueMapper a -> YValueMapper a -> DataFrame a -> Svg msg
singleLineChart dim lineType xValueMapper yValueMapper df =
    lineChart dim lineType xValueMapper [ yValueMapper ] df


{-| Creates a line chart with multiple lines
-}
lineChart : ( Float, Float ) -> LineType -> XValueMapper a -> List (YValueMapper a) -> DataFrame a -> Svg msg
lineChart ( w, h ) lineType xValueMapper yValueMappers df =
    let
        xScale =
            createXScale w xValueMapper df

        yScale =
            createYScale h yValueMappers df
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (paddingX - 1) (h - paddingY) ] ] [ xAxis xScale ]
        , g [ transform [ Translate (paddingX - 1) paddingY ] ] [ yAxis yScale ]
        , g [ transform [ Translate paddingX paddingY ] ] <|
            List.indexedMap (series xScale yScale lineType df) yValueMappers
        ]


series : DataScale a -> ContinuousScale Float -> LineType -> DataFrame a -> Int -> YValueMapper a -> Svg msg
series xScale yScale lineType df index yValueMapper =
    g [ class [ "series" ] ]
        [ line index xScale yScale yValueMapper lineType df

        -- , g [] <| List.map (drawCircle xScale yScale) items
        ]


line : Int -> DataScale a -> ContinuousScale Float -> YValueMapper a -> LineType -> DataFrame a -> Svg msg
line index xScale yScale yValueMapper lineType df =
    g [] [ drawCurve xScale yScale yValueMapper lineType (indexedColor index) df ]


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
