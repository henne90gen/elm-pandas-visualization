module ScatterChart exposing (scatterChart, singleScatterChart)

import Color
import DataFrame exposing (DataFrame, XValueMapper, YValueMapper)
import InternalHelper exposing (DataScale(..), createXScale, createYScale, indexedColor, paddingX, paddingY, xAxis, yAxis)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Length(..), Transform(..))


singleScatterChart : ( Float, Float ) -> XValueMapper a -> YValueMapper a -> DataFrame a -> Svg msg
singleScatterChart dim xValueMapper yValueMapper df =
    scatterChart dim xValueMapper [ yValueMapper ] df


scatterChart : ( Float, Float ) -> XValueMapper a -> List (YValueMapper a) -> DataFrame a -> Svg msg
scatterChart ( w, h ) xValueMapper yValueMappers df =
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
            List.indexedMap (series xScale yScale df) yValueMappers
        ]


series : DataScale a -> ContinuousScale Float -> DataFrame a -> Int -> YValueMapper a -> Svg msg
series xScale yScale df index yValueMapper =
    g [ class [ "series" ] ] <|
        List.map (drawCircle xScale yScale yValueMapper index) df.data


drawCircle : DataScale a -> ContinuousScale Float -> YValueMapper a -> Int -> a -> Svg msg
drawCircle xScale yScale yValueMapper index value =
    case xScale of
        TimeScale ( scale, mapper ) ->
            reallyDrawCircle scale yScale mapper yValueMapper index value

        ValueScale ( scale, mapper ) ->
            reallyDrawCircle scale yScale mapper yValueMapper index value


reallyDrawCircle : ContinuousScale b -> ContinuousScale Float -> (a -> b) -> YValueMapper a -> Int -> a -> Svg msg
reallyDrawCircle xScale yScale xValueMapper yValueMapper index value =
    circle
        [ TypedSvg.Attributes.InPx.cx <| Scale.convert xScale (xValueMapper value)
        , TypedSvg.Attributes.InPx.cy <| Scale.convert yScale (yValueMapper value)
        , TypedSvg.Attributes.InPx.r 1.5
        , fill (Fill <| indexedColor index)
        , stroke <| Color.rgba 1 1 1 0
        ]
        []
