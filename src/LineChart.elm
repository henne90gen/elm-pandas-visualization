module LineChart exposing
    ( singleLineChart, lineChart
    , XValueMapper(..)
    )

{-| This module takes care of drawing line charts


# Create line charts

@docs singleLineChart, lineChart


# Mappers

@docs XValueMapper

-}

import Axis
import Color
import DataFrame exposing (DataFrame, paddingX, paddingY)
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape
import Time exposing (Posix)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, strokeWidth, transform, viewBox)
import TypedSvg.Attributes.InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Length(..), Transform(..))


{-| Maps a row either to a time or to a value
-}
type XValueMapper a
    = TimeMapper (a -> Posix)
    | ValueMapper (a -> Float)


type alias YValueMapper a =
    a -> Float


type DataScale a
    = TimeScale ( ContinuousScale Posix, a -> Posix )
    | ValueScale ( ContinuousScale Float, a -> Float )


{-| Creates a line chart with a single line
-}
singleLineChart : ( Float, Float ) -> XValueMapper a -> YValueMapper a -> DataFrame a -> Svg msg
singleLineChart dim xValueMapper yValueMapper df =
    lineChart dim xValueMapper [ yValueMapper ] df


{-| Creates a line chart with multiple lines
-}
lineChart : ( Float, Float ) -> XValueMapper a -> List (YValueMapper a) -> DataFrame a -> Svg msg
lineChart ( w, h ) xValueMapper yValueMappers df =
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
    g [ class [ "series" ] ]
        [ line index xScale yScale yValueMapper df

        -- , g [] <| List.map (drawCircle xScale yScale) items
        ]


line : Int -> DataScale a -> ContinuousScale Float -> YValueMapper a -> DataFrame a -> Svg msg
line index xScale yScale yValueMapper df =
    g [] [ drawCurve xScale yScale yValueMapper (lineColor index) df ]


drawCurve : DataScale a -> ContinuousScale Float -> YValueMapper a -> Color.Color -> DataFrame a -> Svg msg
drawCurve xScale yScale yValueMapper color df =
    case xScale of
        TimeScale ( scale, mapper ) ->
            df.data
                |> List.map (\item -> ( Scale.convert scale (mapper item), Scale.convert yScale (yValueMapper item) ))
                |> List.map Just
                |> Shape.line Shape.naturalCurve
                |> (\path -> Path.element path [ stroke color, fill FillNone, strokeWidth (Px 2) ])

        ValueScale _ ->
            g [] []


lineColor : Int -> Color.Color
lineColor index =
    Maybe.withDefault Color.blue (List.Extra.getAt index [ Color.blue, Color.orange ])


createXScale : Float -> XValueMapper a -> DataFrame a -> DataScale a
createXScale w xValueMapper df =
    case xValueMapper of
        TimeMapper mapper ->
            TimeScale ( createTimeScale w mapper df, mapper )

        ValueMapper mapper ->
            ValueScale ( createValueScale w mapper df, mapper )


createValueScale : Float -> (a -> Float) -> DataFrame a -> ContinuousScale Float
createValueScale w mapper df =
    -- TODO implement this
    Scale.linear ( 0, 0 ) ( 100, 100 )


createTimeScale : Float -> (a -> Posix) -> DataFrame a -> ContinuousScale Posix
createTimeScale w mapper df =
    let
        data =
            df.data
                |> List.map mapper
                |> List.map Time.posixToMillis

        startTime =
            Time.millisToPosix
                (data
                    |> List.minimum
                    |> Maybe.withDefault 0
                )

        endTime =
            Time.millisToPosix
                (data
                    |> List.maximum
                    |> Maybe.withDefault 0
                )
    in
    Scale.time Time.utc ( 0, w ) ( startTime, endTime )


createYScale : Float -> List (YValueMapper a) -> DataFrame a -> ContinuousScale Float
createYScale h yValueMappers df =
    let
        data =
            df.data
                |> List.map (\e -> List.map (\mapper -> mapper e) yValueMappers)
                |> List.foldl (\a b -> a ++ b) []

        minimum =
            data
                |> List.minimum
                |> Maybe.withDefault 0

        maximum =
            data
                |> List.maximum
                |> Maybe.withDefault 0
    in
    Scale.linear ( h - 2 * paddingY, 0 ) ( minimum, maximum )


xAxis : DataScale a -> Svg msg
xAxis scale =
    case scale of
        TimeScale ( s, _ ) ->
            Axis.bottom [] s

        ValueScale ( s, _ ) ->
            Axis.bottom [] s


yAxis : ContinuousScale Float -> Svg msg
yAxis yScale =
    Axis.left [ Axis.tickCount 5 ] yScale


drawCircle : ContinuousScale Posix -> ContinuousScale Float -> Posix -> Float -> Svg msg
drawCircle xScale yScale xValue yValue =
    circle
        [ TypedSvg.Attributes.InPx.cx <| Scale.convert xScale xValue
        , TypedSvg.Attributes.InPx.cy <| Scale.convert yScale yValue
        , TypedSvg.Attributes.InPx.r 1.5
        , fill (Fill Color.white)
        , stroke Color.black
        ]
        []
