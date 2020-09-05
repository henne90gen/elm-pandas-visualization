module InternalHelper exposing
    ( DataScale(..)
    , createXScale
    , createYScale
    , createYScaleMinMax
    , indexedColor
    , paddingX
    , paddingY
    , xAxis
    , yAxis
    )

import Axis
import Color
import DataFrame exposing (DataFrame, XValueMapper(..), YValueMapper)
import List.Extra
import Scale exposing (ContinuousScale)
import Time exposing (Posix)
import TypedSvg.Core exposing (Svg)


type DataScale a
    = TimeScale ( ContinuousScale Posix, a -> Posix )
    | ValueScale ( ContinuousScale Float, a -> Float )


{-| Default horizontal padding
-}
paddingX : Float
paddingX =
    30


{-| Default vertical padding
-}
paddingY : Float
paddingY =
    20


createXScale : Float -> XValueMapper a -> DataFrame a -> DataScale a
createXScale w xValueMapper df =
    case xValueMapper of
        TimeMapper mapper ->
            TimeScale ( createTimeScale w mapper df, mapper )

        ValueMapper mapper ->
            ValueScale ( createValueScale w mapper df, mapper )


createValueScale : Float -> (a -> Float) -> DataFrame a -> ContinuousScale Float
createValueScale w mapper df =
    let
        data =
            df.data
                |> List.map mapper

        minimum =
            data
                |> List.minimum
                |> Maybe.withDefault 0

        maximum =
            data
                |> List.maximum
                |> Maybe.withDefault 0

        valuePadding =
            (maximum - minimum) * 0.015
    in
    Scale.linear
        ( 0, w - 2 * paddingX )
        ( minimum - valuePadding, maximum + valuePadding )


createTimeScale : Float -> (a -> Posix) -> DataFrame a -> ContinuousScale Posix
createTimeScale w mapper df =
    let
        data =
            df.data
                |> List.map mapper
                |> List.map Time.posixToMillis

        start =
            data
                |> List.minimum
                |> Maybe.withDefault 0

        end =
            data
                |> List.maximum
                |> Maybe.withDefault 0

        timePadding =
            round (toFloat (end - start) * 0.015)

        startTime =
            Time.millisToPosix (start - timePadding)

        endTime =
            Time.millisToPosix (end + timePadding)
    in
    Scale.time Time.utc ( 0, w - 2 * paddingX ) ( startTime, endTime )


createYScale : Float -> List (YValueMapper a) -> DataFrame a -> ContinuousScale Float
createYScale h yValueMappers df =
    createYScale h yValueMappers df


createYScaleMinMax : Float -> Maybe Float -> Maybe Float -> List (YValueMapper a) -> DataFrame a -> ContinuousScale Float
createYScaleMinMax h yMin yMax yValueMappers df =
    let
        data =
            df.data
                |> List.map (\e -> List.map (\mapper -> mapper e) yValueMappers)
                |> List.foldl (\a b -> a ++ b) []

        minimum =
            case yMin of
                Just value ->
                    value

                Nothing ->
                    data
                        |> List.minimum
                        |> Maybe.withDefault 0
                        |> clampToZeroIfPositive

        maximum =
            case yMax of
                Just value ->
                    value

                Nothing ->
                    data
                        |> List.maximum
                        |> Maybe.withDefault 0
    in
    Scale.linear ( h - 2 * paddingY, 0 ) ( minimum, maximum )


clampToZeroIfPositive : Float -> Float
clampToZeroIfPositive value =
    if value > 0 then
        0

    else
        value


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


indexedColor : Int -> Color.Color
indexedColor index =
    Maybe.withDefault Color.blue
        (List.Extra.getAt index
            [ Color.rgb 0.121568627 0.466666667 0.705882353
            , Color.rgb 1 0.498039216 0.054901961
            ]
        )
