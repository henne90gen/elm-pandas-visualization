module InternalHelper exposing
    ( DataScale(..)
    , createXScale
    , createYScale
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
    in
    Scale.linear ( 0, w - 2 * paddingX ) ( minimum, maximum )


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
                |> clampToZeroIfPositive

        maximum =
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
