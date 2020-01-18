module BarChart exposing (singleBarChart, barChart)

{-| This module takes care of drawing bar charts


# Create bar charts

@docs singleBarChart, barChart

-}

import Axis
import Color
import DataFrame exposing (DataFrame, YValueMapper)
import Html exposing (text)
import InternalHelper exposing (indexedColor, paddingX, paddingY)
import List.Extra
import Round
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, style, svg)
import TypedSvg.Attributes exposing (class, fill, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Length(..), Transform(..))


type alias XValueMapper a =
    a -> String


type alias DrawingData a =
    { dimensions : ( Float, Float )
    , xValueMapper : XValueMapper a
    , yValueMappers : List (YValueMapper a)
    , dataFrame : DataFrame a
    }


type alias ExtendedDrawingData a =
    { drawingData : DrawingData a
    , minimum : Float
    , maximum : Float
    , xScale : BandScale a
    , yScale : ContinuousScale Float
    , numSeries : Int
    }


{-| Creates a simple bar chart
-}
singleBarChart : ( Float, Float ) -> XValueMapper a -> YValueMapper a -> DataFrame a -> Svg msg
singleBarChart dim xValueMapper yValueMapper df =
    barChart { dimensions = dim, xValueMapper = xValueMapper, yValueMappers = [ yValueMapper ], dataFrame = df }


{-| Creates a bar chart with multiple bars per x-value
-}
barChart : DrawingData a -> Svg msg
barChart drawingData =
    let
        w =
            getWidth drawingData

        h =
            getHeight drawingData

        ( minimum, maximum ) =
            getTotalMinMax drawingData.yValueMappers drawingData.dataFrame

        xScale =
            createXScale w drawingData.dataFrame

        yScale =
            createYScale h (maximum + maximum * 0.025)

        extendedDD =
            { drawingData = drawingData
            , minimum = minimum
            , maximum = maximum
            , xScale = xScale
            , yScale = yScale
            , numSeries = List.length drawingData.yValueMappers
            }
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (paddingX - 1) (h - paddingY) ] ] [ xAxis xScale drawingData.xValueMapper ]
        , g [ transform [ Translate (paddingX - 1) paddingY ] ] [ yAxis yScale ]
        , g [ transform [ Translate paddingX paddingY ] ] <|
            List.indexedMap (series extendedDD) drawingData.yValueMappers
        ]


series : ExtendedDrawingData a -> Int -> YValueMapper a -> Svg msg
series extendedDD index yValueMapper =
    let
        bandWidth =
            Scale.bandwidth extendedDD.xScale

        barWidth =
            bandWidth / toFloat extendedDD.numSeries
    in
    g [ class [ "series" ] ] <| List.map (bar extendedDD barWidth index yValueMapper) extendedDD.drawingData.dataFrame.data


bar : ExtendedDrawingData a -> Float -> Int -> YValueMapper a -> a -> Svg msg
bar extendedDD barWidth index yValueMapper elem =
    let
        h =
            getHeight extendedDD.drawingData

        xValue =
            Scale.convert extendedDD.xScale elem

        yValue =
            yValueMapper elem
    in
    g [ class [] ]
        [ rect
            [ x <| xValue + barWidth * toFloat index
            , y <| Scale.convert extendedDD.yScale yValue
            , width <| barWidth
            , height <| h - Scale.convert extendedDD.yScale yValue - 2 * paddingY
            , fill (Fill <| indexedColor index)
            ]
            []
        , textOnBar extendedDD barWidth index yValue elem
        ]


textOnBar : ExtendedDrawingData a -> Float -> Int -> Float -> a -> Svg msg
textOnBar extendedDD barWidth index year value =
    let
        h =
            getHeight extendedDD.drawingData

        x_ =
            getTextXPos barWidth index extendedDD.xScale value

        ( y_, anchor ) =
            getTextYPos h extendedDD.yScale year

        fontSize_ =
            min (barWidth / 3.5) 18
    in
    TypedSvg.text_
        [ x <| x_
        , y <| y_
        , transform [ Rotate 90 x_ y_ ]
        , textAnchor anchor
        , fontSize (Px fontSize_)
        , fill (Fill <| Color.rgb 0.1 0.1 0.1)
        ]
        [ text <| Round.round 2 year ]


getTextXPos : Float -> Int -> BandScale a -> a -> Float
getTextXPos barWidth index xScale elem =
    Scale.convert xScale elem + calculateBarTextOffset barWidth index


calculateBarTextOffset : Float -> Int -> Float
calculateBarTextOffset barWidth index =
    toFloat index * barWidth + (barWidth / 2 - barWidth * 0.1)


getTextYPos : Float -> ContinuousScale Float -> Float -> ( Float, AnchorAlignment )
getTextYPos h yScale value =
    let
        y_ =
            Scale.convert yScale value
    in
    if h - y_ > 100 then
        ( y_ + 10, AnchorStart )

    else
        ( y_ - 5, AnchorEnd )


getTotalMinMax : List (YValueMapper a) -> DataFrame a -> ( Float, Float )
getTotalMinMax yValueMappers df =
    ( Maybe.withDefault 0
        (yValueMappers
            |> List.map (getMin df)
            |> List.minimum
        )
    , Maybe.withDefault 0
        (yValueMappers
            |> List.map (getMax df)
            |> List.maximum
        )
    )


getMax : DataFrame a -> YValueMapper a -> Float
getMax df yValueMapper =
    Maybe.withDefault 0
        (df.data
            |> List.map yValueMapper
            |> List.maximum
        )


getMin : DataFrame a -> YValueMapper a -> Float
getMin df yValueMapper =
    Maybe.withDefault 0
        (df.data
            |> List.map yValueMapper
            |> List.minimum
        )


createXScale : Float -> DataFrame a -> BandScale a
createXScale w df =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * paddingX ) df.data


createYScale : Float -> Float -> ContinuousScale Float
createYScale h maximum =
    Scale.linear ( h - 2 * paddingY, 0 ) ( 0, maximum )


xAxis : BandScale a -> XValueMapper a -> Svg msg
xAxis xScale xValueMapper =
    Axis.bottom [] (Scale.toRenderable xValueMapper xScale)


yAxis : ContinuousScale a -> Svg msg
yAxis yScale =
    Axis.left [ Axis.tickCount 5 ] yScale


getWidth : DrawingData a -> Float
getWidth dd =
    Tuple.first dd.dimensions


getHeight : DrawingData a -> Float
getHeight dd =
    Tuple.second dd.dimensions
