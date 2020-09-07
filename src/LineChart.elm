module LineChart exposing
    ( lineChart
    , Model, Msg, initialModel, update
    )

{-| This module takes care of drawing line charts


# Create line charts

@docs lineChart

-}

import Color
import DataFrame exposing (DataFrame, XValueMapper, YValueMapper)
import InternalHelper exposing (DataScale(..), createXScale, createYScaleMinMax, indexedColor, paddingX, paddingY, xAxis, yAxis)
import Json.Decode as Decode
import Path
import Scale exposing (ContinuousScale)
import Shape
import SubPath
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, strokeWidth, transform, viewBox, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (on)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Length(..), Transform(..))
import VirtualDom


type alias LineType =
    List ( Float, Float ) -> SubPath.SubPath


type alias LineConfig a =
    { yFunc : YValueMapper a
    , label : Maybe String
    , color : Maybe Color.Color
    }


type alias Model =
    { mousePosition : Maybe MousePosition
    }


type alias MousePosition =
    { x : Int
    , y : Int
    }


type Msg
    = UpdateMousePosition MousePosition


initialModel : Model
initialModel =
    { mousePosition = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMousePosition pos ->
            ( { model | mousePosition = Just pos }, Cmd.none )


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
    , yMin : Maybe Float
    , yMax : Maybe Float
    , model : Model
    , msgMapper : Msg -> msg
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
            createYScaleMinMax h data.yMin data.yMax (List.map (\e -> e.yFunc) data.lines) data.dataFrame

        lineCount =
            List.length data.lines
    in
    TypedSvg.Core.map data.msgMapper <|
        svg
            [ viewBox 0 0 w h
            , on "mousemove" <| VirtualDom.Normal <| Decode.map UpdateMousePosition mouseMoveDecoder
            ]
            [ drawXAxis h xScale
            , drawYAxis yScale
            , drawSeries xScale yScale data.lineType data.dataFrame data.lines
            , g [ transform [ Translate (w - paddingX * 4) (h - paddingY * (toFloat lineCount + 1)) ] ] <|
                List.indexedMap (drawLabel lineCount) data.lines
            , drawCursor h data.model.mousePosition
            ]


drawSeries : DataScale a -> ContinuousScale Float -> LineType -> DataFrame a -> List (LineConfig a) -> Svg msg
drawSeries xScale yScale lineType df lines =
    g [ transform [ Translate (paddingX * 1.5) paddingY ] ] <|
        List.indexedMap (series xScale yScale lineType df) lines


series : DataScale a -> ContinuousScale Float -> LineType -> DataFrame a -> Int -> LineConfig a -> Svg msg
series xScale yScale lineType df index lineConfig =
    g [ class [ "series" ] ]
        [ line index xScale yScale lineConfig lineType df

        -- , g [] <| List.map (drawCircle xScale yScale) items
        ]


line : Int -> DataScale a -> ContinuousScale Float -> LineConfig a -> LineType -> DataFrame a -> Svg msg
line index xScale yScale lineConfig lineType df =
    let
        color =
            getColor index lineConfig
    in
    g [] [ drawCurve xScale yScale lineConfig.yFunc lineType color df ]


drawXAxis : Float -> DataScale a -> Svg msg
drawXAxis h xScale =
    g [ transform [ Translate (paddingX * 1.5) (h - paddingY) ] ] [ xAxis xScale ]


drawYAxis : ContinuousScale Float -> Svg msg
drawYAxis yScale =
    g [ transform [ Translate (paddingX * 1.5) paddingY ] ] [ yAxis yScale ]


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


drawCursor : Float -> Maybe MousePosition -> Svg msg
drawCursor h mousePosition =
    case mousePosition of
        Nothing ->
            g [] []

        Just { x, y } ->
            -- g
            --     [--  transform [ Translate (toFloat x) 0 ]
            --     ]
            --     [
            TypedSvg.line
                [ x1 (Px <| toFloat x)
                , y1 (Px paddingY)
                , x2 (Px <| toFloat x)
                , y2 (Px <| h - paddingY)
                , TypedSvg.Attributes.style "stroke:rgb(255,0,0);stroke-width:2"
                ]
                []



-- Decoders


mouseMoveDecoder : Decode.Decoder MousePosition
mouseMoveDecoder =
    Decode.map2 MousePosition
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)
