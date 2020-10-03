module LineChart exposing
    ( lineChart
    , lineChartInteractive
    , initialModel
    , update
    , subscriptions
    , Model
    , Msg
    )

{-| This module takes care of drawing line charts


# Create line charts

@docs lineChart


# Create interactive line charts

@docs lineChartInteractive
@docs initialModel
@docs update
@docs subscriptions
@docs Model
@docs Msg

-}

import Browser.Dom
import Browser.Events
import Color
import DataFrame exposing (DataFrame, XValueMapper, YValueMapper)
import InternalHelper exposing (DataScale(..), createXScale, createYScaleMinMax, indexedColor, paddingX, paddingY, xAxis, yAxis)
import Interpolation
import Json.Decode as Decode
import List.Extra
import Path
import Round
import Scale exposing (ContinuousScale)
import Shape
import SubPath
import Task
import Time
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, fill, id, stroke, strokeWidth, transform, viewBox, x, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (on)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import VirtualDom


type alias LineType =
    List ( Float, Float ) -> SubPath.SubPath


type alias LineConfig a =
    { yFunc : YValueMapper a
    , label : Maybe String
    , color : Maybe Color.Color
    }


type alias CursorConfig =
    { color : Color.Color
    , dotColor : Color.Color
    , dotSize : Float
    }


type alias ChartInfo =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


{-| Model for the internal state for interactive charts
-}
type alias Model =
    { id : String
    , chartInfo : ChartInfo
    , mousePosition : Maybe MousePosition
    }


type alias MousePosition =
    { x : Int
    , y : Int
    }


{-| Msg object to allow interactions with an interactive chart
-}
type Msg
    = UpdateMousePosition MousePosition
    | OnMouseLeave
    | OnResizeMsg Int Int
    | SvgElementMsg (Result Browser.Dom.Error Browser.Dom.Element)
    | ChartLoaded


{-| Initializes the model
-}
initialModel : String -> Model
initialModel id =
    { id = id
    , chartInfo = { x = 0, y = 0, width = 0, height = 0 }
    , mousePosition = Nothing
    }


{-| Updates the model with the given message
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMousePosition pos ->
            ( { model | mousePosition = Just pos }, Cmd.none )

        OnMouseLeave ->
            ( { model | mousePosition = Nothing }, Cmd.none )

        OnResizeMsg _ _ ->
            ( model, Task.attempt SvgElementMsg (Browser.Dom.getElement model.id) )

        ChartLoaded ->
            ( model, Task.attempt SvgElementMsg (Browser.Dom.getElement model.id) )

        SvgElementMsg result ->
            case result of
                Ok elem ->
                    ( { model
                        | chartInfo =
                            { x = elem.element.x
                            , y = elem.element.y
                            , width = elem.element.width
                            , height = elem.element.height
                            }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


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
    }
    -> Svg msg
lineChart data =
    lineChart_
        { dimensions = data.dimensions
        , lineType = data.lineType
        , xFunc = data.xFunc
        , lines = data.lines
        , dataFrame = data.dataFrame
        , xAxisLabel = data.xAxisLabel
        , yAxisLabel = data.yAxisLabel
        , yMin = data.yMin
        , yMax = data.yMax
        , msgMapper = \m -> m
        }
        []
        []


{-| Creates an interactive line chart with multiple lines
-}
lineChartInteractive :
    { dimensions : ( Float, Float )
    , lineType : LineType
    , xFunc : XValueMapper a
    , lines : List (LineConfig a)
    , dataFrame : DataFrame a
    , xAxisLabel : Maybe String
    , yAxisLabel : Maybe String
    , yMin : Maybe Float
    , yMax : Maybe Float
    , cursor : CursorConfig
    , model : Model
    , msgMapper : Msg -> msg
    }
    -> Svg msg
lineChartInteractive data =
    lineChart_
        { dimensions = data.dimensions
        , lineType = data.lineType
        , xFunc = data.xFunc
        , lines = data.lines
        , dataFrame = data.dataFrame
        , xAxisLabel = data.xAxisLabel
        , yAxisLabel = data.yAxisLabel
        , yMin = data.yMin
        , yMax = data.yMax
        , msgMapper = data.msgMapper
        }
        [ id data.model.id
        , on "mousemove" <| VirtualDom.Normal <| Decode.map UpdateMousePosition mouseMoveDecoder
        , on "mouseleave" <| VirtualDom.Normal <| Decode.succeed OnMouseLeave
        ]
        [ drawCursor
            data.dimensions
            data.model.chartInfo
            data.cursor
            data.lines
            data.dataFrame
            data.model.mousePosition
        , insertOnLoadHack
        ]


lineChart_ :
    { dimensions : ( Float, Float )
    , lineType : LineType
    , xFunc : XValueMapper a
    , lines : List (LineConfig a)
    , dataFrame : DataFrame a
    , xAxisLabel : Maybe String
    , yAxisLabel : Maybe String
    , yMin : Maybe Float
    , yMax : Maybe Float
    , msgMapper : internalMsg -> externalMsg
    }
    -> List (TypedSvg.Core.Attribute internalMsg)
    -> List (( DataScale a, ContinuousScale Float ) -> TypedSvg.Core.Svg internalMsg)
    -> Svg externalMsg
lineChart_ data additionalAttributes additionalElements =
    let
        ( w, h ) =
            data.dimensions

        xScale =
            createXScale w data.xFunc data.dataFrame

        yScale =
            createYScaleMinMax h data.yMin data.yMax (List.map (\e -> e.yFunc) data.lines) data.dataFrame

        lineCount =
            List.length data.lines
    in
    TypedSvg.Core.map data.msgMapper <|
        svg
            (viewBox 0 0 w h
                :: additionalAttributes
            )
            ([ drawXAxis h xScale
             , drawYAxis yScale
             , drawSeries xScale yScale data.lineType data.dataFrame data.lines
             , g [ transform [ Translate (w - paddingX * 4) (h - paddingY * (toFloat lineCount + 1)) ] ] <|
                List.indexedMap (drawLabel lineCount) data.lines
             ]
                ++ List.map (\e -> e ( xScale, yScale )) additionalElements
            )


drawSeries : DataScale a -> ContinuousScale Float -> LineType -> DataFrame a -> List (LineConfig a) -> Svg msg
drawSeries xScale yScale lineType df lines =
    g [ transform [ Translate (paddingX * 1.5) paddingY ] ] <|
        List.indexedMap (series xScale yScale lineType df) lines


series : DataScale a -> ContinuousScale Float -> LineType -> DataFrame a -> Int -> LineConfig a -> Svg msg
series xScale yScale lineType df index lineConfig =
    g [ class [ "series" ] ]
        [ line index xScale yScale lineConfig lineType df
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
        |> (\path -> Path.element path [ stroke <| Paint color, fill PaintNone, strokeWidth (Px 2) ])


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
                [ TypedSvg.line [ stroke <| Paint color, strokeWidth (Px 3), x1 (Px 0), y1 (Px -3), x2 (Px 10), y2 (Px -3) ] []
                , text_ [ x (Px 13) ] [ text label ]
                ]


getColor : Int -> LineConfig a -> Color.Color
getColor index lineConfig =
    case lineConfig.color of
        Nothing ->
            indexedColor index

        Just c ->
            c


getLabelAndMinMax :
    (comparable -> Float)
    -> (Float -> comparable)
    -> (a -> comparable)
    -> comparable
    -> (comparable -> String)
    -> Float
    -> DataFrame a
    -> ( String, ( Float, Float ) )
getLabelAndMinMax convert invert dataFunc default toString graphX df =
    let
        linesXData =
            List.map dataFunc df.data

        minValue =
            linesXData
                |> List.minimum
                |> Maybe.withDefault default
                |> convert

        maxValue =
            linesXData
                |> List.maximum
                |> Maybe.withDefault default
                |> convert
    in
    ( toString <| invert graphX
    , ( minValue, maxValue )
    )


drawCursor :
    ( Float, Float )
    -> ChartInfo
    -> CursorConfig
    -> List (LineConfig a)
    -> DataFrame a
    -> Maybe MousePosition
    -> ( DataScale a, ContinuousScale Float )
    -> Svg msg
drawCursor dimensions chartInfo cursor lines df mousePosition scales =
    case mousePosition of
        Nothing ->
            g [] []

        Just pos ->
            drawCursorMouseOnChart
                dimensions
                chartInfo
                cursor
                lines
                df
                pos
                scales


drawCursorMouseOnChart :
    ( Float, Float )
    -> ChartInfo
    -> CursorConfig
    -> List (LineConfig a)
    -> DataFrame a
    -> MousePosition
    -> ( DataScale a, ContinuousScale Float )
    -> Svg msg
drawCursorMouseOnChart ( w, h ) chartInfo cursor lines df pos scales =
    let
        ( chartX, chartY ) =
            toChartPos ( w, h ) chartInfo pos

        graphX =
            chartX - paddingX * 1.5
    in
    if graphX < 0 then
        g [] []

    else
        drawCursorMouseOnGraph ( w, h ) cursor lines df ( chartX, chartY ) scales


drawCursorMouseOnGraph :
    ( Float, Float )
    -> CursorConfig
    -> List (LineConfig a)
    -> DataFrame a
    -> ( Float, Float )
    -> ( DataScale a, ContinuousScale Float )
    -> Svg msg
drawCursorMouseOnGraph ( _, h ) cursor lines df ( chartX, chartY ) ( xScale, yScale ) =
    let
        graphX =
            chartX - paddingX * 1.5

        graphY =
            chartY - paddingY * 1.5

        ( label, ( min, max ) ) =
            case xScale of
                ValueScale ( s, f ) ->
                    getLabelAndMinMax
                        (Scale.convert s)
                        (Scale.invert s)
                        f
                        0
                        (Round.round 2)
                        graphX
                        df

                TimeScale ( s, f ) ->
                    getLabelAndMinMax
                        (\v -> v |> Time.millisToPosix |> Scale.convert s)
                        (\v -> v |> Scale.invert s |> Time.posixToMillis)
                        (\v -> v |> f |> Time.posixToMillis)
                        0
                        (\v -> formatTime (Time.millisToPosix v))
                        graphX
                        df
    in
    g [ transform [ Translate chartX paddingY ] ]
        [ TypedSvg.line
            [ x1 (Px 0)
            , y1 (Px 0)
            , x2 (Px 0)
            , y2 (Px <| h - 2 * paddingY)
            , stroke <| Paint cursor.color
            ]
            []
        , g
            [ transform [ Translate 0 -5, Scale 0.75 0.75 ] ]
            [ text_
                [ TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle ]
                [ text label ]
            ]
        , drawCursorDots ( min, max ) cursor yScale lines df ( graphX, graphY )
        ]


drawCursorDots : ( Float, Float ) -> CursorConfig -> ContinuousScale Float -> List (LineConfig a) -> DataFrame a -> ( Float, Float ) -> Svg msg
drawCursorDots ( min, max ) cursor yScale lines df ( graphX, graphY ) =
    if graphX < min || graphX > max then
        g [] []

    else
        g [] (drawCursorDotsMouseOnData ( min, max ) cursor yScale lines df ( graphX, graphY ))


drawCursorDotsMouseOnData : ( Float, Float ) -> CursorConfig -> ContinuousScale Float -> List (LineConfig a) -> DataFrame a -> ( Float, Float ) -> List (Svg msg)
drawCursorDotsMouseOnData ( min, max ) cursor yScale lines df ( graphX, graphY ) =
    let
        linesData =
            List.map
                (\l -> List.map l.yFunc df.data)
                lines

        firsts =
            List.map
                (\l -> List.head l |> Maybe.withDefault 0)
                linesData

        tails =
            List.map
                (\l -> List.tail l |> Maybe.withDefault [])
                linesData

        interpolators =
            List.Extra.zip firsts tails
                |> List.map
                    (\( first, tail ) -> Interpolation.piecewise Interpolation.float first tail)

        dotYs =
            List.map
                (\interpolator ->
                    let
                        value =
                            interpolator ((graphX - min) / (max - min))
                    in
                    ( value, Scale.convert yScale value )
                )
                interpolators
    in
    List.indexedMap
        (\idx ( value, dotY ) ->
            g []
                [ g [ transform [ Translate 0 dotY ] ]
                    [ TypedSvg.circle
                        [ TypedSvg.Attributes.cx <| Px 0
                        , TypedSvg.Attributes.cy <| Px 0
                        , TypedSvg.Attributes.r <| Px cursor.dotSize
                        , TypedSvg.Attributes.fill <| Paint cursor.dotColor
                        ]
                        []
                    ]
                , g [ transform [ Translate -5 (graphY + toFloat idx * 10), Scale 0.75 0.75 ] ]
                    [ text_
                        [ TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorEnd
                        , TypedSvg.Attributes.dominantBaseline TypedSvg.Types.DominantBaselineMiddle
                        ]
                        [ text <| Round.round 2 value ]
                    ]
                ]
        )
        dotYs


formatTime : Time.Posix -> String
formatTime time =
    ""
        ++ (time
                |> Time.toDay Time.utc
                |> String.fromInt
                |> zeroPad 2
           )
        ++ "."
        ++ (time
                |> Time.toMonth Time.utc
                |> monthToInt
                |> String.fromInt
                |> zeroPad 2
           )
        ++ "."
        ++ (time
                |> Time.toYear Time.utc
                |> String.fromInt
                |> zeroPad 2
           )
        ++ " "
        ++ (time
                |> Time.toHour Time.utc
                |> String.fromInt
                |> zeroPad 2
           )
        ++ ":"
        ++ (time
                |> Time.toMinute Time.utc
                |> String.fromInt
                |> zeroPad 2
           )


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


zeroPad : Int -> String -> String
zeroPad width s =
    if String.length s < width then
        String.repeat (width - String.length s) "0" ++ s

    else
        s


toChartPos : ( Float, Float ) -> ChartInfo -> MousePosition -> ( Float, Float )
toChartPos ( w, h ) chartInfo pos =
    let
        chartX =
            (toFloat pos.x - chartInfo.x) / chartInfo.width * w

        chartY =
            (toFloat pos.y - chartInfo.y) / chartInfo.height * h
    in
    ( chartX, chartY )


insertOnLoadHack : ( DataScale a, ContinuousScale Float ) -> Svg Msg
insertOnLoadHack _ =
    g
        []
        [ TypedSvg.style []
            [ text "@keyframes pulse { 0% {background-color: white;} 100% {background-color: white;}}" ]
        , g
            [ TypedSvg.Attributes.style "display: inline-block; animation: pulse 0.01s"
            , TypedSvg.Events.on "animationend" <| VirtualDom.Normal <| Decode.succeed ChartLoaded
            ]
            []
        ]


mouseMoveDecoder : Decode.Decoder MousePosition
mouseMoveDecoder =
    Decode.map2 MousePosition
        (Decode.at [ "pageX" ] Decode.int)
        (Decode.at [ "pageY" ] Decode.int)


{-| Subscribes to various events to allow interactive charts to function properly
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize OnResizeMsg
