module LineChartExample exposing (main)

import Browser
import Color
import DataFrame exposing (DataFrame)
import Html exposing (Html)
import Html.Attributes
import LineChart exposing (lineChart)
import Random
import Shape
import Time


type alias Model =
    { df : DataFrame DataPoint
    , dfLarge : DataFrame DataPoint
    , valueChartModel : LineChart.Model
    , timeChartModel : LineChart.Model
    , timeChartLargeModel : LineChart.Model
    }


type alias DataPoint =
    { x1 : Float
    , x2 : Time.Posix
    , y1 : Float
    , y2 : Float
    }


type Msg
    = ValueChartMsg LineChart.Msg
    | TimeChartMsg LineChart.Msg
    | TimeChartLargeMsg LineChart.Msg
    | GotLargeDataSet (List Float)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ValueChartMsg m ->
            let
                ( newModel, newCmd ) =
                    LineChart.update m model.valueChartModel
            in
            ( { model | valueChartModel = newModel }, Cmd.map ValueChartMsg newCmd )

        TimeChartMsg m ->
            let
                ( newModel, newCmd ) =
                    LineChart.update m model.timeChartModel
            in
            ( { model | timeChartModel = newModel }, Cmd.map TimeChartMsg newCmd )

        TimeChartLargeMsg m ->
            let
                ( newModel, newCmd ) =
                    LineChart.update m model.timeChartLargeModel
            in
            ( { model | timeChartLargeModel = newModel }, Cmd.map TimeChartLargeMsg newCmd )

        GotLargeDataSet data ->
            ( { model | dfLarge = DataFrame.create <| List.indexedMap (\i f -> dataPoint i (toFloat i + f) (toFloat i - f)) data }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ValueChartMsg (LineChart.subscriptions model.valueChartModel)
        , Sub.map TimeChartMsg (LineChart.subscriptions model.timeChartModel)
        ]


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { df = DataFrame.create exampleData
      , dfLarge = DataFrame.create []
      , valueChartModel = LineChart.initialModel "value"
      , timeChartModel = LineChart.initialModel "time"
      , timeChartLargeModel = LineChart.initialModel "time-large"
      }
    , generateLargeDataSet
    )


exampleData : List DataPoint
exampleData =
    [ dataPoint 0 10 7
    , dataPoint 1 15 10
    , dataPoint 2 13 5
    , dataPoint 3 9 11
    , dataPoint 4 11 7
    , dataPoint 5 17 10
    , dataPoint 6 10 8
    ]


generateLargeDataSet : Cmd Msg
generateLargeDataSet =
    Random.list 100 (Random.float 0 100)
        |> Random.generate GotLargeDataSet


dataPoint : Int -> Float -> Float -> DataPoint
dataPoint x y1 y2 =
    { x1 = toFloat x, x2 = Time.millisToPosix <| x * 86400000, y1 = y1, y2 = y2 }


view : Model -> Browser.Document Msg
view model =
    { title = "LineChart Example"
    , body =
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "align-items" "center"
            ]
            [ Html.div
                [ Html.Attributes.style "width" "60%" ]
                [ valueChartInteractive model ]
            ]
        , timeChartLarge model
        , timeChart model
        , valueChart model
        ]
    }


valueChart : Model -> Html msg
valueChart model =
    LineChart.lineChart
        { dimensions = ( 600, 300 )
        , lineType = Shape.linearCurve
        , xFunc = DataFrame.ValueMapper (\e -> e.x1)
        , lines =
            [ { yFunc = \e -> e.y1 * 1000
              , label = Just "my-data-1"
              , color = Just (Color.rgb 1 0 1)
              }
            , { yFunc = \e -> e.y2 * 1000
              , label = Just "my-data-2"
              , color = Just (Color.rgb 0 1 1)
              }
            , { yFunc = \e -> (e.y2 - 1) * 1000
              , label = Just "my-data-3"
              , color = Just (Color.rgb 1 1 0)
              }
            ]
        , dataFrame = model.df
        , xAxisLabel = Nothing
        , yAxisLabel = Nothing
        , yMin = Just -5
        , yMax = Nothing
        }


valueChartInteractive : Model -> Html Msg
valueChartInteractive model =
    LineChart.lineChartInteractive
        { dimensions = ( 600, 300 )
        , lineType = Shape.linearCurve
        , xFunc = DataFrame.ValueMapper (\e -> e.x1)
        , lines =
            [ { yFunc = \e -> e.y1 * 1000
              , label = Just "my-data-1"
              , color = Just (Color.rgb 1 0 1)
              }
            , { yFunc = \e -> e.y2 * 1000
              , label = Just "my-data-2"
              , color = Just (Color.rgb 0 1 1)
              }
            , { yFunc = \e -> (e.y2 - 1) * 1000
              , label = Just "my-data-3"
              , color = Just (Color.rgb 1 1 0)
              }
            ]
        , dataFrame = model.df
        , xAxisLabel = Nothing
        , yAxisLabel = Nothing
        , yMin = Just -5
        , yMax = Nothing
        , cursor = { color = Color.red, dotColor = Color.darkGreen, dotSize = 1 }
        , model = model.valueChartModel
        , msgMapper = ValueChartMsg
        }


timeChart : Model -> Html Msg
timeChart model =
    LineChart.lineChartInteractive
        { dimensions = ( 600, 300 )
        , lineType = Shape.linearCurve
        , xFunc = DataFrame.TimeMapper (\e -> e.x2)
        , lines =
            [ { yFunc = \e -> e.y1 * 1000
              , label = Just "my-data-1"
              , color = Just (Color.rgb 1 0 1)
              }
            , { yFunc = \e -> e.y2 * 1000
              , label = Just "my-data-2"
              , color = Just (Color.rgb 0 1 1)
              }
            , { yFunc = \e -> (e.y2 - 1) * 1000
              , label = Just "my-data-3"
              , color = Just (Color.rgb 1 1 0)
              }
            ]
        , dataFrame = model.df
        , xAxisLabel = Nothing
        , yAxisLabel = Nothing
        , yMin = Just -5
        , yMax = Nothing
        , cursor = { color = Color.green, dotColor = Color.black, dotSize = 1 }
        , model = model.timeChartModel
        , msgMapper = TimeChartMsg
        }


timeChartLarge : Model -> Html Msg
timeChartLarge model =
    LineChart.lineChartInteractive
        { dimensions = ( 600, 300 )
        , lineType = Shape.linearCurve
        , xFunc = DataFrame.TimeMapper (\e -> e.x2)
        , lines =
            [ { yFunc = \e -> e.y1 * 1000
              , label = Just "my-data-1"
              , color = Just (Color.rgb 1 0 1)
              }
            ]
        , dataFrame = model.dfLarge
        , xAxisLabel = Nothing
        , yAxisLabel = Nothing
        , yMin = Nothing
        , yMax = Nothing
        , cursor = { color = Color.green, dotColor = Color.black, dotSize = 1 }
        , model = model.timeChartLargeModel
        , msgMapper = TimeChartLargeMsg
        }
