module LineChartExample exposing (main)

import Browser
import Color
import DataFrame exposing (DataFrame)
import Html exposing (Html)
import LineChart
import Shape
import Time


type alias Model =
    DataFrame DataPoint


type alias DataPoint =
    { x1 : Float
    , x2 : Time.Posix
    , y1 : Float
    , y2 : Float
    }


type Msg
    = DefaultMsg


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : flags -> ( Model, Cmd Msg )
init _ =
    ( DataFrame.create exampleData, Cmd.none )


exampleData : List DataPoint
exampleData =
    [ dataPoint 0 10 7
    , dataPoint 1 15 10
    , dataPoint 2 13 5
    , dataPoint 3 9 8
    , dataPoint 4 11 7
    , dataPoint 5 17 10
    , dataPoint 6 10 8
    ]


dataPoint : Int -> Float -> Float -> DataPoint
dataPoint x y1 y2 =
    { x1 = toFloat x, x2 = Time.millisToPosix <| x * 86400000, y1 = y1, y2 = y2 }


view : Model -> Browser.Document Msg
view model =
    { title = "LineChart Example"
    , body =
        [ valueChart model
        , timeChart model
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
        , dataFrame = model
        , xAxisLabel = Nothing
        , yAxisLabel = Nothing
        , yMin = Just -5
        , yMax = Nothing
        }


timeChart : Model -> Html msg
timeChart model =
    LineChart.lineChart
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
        , dataFrame = model
        , xAxisLabel = Nothing
        , yAxisLabel = Nothing
        , yMin = Just -5
        , yMax = Nothing
        }
