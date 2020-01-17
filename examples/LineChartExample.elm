module LineChartExample exposing (main)

import Browser
import DataFrame exposing (DataFrame)
import LineChart


type alias Model =
    DataFrame DataPoint


type alias DataPoint =
    { x : Float, y : Float }


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
    [ dataPoint 0 10
    , dataPoint 1 150
    , dataPoint 2 12
    , dataPoint 3 9
    , dataPoint 4 13
    , dataPoint 5 17
    , dataPoint 60 11
    ]


dataPoint : Float -> Float -> DataPoint
dataPoint x y =
    { x = x, y = y }


view : Model -> Browser.Document Msg
view model =
    { title = "LineChart Example"
    , body =
        [ LineChart.singleLineChart ( 600, 300 )
            (LineChart.ValueMapper (\e -> e.x))
            (\e -> e.y)
            model
        ]
    }
