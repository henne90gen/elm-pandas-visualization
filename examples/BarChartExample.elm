module BarChartExample exposing (main)

import BarChart
import Browser
import DataFrame exposing (DataFrame)


type alias Model =
    DataFrame DataPoint


type alias DataPoint =
    { weekday : Int
    , value : Float
    }


type Msg
    = Default


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( DataFrame.create exampleData, Cmd.none )


exampleData : List DataPoint
exampleData =
    [ dataPoint 0 10
    , dataPoint 1 15
    , dataPoint 2 12
    , dataPoint 3 9
    , dataPoint 4 13
    , dataPoint 5 17
    , dataPoint 6 11
    ]


dataPoint : Int -> Float -> DataPoint
dataPoint weekday value =
    { weekday = weekday, value = value }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "BarChart Example"
    , body =
        [ BarChart.singleBarChart ( 600, 300 )
            (\e -> String.fromInt e.weekday)
            (\e -> e.value)
            model
        ]
    }
