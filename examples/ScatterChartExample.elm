module ScatterChartExample exposing (main)

import Browser
import DataFrame exposing (DataFrame)
import ScatterChart


type alias Model =
    DataFrame DataPoint


type alias DataPoint =
    { x : Float
    , y : Float
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
    [ dataPoint 0 10
    , dataPoint 1 15
    , dataPoint 2 13
    , dataPoint 3 9
    , dataPoint 4 11
    , dataPoint 5 17
    , dataPoint 6 12
    ]


dataPoint : Float -> Float -> DataPoint
dataPoint x y =
    { x = x, y = y }


view : Model -> Browser.Document Msg
view model =
    { title = "ScatterChart Example"
    , body =
        [ ScatterChart.singleScatterChart ( 600, 300 )
            (DataFrame.ValueMapper (\e -> e.x))
            (\e -> e.y)
            model
        ]
    }
