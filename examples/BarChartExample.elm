module BarChartExample exposing (main)

import Browser
import DataFrame exposing (DataFrame)


type alias Model =
    { df : DataFrame DataPoint
    }


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
