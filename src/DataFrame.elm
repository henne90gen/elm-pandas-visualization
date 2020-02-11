module DataFrame exposing
    ( DataFrame, create, dataFrameDecoder
    , map, filter, length
    , XValueMapper(..), YValueMapper
    )

{-| Elm representation of a pandas DataFrame.

@docs DataFrame, create, dataFrameDecoder, XValueMapper, YValueMapper


# Helper Methods

@docs map, filter, length

-}

import Json.Decode exposing (Decoder, field, list, map2, map3, string)
import Time exposing (Posix)


{-| The DataFrame type containing the schema of the data and the data itself
-}
type alias DataFrame a =
    { schema : Maybe Schema
    , data : List a
    }


type alias Schema =
    { fields : List Field
    , pandasVersion : String
    , primaryKey : List String
    }


type alias Field =
    { name : String
    , type_ : String
    }

{-| Decides how the values are mapped to the x-axis
-}
type XValueMapper a
    = TimeMapper (a -> Posix)
    | ValueMapper (a -> Float)

{-| Decides how the values are mapped to the y-axis
-}
type alias YValueMapper a =
    a -> Float


{-| Creates a DataFrame from the supplied data
-}
create : List a -> DataFrame a
create data =
    { data = data, schema = Nothing }


{-| Decodes a JSON serialized DataFrame
-}
dataFrameDecoder : Decoder a -> Decoder (DataFrame a)
dataFrameDecoder dataDecoder =
    map2 DataFrame (field "schema" schemaDecoder) (field "data" (list dataDecoder))


schemaDecoder : Decoder (Maybe Schema)
schemaDecoder =
    Json.Decode.map Just
        (map3 Schema
            (field "fields" fieldsDecoder)
            (field "pandas_version" string)
            (field "primaryKey" (list string))
        )


fieldsDecoder : Decoder (List Field)
fieldsDecoder =
    list (map2 Field (field "name" string) (field "type" string))


{-| Maps a function over each row of a DataFrame
-}
map : (a -> b) -> DataFrame a -> DataFrame b
map func df =
    DataFrame df.schema (List.map func df.data)


{-| Filter rows by the specified filter function
-}
filter : (a -> Bool) -> DataFrame a -> DataFrame a
filter func df =
    DataFrame df.schema (List.filter func df.data)


{-| Gives back the number of rows in the DataFrame
-}
length : DataFrame a -> Int
length df =
    List.length df.data
