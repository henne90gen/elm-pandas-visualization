module DataFrame exposing
    ( DataFrame, dataFrameDecoder
    , map, filter, length
    , paddingX, paddingY
    )

{-| Elm representation of a pandas DataFrame.

@docs DataFrame, dataFrameDecoder


# Helper Methods

@docs map, filter, length


# Default paddings

@docs paddingX, paddingY

-}

import Json.Decode exposing (Decoder, field, list, map2, map3, string)


{-| The DataFrame type containing the schema of the data and the data itself
-}
type alias DataFrame a =
    { schema : Schema
    , data : List a
    }


type alias Schema =
    { field : List Field
    , pandasVersion : String
    , primaryKey : List String
    }


type alias Field =
    { name : String
    , type_ : String
    }


{-| Decodes a JSON serialized DataFrame
-}
dataFrameDecoder : Decoder a -> Decoder (DataFrame a)
dataFrameDecoder dataDecoder =
    map2 DataFrame (field "schema" schemaDecoder) (field "data" (list dataDecoder))


schemaDecoder : Decoder Schema
schemaDecoder =
    map3 Schema (field "fields" fieldsDecoder) (field "pandas_version" string) (field "primaryKey" (list string))


fieldsDecoder : Decoder (List Field)
fieldsDecoder =
    list (map2 Field (field "name" string) (field "type" string))


{-| Default horizontal padding
-}
paddingX : Float
paddingX =
    30


{-| Default vertical padding
-}
paddingY : Float
paddingY =
    20


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
