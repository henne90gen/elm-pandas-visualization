module DataFrame exposing
    ( DataFrame
    , dataFrameDecoder
    , filter
    , length
    , map
    , paddingX
    , paddingY
    )

import Json.Decode exposing (Decoder, field, list, map2, map3, string)


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


dataFrameDecoder : Decoder a -> Decoder (DataFrame a)
dataFrameDecoder dataDecoder =
    map2 DataFrame (field "schema" schemaDecoder) (field "data" (list dataDecoder))


schemaDecoder : Decoder Schema
schemaDecoder =
    map3 Schema (field "fields" fieldsDecoder) (field "pandas_version" string) (field "primaryKey" (list string))


fieldsDecoder : Decoder (List Field)
fieldsDecoder =
    list (map2 Field (field "name" string) (field "type" string))


paddingX : Float
paddingX =
    30


paddingY : Float
paddingY =
    20


map : (a -> b) -> DataFrame a -> DataFrame b
map func df =
    DataFrame df.schema (List.map func df.data)


filter : (a -> Bool) -> DataFrame a -> DataFrame a
filter func df =
    DataFrame df.schema (List.filter func df.data)


length : DataFrame a -> Int
length df =
    List.length df.data
