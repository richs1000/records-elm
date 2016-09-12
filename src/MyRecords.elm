module MyRecords exposing (..)

import RandomStuff exposing (..)


type RecordValue
    = RandomInt Int
    | RandomString String
    | RandomBool Bool


type alias RecordKey =
    String


type alias RecordItem =
    ( RecordKey, RecordValue )


type alias RandomRecord =
    List RecordItem


recordItemToTypeString : RecordItem -> String
recordItemToTypeString ( _, v ) =
    case v of
        RandomInt _ ->
            "int"

        RandomString _ ->
            "string"

        RandomBool _ ->
            "bool"


recordItemToWrongTypeString : RecordItem -> String
recordItemToWrongTypeString ( _, v ) =
    case v of
        RandomInt _ ->
            "string"

        RandomString _ ->
            "bool"

        RandomBool _ ->
            "int"


recordValueToString : RecordItem -> String
recordValueToString ( _, v ) =
    case v of
        RandomInt i ->
            toString i

        RandomString s ->
            toString s

        RandomBool b ->
            toString b


recordKeyToString : RecordItem -> String
recordKeyToString ( k, v ) =
    k


recordItemToString : RecordItem -> String
recordItemToString r =
    (recordKeyToString r) ++ " : " ++ (recordValueToString r)


recordTo : (RecordItem -> String) -> String -> String -> RandomRecord -> String
recordTo recItemTo leftDelimeter rightDelimeter rec =
    let
        helperFunc rec' =
            case rec' of
                [] ->
                    ""

                r :: [] ->
                    (recItemTo r)

                r :: rs ->
                    (recItemTo r) ++ ", " ++ (helperFunc rs)
    in
        leftDelimeter ++ (helperFunc rec) ++ rightDelimeter


recordToWrongTypeString : RandomRecord -> String
recordToWrongTypeString rec =
    recordTo recordItemToWrongTypeString "{" "}" rec


recordToTypeString : RandomRecord -> String
recordToTypeString rec =
    recordTo recordItemToTypeString "{" "}" rec


recordToString : RandomRecord -> String
recordToString rec =
    recordTo recordItemToString "{" "}" rec


randomRecordItem : List Int -> RecordItem
randomRecordItem randomValues =
    let
        randStrings =
            [ "dog", "cat", "pig", "moose", "cow", "bird", "red", "green", "blue", "white", "black" ]

        rType =
            pickOne randomValues [ 1, 2, 3 ] 1

        rKey =
            pickOne (List.drop 1 randomValues) randStrings "foo"
    in
        case rType of
            1 ->
                let
                    rVal =
                        pickOne (List.drop 2 randomValues) [ 1, 2 ] 1
                in
                    if rVal == 1 then
                        ( rKey, RandomBool True )
                    else
                        ( rKey, RandomBool False )

            2 ->
                let
                    rVal =
                        pickOne (List.drop 2 randomValues) [0..9] 1
                in
                    ( rKey, RandomInt rVal )

            _ ->
                let
                    rVal =
                        pickOne (List.drop 2 randomValues) randStrings "cat"
                in
                    ( rKey, RandomString rVal )


randomRecordValues : List Int -> Int -> List RecordValue
randomRecordValues randomValues cnt =
    if cnt == 0 then
        []
    else
        (randomRecordValue randomValues) :: (randomRecordValues (List.drop 2 randomValues) (cnt - 1))


randomRecordValue : List Int -> RecordValue
randomRecordValue randomValues =
    let
        randStrings =
            [ "dog", "cat", "pig", "moose", "cow", "bird", "red", "green", "blue", "white", "black" ]

        rType =
            pickOne randomValues [ 1, 2, 3 ] 1
    in
        case rType of
            1 ->
                let
                    rVal =
                        pickOne (List.drop 1 randomValues) [ 1, 2 ] 1
                in
                    if rVal == 1 then
                        RandomBool True
                    else
                        RandomBool False

            2 ->
                let
                    rVal =
                        pickOne (List.drop 1 randomValues) [0..9] 1
                in
                    RandomInt rVal

            _ ->
                let
                    rVal =
                        pickOne (List.drop 1 randomValues) randStrings "cat"
                in
                    RandomString rVal


randomRecord : List Int -> RandomRecord
randomRecord randomValues =
    -- let
    --     recordLen =
    --         pickOne randomValues lengths defVal
    --
    --     helperFunc cnt =
    --         if cnt >= recordLen then
    --             []
    --         else
    --             (randomRecordItem (List.drop (3 * cnt + 1) randomValues)) :: (helperFunc (cnt + 1))
    -- in
    --     helperFunc 0
    let
        randStrings =
            [ "dog", "cat", "pig", "moose", "cow", "bird", "red", "green", "blue", "white", "black" ]

        recordLen =
            pickOne randomValues [ 2, 3, 4 ] 3

        keys =
            pickABunch (List.drop 1 randomValues) recordLen randStrings "foo"
                |> compressList

        values =
            randomRecordValues (List.drop (recordLen + 1) randomValues) (List.length keys)
    in
        List.map2 (,) keys values


extractItemFromRecord : RandomRecord -> String -> Maybe RecordItem
extractItemFromRecord rec key =
    -- Maybe.withDefault ( "foo", RandomString "foo" ) (List.head (List.drop (index - 1) rec))
    case rec of
        [] ->
            Nothing

        ( k, v ) :: rs ->
            if k == key then
                Just ( k, v )
            else
                extractItemFromRecord rs key


extractValueFromRecord : RandomRecord -> String -> Maybe RecordValue
extractValueFromRecord rec key =
    -- Maybe.withDefault ( "foo", RandomString "foo" ) (List.head (List.drop (index - 1) rec))
    case rec of
        [] ->
            Nothing

        ( k, v ) :: rs ->
            if k == key then
                Just v
            else
                extractValueFromRecord rs key


recordKeys : RandomRecord -> List String
recordKeys rec =
    case rec of
        [] ->
            []

        r :: rs ->
            (recordKeyToString r) :: (recordKeys rs)


recordValues : RandomRecord -> List String
recordValues rec =
    case rec of
        [] ->
            []

        r :: rs ->
            (recordValueToString r) :: (recordValues rs)
