module Question exposing (..)

import MyRecords exposing (..)
import RandomStuff exposing (pickOne, pickABunch, compressList)


-- import Debug exposing (..)


type QuestionFormat
    = FillInTheBlank
    | MultipleChoice


type alias Question =
    { question : List String
    , distractors : List ResponseAndFeedback
    , answer : ResponseAndFeedback
    , format : QuestionFormat
    }


type alias ResponseAndFeedback =
    ( String, String )


emptyQuestion : Question
emptyQuestion =
    { question = []
    , distractors = []
    , answer = ( "", "" )
    , format = FillInTheBlank
    }


newQuestion : List Int -> Int -> Question
newQuestion randomValues index =
    let
        rec =
            randomRecord randomValues

        randKey =
            pickOne (List.drop 20 randomValues) (recordKeys rec) "foo"

        allKeys =
            recordKeys rec

        strExpr =
            "#" ++ randKey ++ " e"

        valueDistractors =
            List.map
                (\k -> recordValueToString (Maybe.withDefault ( "foo", RandomString "foo" ) (extractItemFromRecord rec k)))
                allKeys

        typeDistractors =
            List.map recordItemToTypeString rec
    in
        -- extract value
        if index == 1 then
            let
                question' =
                    [ "What is the value of ans after the following ML expressions are evaluated?"
                    , ""
                    , "val e = " ++ (recordToString rec)
                    , "val ans = " ++ strExpr
                    , ""
                    ]

                answer' =
                    (extractItemFromRecord rec randKey)
                        |> Maybe.withDefault ( "foo", RandomString "foo" )
                        |> recordValueToString

                ( _, distractors' ) =
                    List.partition (\d -> d == answer') (compressList (List.append valueDistractors typeDistractors))

                -- d =
                --     Debug.log "partitioned " distractors'
            in
                { question = question'
                , distractors = List.map (\dis -> ( dis, "Incorrect." )) distractors'
                , answer = ( answer', "Correct" )
                , format = MultipleChoice
                }
            -- what is the type of?
        else
            let
                question' =
                    [ "What is the type of ans after the following ML expressions are evaluated?"
                    , ""
                    , "val e = " ++ (recordToString rec)
                    , "val ans = " ++ strExpr
                    , ""
                    ]

                answer' =
                    extractItemFromRecord rec randKey
                        |> Maybe.withDefault ( "foo", RandomString "foo" )
                        |> recordItemToTypeString

                ( _, distractors' ) =
                    List.partition (\d -> d == answer') (compressList (List.append valueDistractors typeDistractors))
            in
                { question = question'
                , distractors = List.map (\dis -> ( dis, "Incorrect." )) distractors'
                , answer = ( answer', "Correct" )
                , format = MultipleChoice
                }


findFeedback : String -> String -> List ResponseAndFeedback -> String
findFeedback answer response distractors =
    case distractors of
        [] ->
            "Incorrect. The answer is " ++ answer

        d :: ds ->
            if ((fst d) == response || ((fst d) == "")) then
                (snd d) ++ " The answer is " ++ answer
            else
                findFeedback answer response ds
