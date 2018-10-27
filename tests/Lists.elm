module Lists exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P exposing ((|.), (|=), Parser)
import Parser.Indent as Indent
import Parser.Utils as U
import Test exposing (..)



-- Type


type Item
    = Item Int



-- Parsers


item =
    P.succeed Item
        |. P.keyword "Item"
        |. P.spaces
        |= P.int


list =
    P.succeed identity
        |. P.keyword "List"
        |. U.spaces
        |. P.symbol ":"
        |. U.spaces
        |. eol
        |= Indent.list item


eol : Parser ()
eol =
    P.chompUntilEndOr "\n"



-- tests


list0 =
    """List:
"""


list1 =
    """List:
    Item 1
    Item 2
    Item 3
    Item 4
    Item 5
"""


suite : Test
suite =
    describe "The Parser.Maybe module"
        [ describe "maybe"
            -- Nest as many descriptions as you like.
            [ test "empty list" <|
                \() ->
                    P.run list list0
                        |> Expect.equal (Ok [])
            , test "small list" <|
                \() ->
                    P.run list list1
                        |> Expect.equal (Ok [ Item 1, Item 2, Item 3, Item 4, Item 5 ])

            -- , test "maybe not an int" <|
            --     \() ->
            --         P.run (M.maybe P.int) "a"
            --             |> Expect.equal (Ok Nothing)
            -- , test "maybe not an int, so it is zero" <|
            --     \() ->
            --         P.run (M.withDefault 0 P.int) "a"
            --             |> Expect.equal (Ok 0)
            -- , test "maybe an int, 2" <|
            --     \() ->
            --         P.run (M.withDefault 0 P.int) "2"
            --             |> Expect.equal (Ok 2)
            -- , test "maybe a tuple!" <|
            --     \() ->
            --         P.run (M.maybe point) "2 2"
            --             |> Expect.equal (Ok (Just ( 2, 2 )))
            -- , test "maybe not a tuple!" <|
            --     \() ->
            --         P.run (M.maybe point) "2 a"
            --             |> Expect.equal (Ok Nothing)
            -- , test "maybe not a tuple, in which case we make it (0,0)!" <|
            --     \() ->
            --         P.run (M.withDefault ( 0, 0 ) point) "2 a"
            --             |> Expect.equal (Ok ( 0, 0 ))
            -- , test "maybe a tuple between two ints - nothing" <|
            --     \() ->
            --         P.run optionalPointBetweenTwoInts "23 23"
            --             |> Expect.equal (Ok Nothing)
            --
            -- -- , test "maybe a tuple between two ints - nothing again FAILS TEST" <|
            -- --     \() ->
            -- --         P.run optionalPointBetweenTwoInts "23 21 23"
            -- --             |> Expect.equal (Ok Nothing)
            -- , test "maybe an int between two ints - Just" <|
            --     \() ->
            --         P.run optionalIntBetweenTwoInts "23 21 23"
            --             |> Expect.equal (Ok (Just 21))
            -- , test "maybe an int between two ints - Nothing" <|
            --     \() ->
            --         P.run optionalIntBetweenTwoInts "23 21"
            --             |> Expect.equal (Ok Nothing)
            -- , test "maybe a tuple between two ints - just " <|
            --     \() ->
            --         P.run optionalPointBetweenTwoInts "23 21 21 23"
            --             |> Expect.equal (Ok (Just ( 21, 21 )))
            ]
        ]
