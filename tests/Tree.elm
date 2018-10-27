module Tree exposing (Tree(..), leaf, suite, tree1)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P exposing ((|.), (|=), Parser)
import Parser.Indent as Indent
import Parser.Utils as U
import Test exposing (..)



-- Type


type Tree
    = Node (List Tree)
    | Leaf Int



-- Parsers


tree =
    P.oneOf
        [ node
        , leaf
        ]


leaf =
    P.succeed Leaf
        |. P.keyword "Leaf"
        |. P.spaces
        |= P.int


node =
    P.succeed Node
        |. P.keyword "Node"
        |. U.spaces
        |. eol
        |= Indent.list (P.lazy (\_ -> tree))


eol : Parser ()
eol =
    P.chompUntilEndOr "\n"



-- tests


tree1 =
    """Node
    Leaf 3
    Leaf 4
"""


tree2 =
    """Node
    Node
      Leaf 1
      Leaf 2
    Leaf 3
"""


suite : Test
suite =
    describe "The Parser.Maybe module"
        [ describe "maybe"
            -- Nest as many descriptions as you like.
            [ test "small tree" <|
                \() ->
                    P.run tree tree1
                        |> Expect.equal (Ok (Node [ Leaf 3, Leaf 4 ]))
            , test "tree" <|
                \() ->
                    P.run tree tree2
                        |> Expect.equal (Ok (Node [ Node [ Leaf 1, Leaf 2 ], Leaf 3 ]))

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
