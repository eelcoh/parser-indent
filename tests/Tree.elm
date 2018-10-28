module Tree exposing (Tree(..), leaf, suite, tree1)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P exposing ((|.), (|=), Parser, chompUntilEndOr, int, keyword, oneOf, run, spaces, succeed)
import Parser.Indent as Indent
import Test exposing (..)



-- Type


type Tree
    = Node (List Tree)
    | Leaf Int



-- Parsers


tree =
    oneOf
        [ node
        , leaf
        ]


leaf =
    succeed Leaf
        |. keyword "Leaf"
        |. spaces
        |= int


node =
    succeed Node
        |. keyword "Node"
        |. spaces
        |. eol
        |= Indent.list (P.lazy (\_ -> tree))


spaces =
    P.chompWhile (\c -> c == ' ')


eol : Parser ()
eol =
    chompUntilEndOr "\n"



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
    Leaf 4
"""


suite : Test
suite =
    describe "The Parser.Indent module"
        [ describe "indent"
            [ test "small tree" <|
                \() ->
                    P.run tree tree1
                        |> Expect.equal (Ok (Node [ Leaf 3, Leaf 4 ]))
            , test "tree" <|
                \() ->
                    P.run tree tree2
                        |> Expect.equal (Ok (Node [ Node [ Leaf 1, Leaf 2, Leaf 3 ], Leaf 4 ]))
            ]
        ]
