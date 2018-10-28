module Lists exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P exposing ((|.), (|=), Parser, chompUntilEndOr, int, keyword, run, spaces, succeed, symbol)
import Parser.Indent as Indent
import Test exposing (..)



-- Type


type Item
    = Item Int



-- Parsers


item =
    succeed Item
        |. keyword "Item"
        |. spaces
        |= int


list =
    succeed identity
        |. keyword "List"
        |. spaces
        |. symbol ":"
        |. spaces
        |. eol
        |= Indent.list item


spaces =
    P.chompWhile (\c -> c == ' ')


eol : Parser ()
eol =
    chompUntilEndOr "\n"



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
    describe "The Parser.Indent module"
        [ describe "indent"
            [ test "empty list" <|
                \() ->
                    run list list0
                        |> Expect.equal (Ok [])
            , test "small list" <|
                \() ->
                    run list list1
                        |> Expect.equal (Ok [ Item 1, Item 2, Item 3, Item 4, Item 5 ])
            ]
        ]
