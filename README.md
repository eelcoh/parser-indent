A parser for parsing indented lists.

## Example - parsing a list:

```
List:
  Item 1
  Item 2
  Item 3
  Item 4
  Item 5
```

With the following parsers:

```elm

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

```

## Example - parsing a tree with lists:

```
Node
  Node
    Leaf 1
    Leaf 2
  Leaf 3
```

With the following parsers:


```elm

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
```
