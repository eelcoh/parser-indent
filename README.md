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

eol : Parser ()
eol =
    chompUntilEndOr "\n"

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
        |= Indent.list (lazy (\_ -> tree))

eol : Parser ()
eol =
    chompUntilEndOr "\n"
```
