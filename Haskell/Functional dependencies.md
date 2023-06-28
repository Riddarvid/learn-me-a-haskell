
https://wiki.haskell.org/Functional_dependencies

Useful when we have a multi-parameter type class and want to specify that one of the parameters depends on the other ones. For example, when multiplying two matrices we know that the result will also be a matrix, so instead of writing:

`class Mult a b c where`

we can write

`class Mult a b c | a b -> c where`

In both cases, the instance declaration will be

`instance Mult Matrix Matrix Matrix`

but in the latter case you cannot also define `instance Mult Matrix Matrix Vector`, since we have specified that the third parameter is uniquely determined by the first two.

TODO: 

https://wiki.haskell.org/GHC/Type_families
https://wiki.haskell.org/Functional_dependencies_vs._type_families
