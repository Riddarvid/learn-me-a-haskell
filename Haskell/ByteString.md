
Two varieties, lazy and strict.

### Strict

In strict ByteStrings, if you evaluate the first byte of the string, then the whole ByteString gets evaluated.

### Lazy

In lazy ByteStrings, if you evaluate the first byte of the string, then the next 64K bytes get evaluated, but no more.

## Structure

(I don't know exactly how it's done, but this is my working model as of now.)

The ByteString is represented as:

``` data ByteString a = Chunk [a] (ByteString a) | Empty ```

where the list contained in a chunk has some maximum size.

## Functions

cons simply appends a new singleton chunk, while cons' first tries to append the element to the head chunk.

## Comments

In general, strict ByteStrings are faster, since there are very  few thunks. The downside is that you have to be able to fit the entire ByteString in memory.