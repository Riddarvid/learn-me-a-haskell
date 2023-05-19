
## Laziness

IO is by default lazy in Haskell. This means that if you're reading a file, the entire contents of the file won't be loaded into memory.

## Handles

A file handle is some sort of reference to a file, also containing information of "where" in the file we currently are reading/writing, and probably a lot more.

## Buffering

So we know that IO is lazy. So how is something like hGetContents implemented then? How often do we read from disk and how much data?

There ar two modes for files, binary and text.

The standard for text files is line-buffering, that is reading an entire line every time a character that we havn't yet read is requested. THis means that something like

```putStr <*> readFile "path"```

will read the lines of the file and print them one by one. Thus, the maximum amount of memory used is dependent on the length of the longest line, not the length of the entire file.

The standard for binary files is instead block buffering. Files are read in chunks determined by the OS.

It is possible to set buffering yourself with the modes 

* NoBuffering - one character at a time.
* LineBuffering - one line at a time.
* BlockBuffering (Maybe Int)
	* Just n - n bytes at a time.
	* Nothing - OS determines chunk size.