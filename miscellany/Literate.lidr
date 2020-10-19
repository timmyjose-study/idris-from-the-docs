> module Literate

This is a literate program in Idris. The file extension is `.lidr`, and the actual code is in lines starting with a `>`. A blank line is also expected between a line of
comment and a line of code.

Here is a simple "Hello, world" program:

> main : HasIO io => io ()
> main = putStrLn "Hello, world!"

And that is the end of the demo!