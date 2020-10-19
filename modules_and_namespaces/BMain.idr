module Main 

import BTree

main : HasIO io => io ()
main =  do let t = toTree [1, 8, 2, 7, 9, 3]
           printLn $ BTree.toList t