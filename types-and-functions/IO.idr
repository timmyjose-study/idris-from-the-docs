module IO

import System.File

%default total

{-
  data File -- abstract
  data Mode = Read | Write | ReadWrite

  openFile : (f : String) -> (m : Mode) -> IO (Either FileError File)
  closeFile : File -> IO ()

  fGetLine : (h : File) -> IO (Either FileError String)
  fPutStr : (h : File) -> (str : String) -> IO (Either FileError ())
  fEOF : File -> IO Bool
-}

partial
printFile : (filename : String) -> IO ()
printFile filename = 
  do Right handle <- openFile filename Read | Left error => putStrLn $ "Error while opening file: " ++ show error ++ "\n"
     printLines handle
     closeFile handle
  where
    partial
    printLines : File -> IO ()
    printLines handle = do f <- fEOF handle
                           if f then pure ()
                                else do Right line <- fGetLine handle | Left error => do putStrLn (show error) ; pure ()
                                        printLn line
                                        printLines handle

greet : IO ()
greet = do putStr "Enter your name: "
           name <- getLine
           putStrLn $ "Nice to meet you, " ++ name