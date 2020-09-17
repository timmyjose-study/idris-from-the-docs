module ReadNums

import Data.Strings

%default total

readNum : IO (Maybe Nat)
readNum = do ns <- getLine
             if all isDigit (unpack ns) 
                then pure $ Just (stringToNatOrZ ns)
                else pure Nothing

-- pattern matching bind for the win!
readNumbers : IO (Maybe (Nat, Nat))
readNumbers = do Just x <- readNum | Nothing => pure Nothing
                 Just y <- readNum | Nothing => pure Nothing
                 pure $ Just (x, y)

readNumbersVerbose : IO (Maybe (Nat, Nat))
readNumbersVerbose = do mx <- readNum
                        case mx of
                             Nothing => pure Nothing
                             Just x => do my <- readNum
                                          case my of
                                               Nothing => pure Nothing
                                               Just y => pure $ Just (x, y)