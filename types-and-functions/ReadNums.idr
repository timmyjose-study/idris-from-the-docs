module ReadNums

import Data.Strings

%default total

readNumber : HasIO io => io (Maybe Nat)
readNumber = do n <- getLine
                if all isDigit (unpack n) 
                   then pure (Just (stringToNatOrZ n))
                   else pure Nothing

readNumbers : HasIO io => io (Maybe (Nat, Nat))
readNumbers = do n <- readNumber
                 case n of
                      Nothing => pure Nothing
                      Just nn => do m <- readNumber
                                    case m of 
                                         Nothing => pure Nothing
                                         Just mm => pure $ Just (nn, mm)

readNums : HasIO io => io (Maybe (Nat, Nat))
readNums = do Just n <- readNumber | Nothing => pure Nothing
              Just m <- readNumber | Nothing => pure Nothing
              pure $ Just (n, m)