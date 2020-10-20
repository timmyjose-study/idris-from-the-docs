-- using Idris as a library to install new backends
-- build with: $ idris2 -p idris2 -p contrib -p network -o lazy-idris2 Lazy.idr
-- run with: $ ./build/exec/lazy-idris2 --cg lazy -o <output-exec> <File.idr>

module Main

import Core.Context
import Compiler.Common
import Idris.Driver -- mainWithCodegens

compile : Ref Ctxt Defs -> (tmpDir : String) -> (outputDir : String) -> ClosedTerm -> (outfile : String) -> Core (Maybe String)
compile defs tmpDir outputDir term file = do coreLift $ putStrLn "I'd rather not."
                                             pure Nothing

execute : Ref Ctxt Defs -> (tmpDir : String) -> ClosedTerm -> Core ()
execute defs tmpDir term = do coreLift $ putStrLn "Maybe in an hour!"

lazyCodegen : Codegen
lazyCodegen = MkCG compile execute

main : IO ()
main = mainWithCodegens [("lazy", lazyCodegen)]