module ExplicitNamespaces 

namespace X
  export 
  test : Int -> Int
  test x = x * 2

namespace Y
  export 
  test : String -> String
  test s = "Hello, " ++ s
  