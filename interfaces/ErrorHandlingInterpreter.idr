module ErrorHandlingInterpreter 

%default total

data Expr = Var String
          | Val Int
          | Add Expr Expr

data Eval : Type -> Type where
  MkEval : (List (String, Int) -> Maybe a) -> Eval a

fetch : String -> Eval Int
fetch s = MkEval (\e => fetchVal e) where
  fetchVal : List (String, a) -> Maybe a 
  fetchVal [] = Nothing
  fetchVal ((v, val) :: xs) = if v == s 
                                 then (Just val)
                                 else (fetchVal xs)

Functor Eval where
  -- map : (a -> b) -> Eval a -> Eval b
  map f (MkEval g) = MkEval (\x => map f (g x))

Applicative Eval where
  -- pure : Eval a
  pure x = MkEval (\e => Just x)

  -- (<*>) : Eval (a -> b) -> Eval a -> Eval b
  (MkEval f) <*> (MkEval g) = MkEval (\x => app (f x) (g x)) where
    app : Maybe (a -> b) -> Maybe a -> Maybe b
    app (Just fx) (Just gx) = Just (fx gx)
    app _ _ = Nothing

{-
eval : Expr -> Eval Int
eval (Var x) = fetch x
eval (Val n) = pure n
eval (Add x y) = pure (+) <*> eval x <*> eval y
-}

eval : Expr -> Eval Int
eval (Var x) = fetch x
eval (Val n) = [| n |] 
eval (Add x y) = [| eval x + eval y |]

runEval : List (String, Int) -> Expr -> Maybe Int
runEval env e = case eval e of
                     MkEval envFn => envFn env
