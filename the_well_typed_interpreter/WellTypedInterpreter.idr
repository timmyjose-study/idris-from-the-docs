module Main

import Data.Vect

%default total

data Ty = TyBool | TyInt | TyFun Ty Ty

interpTy : Ty -> Type
interpTy TyBool = Bool
interpTy TyInt = Integer
interpTy (TyFun a b) = interpTy a -> interpTy b

data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
  Stop : HasType FZ (t :: ctx) t
  Pop : HasType k ctx t -> HasType (FS k) (u :: ctx) t

data Expr : Vect n Ty -> Ty -> Type where
  Var : HasType i ctx t -> Expr ctx t
  Val : (x : Integer) -> Expr ctx TyInt
  Lam : Expr (a :: ctx) t -> Expr ctx (TyFun a t)
  App : Expr ctx (TyFun a t) -> Expr ctx a -> Expr ctx t
  Op : (interpTy a -> interpTy b -> interpTy c) -> Expr ctx a -> Expr ctx b -> Expr ctx c
  If : Expr ctx TyBool -> Lazy (Expr ctx a) -> Lazy (Expr ctx a) -> Expr ctx a

data Env : Vect n Ty -> Type where
  Nil : Env Nil
  (::) : interpTy a -> Env ctx -> Env (a :: ctx)

lookup : HasType i ctx t -> Env ctx -> interpTy t
lookup Stop (x :: xs) = x
lookup (Pop k) (x :: xs) = lookup k xs

interp : Env ctx -> Expr ctx t -> interpTy t
interp env (Var i) = lookup i env
interp env (Val n) = n
interp env (Lam sc) = \x => interp (x :: env) sc
interp env (App f s) = interp env f (interp env s)
interp env (Op op x y) = op (interp env x) (interp env y)
interp env (If x t e) = if interp env x then interp env t 
                                        else interp env e

add : Expr ctx (TyFun TyInt (TyFun TyInt TyInt))
add = Lam (Lam (Op (+) (Var Stop) (Var (Pop Stop))))

partial
fac : Expr ctx (TyFun TyInt TyInt)
fac = Lam (If (Op (==) (Var Stop) (Val 0))
              (Val 1)
              (Op (*) (App fac (Op (-) (Var Stop) (Val 1)))
                      (Var Stop)))

partial
main : IO ()
main = do putStr "Enter a number: "
          x <- getLine
          printLn (interp [] fac (cast x))
