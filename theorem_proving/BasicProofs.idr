module BasicProofs

%default total

{-
  data Equal : a -> b -> Type where
    Refl : Equal x x
-}

proofNotTrue : (not True) = False
proofNotTrue = Refl

proofNotFalse : (not False) = True
proofNotFalse = Refl

proofOnePlusOne : 1 + 1 = 2
proofOnePlusOne = Refl

proofNotInvolutive : (x : Bool) -> not (not x) = x
proofNotInvolutive False = Refl
proofNotInvolutive True = Refl

plusZeroRightNeutral : (left : Nat) -> left + 0 = left
plusZeroRightNeutral Z = Refl
plusZeroRightNeutral (S k) =
  let inductiveHypothesis = plusZeroRightNeutral k in
      rewrite inductiveHypothesis in Refl

varEquality : m = m
varEquality = Refl
