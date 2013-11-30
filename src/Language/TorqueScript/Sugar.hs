module Language.TorqueScript.Sugar where

import Language.TorqueScript.AST

{- Construct top-level definitions and top-level statements. -}
tld def = Right def
tls stm = Left stm

(:=), (:+=), (:-=), (:*=), (:/=), (:&=), (:|=), (:^=), (:<<=), (:>>=) :: Expression -> Expression -> Expression
a := b   = Assign a Equals b
a :+= b  = Assign a PlusEquals b
a :-= b  = Assign a MinusEquals b
a :*= b  = Assign a TimesEquals b
a :/= b  = Assign a DivEquals b
a :&= b  = Assign a AndEquals b
a :|= b  = Assign a OrEquals b
a :^= b  = Assign a XOrEquals b
a :<<= b = Assign a LeftShiftEquals b
a :>>= b = Assign a RightShiftEquals b

(?==), (?!=), (?$=), (?!$=), (?<, (?>, (?<=), (?>=) :: Expression -> Expression -> Expression
a ?== b  = Binary a IsEqual b
a ?!= b  = Binary a IsNotEqual b
a ?$= b  = Binary a IsStringEqual b
a ?!$= b = Binary a IsNotStringEqual b
a ?> b   = Binary a GreaterThan b
a ?< b   = Binary a LessThan b
a ?>= b  = Binary a GreaterThanEqual b
a ?<= b  = Binary a LessThanEqual b

(@), cat, spc, nl, tab :: Expression -> Expression -> Expression
a @ b = Binary a Cat b
a `cat` b = Binary a Cat b
a `spc` b = Binary a Space b
a `nl` b = Binary a Line b
a `tab` b = Binary a Tab b

postInc, postDec :: Expression -> Expression
postInc = Post Increment
postDec = Post Decrement
