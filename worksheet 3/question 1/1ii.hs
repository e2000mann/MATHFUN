import Prelude hiding ((&&))

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

-- includes wildcard pattern
