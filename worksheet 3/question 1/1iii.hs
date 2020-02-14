import Prelude hiding ((&&))

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
False && _ = False
True && p = p

-- Wildcard and named parameter (p)
