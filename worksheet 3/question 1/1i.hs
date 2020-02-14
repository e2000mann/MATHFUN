import Prelude hiding ((&&))

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
False && False = False
True && False = False
False && True = False
True && True = True
