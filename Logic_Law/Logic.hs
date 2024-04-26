module Logic where

data Logic = A | B | C
  | And Logic Logic
  | Or Logic Logic
  | Not Logic
  | Imply Logic Logic
  | Equiv Logic Logic
  deriving (Eq, Show)

-- Distribute function applying the distributive law
distribute :: Logic -> Logic
distribute (And a (Or b c)) = Or (And a b) (And a c)
distribute (Or a (And b c)) = And (Or a b) (Or a c)
distribute (And a b) = And (distribute a) (distribute b)
distribute (Or a b) = Or (distribute a) (distribute b)
distribute x = x  -- No distribution needed for other cases

-- deMorgan function applying De Morgan's laws
deMorgan :: Logic -> Logic
deMorgan (Not (And a b)) = Or (Not a) (Not b)
deMorgan (Not (Or a b)) = And (Not a) (Not b)
deMorgan (Not x) = Not (deMorgan x)
deMorgan (And a b) = And (deMorgan a) (deMorgan b)
deMorgan (Or a b) = Or (deMorgan a) (deMorgan b)
deMorgan x = x  -- No De Morgan's law applies to other cases
