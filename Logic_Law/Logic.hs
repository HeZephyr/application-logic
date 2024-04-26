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
distribute (Or p (And q r)) = And (Or (distribute p) (distribute q)) (Or (distribute p) (distribute r))
distribute (Or (And p q) r) = And (Or (distribute p) (distribute r)) (Or (distribute q) (distribute r))
distribute (And p q) = And (distribute p) (distribute q)
distribute (Or p q) = Or (distribute p) (distribute q)
distribute (Not p) = Not (distribute p)
distribute (Imply p q) = Imply (distribute p) (distribute q)
distribute (Equiv p q) = Equiv (distribute p) (distribute q)
distribute p = p  -- No distribution needed for other cases

-- deMorgan function applying De Morgan's laws
deMorgan :: Logic -> Logic
deMorgan (Not (Or p q)) = And (Not (deMorgan p)) (Not (deMorgan q))
deMorgan (Not (And p q)) = Or (Not (deMorgan p)) (Not (deMorgan q))
deMorgan (And p q) = And (deMorgan p) (deMorgan q)
deMorgan (Or p q) = Or (deMorgan p) (deMorgan q)
deMorgan (Not p) = Not (deMorgan p)
deMorgan (Imply p q) = Imply (deMorgan p) (deMorgan q)
deMorgan (Equiv p q) = Equiv (deMorgan p) (deMorgan q)
deMorgan p = p  -- No De Morgan's law applies to other cases
