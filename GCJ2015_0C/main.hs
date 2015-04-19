
data Sign = P | N deriving (Eq)

data Q = One
          | I
          | J
          | K
            deriving (Eq, Ord)
            
type Quat = (Q,Sign)

multSign :: Sign -> Sign -> Sign
multSign s1 s2
  | s1 == s2 = P
  | otherwise = N

multQSign :: Q -> Q -> Sign
multQSign q1 q2
  | q1 == q2 && q1 /= One = N
  | q1 > q2 = N
  | otherwise = P
                
multQ :: Q -> Q -> Q
multQ One x = x
multQ x One = x
multQ I J = K
multQ J K = I
multQ K I = J
multQ J I = K
multQ K J = I
multQ I K = J

mult :: Quat -> Quat -> Quat
mult (q1,s1) (q2,s2) 
  | q1 == q2 = (One, newSign)
  | otherwise = ((multQ q1 q2), (newSign))
  where newSign = multSign (multQSign q1 q2) (multSign s1 s2)
        
instance Show Sign where
  show P = "P"
  show N = "N"
  
instance Show Q where
  show One = "1"
  show I = "I"
  show J = "J"
  show K = "K"

solve :: [Quat] -> Int -> Bool
solve all@(x:y:xs) acc
  | x == (I,P) && acc == 0 = solve (y:xs) (acc + 1)
  | x == (J,P) && acc == 1 = solve (y:xs) (acc + 1)
  | x == (K,P) && acc == 2 = solve (y:xs) (acc + 1)
  | length all == 0 && acc == 3 = True
  | length all == 0 && acc /= 3 = False
  | otherwise = solve ((mult x y) : xs) acc
                
cq :: Char -> Quat
cq a 
  | a == 'i' = (I,P)
  | a == 'j' = (J,P)
  | a == 'k' = (K,P)
  | otherwise = error "Unknown char"

cqa = map cq
