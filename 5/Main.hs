module Main where

eval_ :: Int -> [Int] -> Int
eval_ x = ev 0 where 
  ev i coeffs = 
    case coeffs of
      [] -> 0
      (c:cs) -> c*x^i + ev (i+1) cs

eval' :: (Num a, Foldable t) => a -> t a -> a
eval' x = foldr1 f where f a c = a + x*c

p = [10, 2, -7, 5]

q = [1, -8, 3, 4, 2]

r = [5]

main :: IO ()
main = do
  print (eval_ 4 p)
  print (eval' 4 p)
  print (eval_ 8 p)
  print (eval' 8 p)
  print (eval_ 9 p)
  print (eval' 9 p)
  print (eval_ 2 q)
  print (eval' 2 q)
  print (eval_ 1 q)
  print (eval' 1 q)
  print (eval_ 0 q)
  print (eval' 0 q)
  print (eval_ 8 r)
  print (eval' 8 r)
