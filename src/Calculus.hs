module Calculus where


import Data.Text (Text)




import Expr
import Utility



differentiate :: Expr -> Text -> Expr
differentiate expr x = simplify (diff (simplify expr) x)



-- TODO: teste denne sjiten
diff :: Expr -> Text -> Expr
diff (Num n) _ = Num 0                                                                                              -- a' = 0
diff (Var v) x | v == x = Num 1                                                                                     -- d/dx x = 1
               | otherwise = Num 0                                                                                  -- d/dx y = 0
diff (Add a b) x = Add (diff a x) (diff b x)                                                                        -- (a+b)' = a' + b'
diff (USub a) x = USub (diff a x)                                                                                   -- (-a)' = -(a')
diff (BSub a b) x = BSub (diff a x) (diff b x)                                                                      -- (a-b)' = a' - b'
diff (Mult a b) x = Add (Mult (diff a x) b) (Mult a (diff b x))                                                     -- (a*b)' = a'*b + a*b'
diff (Div a b) x = Div (BSub (Mult (diff a x) b) (Mult a (diff b x))) (Expo b (Num 2))                              -- (a/b)' = (a'*b - a*b')/(b^2)
diff (Expo f g) x = Mult (Expo f g) (Add (Div (Mult (diff f x) g) (diff f x)) (Mult (diff g x) (Log (Num e) f)))    -- (f^g)' = (f^g)*(f'*g/f + g'*ln(f))
diff (Log f g) x | f == Num e = Div (diff g x) g                                                                    -- (ln(f))' = (ln(f))' / f
                 | otherwise  = Div (BSub (Mult (Div (diff g x) g) (Log (Num e) f)) (Mult (Log (Num e) g) (Div (diff f x) f))) (Expo (Log (Num e) f) (Num 2))   
                                                                                                                    -- logf(g)' = (g'*ln(f)/g - ln(g)*f'/f) / (ln(f))^2



-- TODOOOO
simplify :: Expr -> Expr
simplify = id

{-
diff :: Ex -> Ex
diff (N _)   = N 0                                                          -- a' = 0
diff  X      = (N 1)                                                        -- x' = 1
diff (S f)   = S (diff f)                                                   -- (-f)' = -f'
diff (A f g) = A (diff f) (diff g)                                          -- (f + g)' = f' + g'
diff (M f g) = A (M f (diff g)) (M (diff f) g)                              -- (fg)' = f'g + fg'
diff (D f g) = D (A (M (diff f) g) (S (M f (diff g)))) (E g (N 2))          -- (f/g)' = (f'g - fg')/(g^2)
diff (E f g) = M (E f g) (A (M (diff f) (D g f)) (M (diff g) (L (N e) f)))  -- (f^g)' = f^g(f'(g/f) + g'ln(f))
diff (L f g) = D (A (M (diff g) (L (N e) f)) (S (M (diff f) (L (N e) g)))) (M X (E (L (N e) f) (N 2)))  -- logf(g)' = (g'lnf - f'lng)/(xln^2f)

-}