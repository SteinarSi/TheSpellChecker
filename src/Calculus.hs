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
diff (BFunc (Infix Add) a b) x = BFunc (Infix Add) (diff a x) (diff b x)                                                                        -- (a+b)' = a' + b'
diff (UFunc USub a) x = UFunc USub (diff a x)                                                                                   -- (-a)' = -(a')
diff (BFunc (Infix BSub) a b) x = BFunc (Infix BSub) (diff a x) (diff b x)       -- (a-b)' = a' - b'
diff (UFunc Sin a) x = BFunc (Infix Mult) (UFunc Cos a) (diff a x)
diff (UFunc Cos a) x = BFunc (Infix Mult) (UFunc USub (UFunc Sin a)) (diff a x)
diff (UFunc Tan a) x = BFunc (Infix Div) (Num 1) (BFunc (Infix Expo) (UFunc Cos a) (Num 2))
diff (BFunc (Infix Mult) a b) x = BFunc (Infix Add) (BFunc (Infix Mult) (diff a x) b) (BFunc (Infix Mult) a (diff b x))                                                     -- (a*b)' = a'*b + a*b'
diff (BFunc (Infix Div) a b) x = BFunc (Infix Div) (BFunc (Infix BSub) (BFunc (Infix Mult) (diff a x) b) (BFunc (Infix Mult) a (diff b x))) (BFunc (Infix Expo) b (Num 2))                              -- (a/b)' = (a'*b - a*b')/(b^2)
diff (BFunc (Infix Expo) f g) x = BFunc (Infix Mult) (BFunc (Infix Expo) f g) (BFunc (Infix Add) (BFunc (Infix Div) (BFunc (Infix Mult) (diff f x) g) (diff f x)) (BFunc (Infix Mult) (diff g x) (BFunc (Prefix Log) (Num e) f)))    -- (f^g)' = (f^g)*(f'*g/f + g'*ln(f))
diff (BFunc (Prefix Log) f g) x | f == Num e = BFunc (Infix Div) (diff g x) g                                                                    -- (ln(f))' = (ln(f))' / f
                 | otherwise  = BFunc (Infix Div) (BFunc (Infix BSub) (BFunc (Infix Mult) (BFunc (Infix Div) (diff g x) g) (BFunc (Prefix Log) (Num e) f)) (BFunc (Infix Mult) (BFunc (Prefix Log) (Num e) g) (BFunc (Infix Div) (diff f x) f))) (BFunc (Infix Expo) (BFunc (Prefix Log) (Num e) f) (Num 2))    
                                                                                                                    -- logf(g)' = (g'*ln(f)/g - ln(g)*f'/f) / (ln(f))^2



-- TODOOOO
simplify :: Expr -> Expr
simplify = id
