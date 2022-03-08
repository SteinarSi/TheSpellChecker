
{-# LANGUAGE OverloadedStrings #-}


module Calculus where


import Data.Text (Text)
import TextShow (TextShow)




import Expr
import Utility



differentiate :: (RealFloat n, Show n, TextShow n) => Expr n -> Text -> Either Text (Expr n)
differentiate expr x = fmap simplify (diff (simplify expr) x)



diff :: (RealFloat n, Show n, TextShow n) => Expr n -> Text -> Either Text (Expr n)

-- a' = 0
diff (Num n) _ = Right (Num 0)

-- x' = 1, y' = 0
diff (Var v) x | v == x = Right (Num 1)
               | otherwise = Right (Num 0)

-- (f + g)' = f' + g'
diff (BFunc (Infix Add) a b) x = BFunc (Infix Add) <$> diff a x <*> diff b x

-- (f - g)' = f' - g'
diff (BFunc (Infix BSub) a b) x = BFunc (Infix BSub) <$> diff a x <*> diff b x

-- (-f)' = -f'
diff (UFunc USub a) x = UFunc USub <$> diff a x

-- sin(f)' = cos(x) * f'
diff (UFunc Sin a) x = BFunc (Infix Mult) (UFunc Cos a) <$> diff a x

-- cos(f)' = - sin(x) * f'
diff (UFunc Cos a) x = BFunc (Infix Mult) (UFunc USub (UFunc Sin a)) <$> diff a x

-- tan(f)' = f'/cos^2(f)
diff (UFunc Tan a) x = BFunc (Infix Div) <$> diff a x <*> Right (BFunc (Infix Expo) (UFunc Cos a) (Num 2))

-- (sqrt(f))' = (f^(1/2))'
diff (UFunc Sqrt f) x = diff (BFunc (Infix Expo) f (BFunc (Infix Div) (Num 1) (Num 2))) x

-- (f * g)' = f' * g + f * g'
diff (BFunc (Infix Mult) a b) x = BFunc (Infix Add) <$> (BFunc (Infix Mult) <$> diff a x <*> Right b) <*> (BFunc (Infix Mult) a <$> diff b x)

-- (f / g)' = (f' * g - f * g') / g^2
diff (BFunc (Infix Div) f g) x = BFunc (Infix Div) <$> 
    (BFunc (Infix BSub) <$> 
        (BFunc (Infix Mult) <$> diff f x <*> Right g) <*> 
        (BFunc (Infix Mult) f <$> diff g x)) <*> 
    Right (BFunc (Infix Expo) g (Num 2))

-- (f^g)' = (f^g)*(f'*g/f + g'*ln(f))                                                                                                                                           -- (f^g)' = (f^g)*(f'*g/f + g'*ln(f))
diff (BFunc (Infix Expo) f g) x = BFunc (Infix Mult) 
        (BFunc (Infix Expo) f g) <$> 
        (BFunc (Infix Add) <$> 
            (BFunc (Infix Div) <$> (BFunc (Infix Mult) <$> diff f x <*> Right g) <*> Right f) <*> 
            (BFunc (Infix Mult) <$> diff g x <*> Right (BFunc (Prefix Log) (Num e) f)))

-- (ln(g))' = (ln(g))' / g
diff (BFunc (Prefix Log) f g) x | f == Num e = BFunc (Infix Div) <$> diff g x <*> Right g

                            -- logf(g)' = (g'*ln(f)/g - ln(g)*f'/f) / (ln(f))^2
                                | otherwise = BFunc (Infix Div) <$> 
                                    (BFunc (Infix BSub) <$> 
                                        (BFunc (Infix Div) <$> (BFunc (Infix Mult) <$> diff g x <*> Right (BFunc (Prefix Log) (Num e) f)) <*> Right g) <*> 
                                        (BFunc (Infix Div) <$> (BFunc (Infix Mult) (BFunc (Prefix Log) (Num e) g) <$> diff f x) <*> Right f)) <*> 
                                    Right (BFunc (Infix Expo) (BFunc (Prefix Log) (Num e) f) (Num 2))

diff (BFunc (Prefix c) _ _) _ = let (_, name, _) = bFuncFromConstr (Prefix c) in Left ("The function " <> name <> "() has no defined derivative (yet)")
diff (BFunc (Infix c)  _ _) _ = let (_, name, _) = bFuncFromConstr (Infix  c) in Left ("The operator " <> name <> " has no defined derivative (yet)")
diff (UFunc c _) _ = let (_, name, _) = uFuncFromConstr c in Left ("The function " <> name <> " has no defined derivative (yet)")


{-
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

-}

-- TODOOOO
simplify :: Expr n -> Expr n
simplify = id
