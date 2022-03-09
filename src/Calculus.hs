
{-# LANGUAGE OverloadedStrings #-}


module Calculus (differentiate, simplify) where


import Data.Text (Text)
import TextShow (TextShow)




import Expr
import Utility



differentiate :: (RealFloat n, Show n, TextShow n) => Expr n -> Text -> Either Text (Expr n)
differentiate expr x = fmap simplify (diff (simplify expr) x)



diff :: (RealFloat n, Show n, TextShow n) => Expr n -> Text -> Either Text (Expr n)

-- a' = 0
diff (Num n) _ = Right (Num 0)
diff (Const name n) _ = Right (Const name n)

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


-- TODOOOO
simplify :: (RealFloat n, TextShow n, Show n) => Expr n -> Expr n
simplify ex | simpC ex == ex = ex
            | otherwise = simplify (simpC ex)

simpC :: (RealFloat n, TextShow n, Show n) => Expr n -> Expr n
simpC ex | isConstant ex = Num (evalConstant ex)
         | otherwise = simpX ex

simpX :: (RealFloat n, TextShow n, Show n) => Expr n -> Expr n
simpX (BFunc (Infix Add) a (Num 0)) = simpC a
simpX (BFunc (Infix Add) (Num 0) b) = simpC b
simpX (BFunc (Infix Add) a (UFunc USub b)) = simpC (BFunc (Infix BSub) a b)

simpX (UFunc USub (UFunc USub a)) = simpC a

simpX (BFunc (Infix Mult) a (Num 1)) = simpC a
simpX (BFunc (Infix Mult) (Num 1) b) = simpC b
simpX (BFunc (Infix Mult) _ (Num 0)) = Num 0
simpX (BFunc (Infix Mult) (Num 0) _) = Num 0
simpX (BFunc (Infix Mult) (BFunc (Infix Div) a b) (BFunc (Infix Div) c d)) = BFunc (Infix Div) (BFunc (Infix Mult) (simpC a) (simpC c)) (BFunc (Infix Mult) (simpC b) (simpC d))

simpX (BFunc (Infix Div) (BFunc (Infix Div) a b) (BFunc (Infix Div) c d)) = BFunc (Infix Div) (BFunc (Infix Mult) (simpC a) (simpC d)) (BFunc (Infix Mult) (simpC b) (simpC c))



-- Resten av casene, når ingen av reglene gjelder
simpX (Num n) = Num n
simpX (Const name n) = Const name n
simpX (Var x) = Var x
simpX (BFunc c a b) = BFunc c (simpC a) (simpC b)
simpX (UFunc c a) = UFunc c (simpC a)



