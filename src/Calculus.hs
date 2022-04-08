
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
diff (Z _) _ = Right (Z 0)
diff (R _) _ = Right (R 0)
diff (Const _) _ = Right (Z 0)

-- x' = 1, y' = 0
diff (Var v) x | v == x = Right (Z 1)
               | otherwise = Right (Z 0)

-- (f + g)' = f' + g'
diff (BFunc (Infix Add) a b) x = BFunc (Infix Add) <$> diff a x <*> diff b x

-- (f - g)' = f' - g'
diff (BFunc (Infix BSub) a b) x = BFunc (Infix BSub) <$> diff a x <*> diff b x

-- (-f)' = -f'
diff (UFunc USub a) x = UFunc USub <$> diff a x

-- (succ(f))' = f'
diff (UFunc Succ a) x = diff a x

-- (pred(f))' = f'
diff (UFunc Pred a) x = diff a x

-- sin(f)' = cos(x) * f'
diff (UFunc Sin a) x = BFunc (Infix Mult) (UFunc Cos a) <$> diff a x

-- cos(f)' = - sin(x) * f'
diff (UFunc Cos a) x = BFunc (Infix Mult) (UFunc USub (UFunc Sin a)) <$> diff a x

-- tan(f)' = f'/cos^2(f)
diff (UFunc Tan a) x = BFunc (Infix Div) <$> diff a x <*> Right (BFunc (Infix Expo) (UFunc Cos a) (Z 2))

-- (sqrt(f))' = (f^(1/2))'
diff (UFunc Sqrt f) x = diff (BFunc (Infix Expo) f (BFunc (Infix Div) (Z 1) (Z 2))) x

-- (f * g)' = f' * g + f * g'
diff (BFunc (Infix Mult) a b) x = BFunc (Infix Add) <$> (BFunc (Infix Mult) <$> diff a x <*> Right b) <*> (BFunc (Infix Mult) a <$> diff b x)

-- (f / g)' = (f' * g - f * g') / g^2
diff (BFunc (Infix Div) f g) x = BFunc (Infix Div) <$> 
    (BFunc (Infix BSub) <$> 
        (BFunc (Infix Mult) <$> diff f x <*> Right g) <*> 
        (BFunc (Infix Mult) f <$> diff g x)) <*> 
    Right (BFunc (Infix Expo) g (Z 2))

-- (f^g)' = (f^g)*(f'*g/f + g'*ln(f))                                                                                                                                           -- (f^g)' = (f^g)*(f'*g/f + g'*ln(f))
diff (BFunc (Infix Expo) f g) x = BFunc (Infix Mult) 
        (BFunc (Infix Expo) f g) <$> 
        (BFunc (Infix Add) <$> 
            (BFunc (Infix Div) <$> (BFunc (Infix Mult) <$> diff f x <*> Right g) <*> Right f) <*> 
            (BFunc (Infix Mult) <$> diff g x <*> Right (BFunc (Prefix Log) (Const E) f)))

-- (ln(g))' = (ln(g))' / g
diff (BFunc (Prefix Log) f g) x | f == Const E = BFunc (Infix Div) <$> diff g x <*> Right g

                            -- logf(g)' = (g'*ln(f)/g - ln(g)*f'/f) / (ln(f))^2
                                | otherwise = BFunc (Infix Div) <$> 
                                    (BFunc (Infix BSub) <$> 
                                        (BFunc (Infix Div) <$> (BFunc (Infix Mult) <$> diff g x <*> Right (BFunc (Prefix Log) (Const E) f)) <*> Right g) <*> 
                                        (BFunc (Infix Div) <$> (BFunc (Infix Mult) (BFunc (Prefix Log) (Const E) g) <$> diff f x) <*> Right f)) <*> 
                                    Right (BFunc (Infix Expo) (BFunc (Prefix Log) (Const E) f) (Z 2))

diff (BFunc (Prefix c) _ _) _ = let (_, name, _) = bFuncFromConstr (Prefix c) in Left ("The function " <> name <> "() has no defined derivative (yet)")
diff (BFunc (Infix c)  _ _) _ = let (_, name, _) = bFuncFromConstr (Infix  c) in Left ("The operator " <> name <> " has no defined derivative (yet)")
diff (UFunc c _) _ = let (_, name, _) = uFuncFromConstr c in Left ("The function " <> name <> " has no defined derivative (yet)")


-- TODOOOO
simplify :: (RealFloat n, TextShow n, Show n) => Expr n -> Expr n
simplify ex | simpC ex == ex = ex
            | otherwise = simplify (simpC ex)

simpC :: (RealFloat n, TextShow n, Show n) => Expr n -> Expr n
simpC ex | isConstant ex = R (evalConstant ex)
         | otherwise = simpX ex

simpX :: (RealFloat n, TextShow n, Show n) => Expr n -> Expr n
simpX (BFunc (Infix Add) a (Z 0)) = simpC a
simpX (BFunc (Infix Add) a (R 0)) = simpC a
simpX (BFunc (Infix Add) (R 0) b) = simpC b
simpX (BFunc (Infix Add) (Z 0) b) = simpC b
simpX (BFunc (Infix Add) a (UFunc USub b)) = simpC (BFunc (Infix BSub) a b)
simpX (BFunc (Infix Add) (UFunc USub b) a) = simpC (BFunc (Infix BSub) a b)

simpX (UFunc USub (UFunc USub a)) = simpC a

simpX (BFunc (Infix Mult) a (Z 1)) = simpC a
simpX (BFunc (Infix Mult) a (R 1)) = simpC a
simpX (BFunc (Infix Mult) (Z 1) b) = simpC b
simpX (BFunc (Infix Mult) (R 1) b) = simpC b
simpX (BFunc (Infix Mult) _ (R 0)) = Z 0
simpX (BFunc (Infix Mult) _ (Z 0)) = Z 0
simpX (BFunc (Infix Mult) (R 0) _) = Z 0
simpX (BFunc (Infix Mult) (Z 0) _) = Z 0
simpX (BFunc (Infix Mult) (BFunc (Infix Div) a b) (BFunc (Infix Div) c d)) = BFunc (Infix Div) (BFunc (Infix Mult) (simpC a) (simpC c)) (BFunc (Infix Mult) (simpC b) (simpC d))

simpX (BFunc (Infix Div) (BFunc (Infix Div) a b) (BFunc (Infix Div) c d)) = BFunc (Infix Div) (BFunc (Infix Mult) (simpC a) (simpC d)) (BFunc (Infix Mult) (simpC b) (simpC c))



-- Resten av casene, når ingen av reglene gjelder
simpX (R n) = R n
simpX (Z n) = Z n
simpX (Const c) = Const c
simpX (Var x) = Var x
simpX (BFunc c a b) = BFunc c (simpC a) (simpC b)
simpX (UFunc c a) = UFunc c (simpC a)



