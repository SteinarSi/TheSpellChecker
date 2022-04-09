
{-# LANGUAGE OverloadedStrings #-}


module Calculus (differentiate, simplify) where


import Data.Text (Text)
import TextShow (TextShow)




import Expr
import Utility


--simplifyFunction :: Function n -> Function n
--simplifyFunction (Function name params ex) = Function name params (simplify ex)

differentiate :: (RealFloat n) => Expr n -> Text -> Either Text (Expr n)
differentiate expr x = fmap simplify (diff (simplify expr) x)



diff :: (RealFloat n) => Expr n -> Text -> Either Text (Expr n)

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

-- (ln(g))' = (ln(g')) / g
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
simplify :: (RealFloat n) => Expr n -> Expr n
simplify ex | simp ex == ex = ex
            | otherwise = simplify (simp ex)

simp :: (RealFloat n) => Expr n -> Expr n
simp (R r) | r == fromInteger (round r) = Z (round r)      -- a.0 = a

simp (BFunc (Infix Add) a (Z 0)) = simp a                 -- a + 0 = a
simp (BFunc (Infix Add) a (R 0)) = simp a
simp (BFunc (Infix Add) (R 0) b) = simp b                 -- 0 + a = a
simp (BFunc (Infix Add) (Z 0) b) = simp b
simp (BFunc (Infix Add) a (UFunc USub b)) = simp (BFunc (Infix BSub) a b)     -- a + (-b) = a - b
simp (BFunc (Infix Add) (UFunc USub b) a) = simp (BFunc (Infix BSub) a b)     -- (-b) + a = a - b

simp (BFunc (Infix Add) (Z z1) (Z z2)) = Z (z1+z2)
simp (BFunc (Infix Add) (R r1) (R r2)) = R (r1+r2)

simp (UFunc USub (UFunc USub a)) = simp a             -- -(-a) = a

simp (BFunc (Infix Mult) a (Z 1)) = simp a            -- a * 1 = a
simp (BFunc (Infix Mult) a (R 1)) = simp a
simp (BFunc (Infix Mult) (Z 1) b) = simp b            -- 1 * a = a
simp (BFunc (Infix Mult) (R 1) b) = simp b
simp (BFunc (Infix Mult) _ (R 0)) = Z 0                -- a * 0 = 0
simp (BFunc (Infix Mult) _ (Z 0)) = Z 0
simp (BFunc (Infix Mult) (R 0) _) = Z 0                -- 0 * a = 0
simp (BFunc (Infix Mult) (Z 0) _) = Z 0
simp (BFunc (Infix Mult) (Z z1) (Z z2)) = Z (z1*z2)
simp (BFunc (Infix Mult) (R r1) (R r2)) = R (r2*r2)
                                                        -- (a/b) * (c/d) = (a*c) / (b*d)
simp (BFunc (Infix Mult) (BFunc (Infix Div) a b) (BFunc (Infix Div) c d)) = BFunc (Infix Div) (BFunc (Infix Mult) (simp a) (simp c)) (BFunc (Infix Mult) (simp b) (simp d))

                                                        -- (a/b) / (c/d) = (a*d) / (b*c)
simp (BFunc (Infix Div) (BFunc (Infix Div) a b) (BFunc (Infix Div) c d)) = BFunc (Infix Div) (BFunc (Infix Mult) (simp a) (simp d)) (BFunc (Infix Mult) (simp b) (simp c))
simp (BFunc (Infix Div) (Z z1) (Z z2)) | mod z1 z2 == 0 = Z (div z1 z2)
                                       | otherwise = BFunc (Infix Div) (Z (z1 `div` gcd z1 z2)) (Z (z2 `div` gcd z1 z2))
simp (BFunc (Infix Div) (R r1) (R r2)) = R (r1 / r2)



-- Resten av casene, når ingen av reglene gjelder
simp (R n) = R n
simp (Z n) = Z n
simp (Const c) = Const c
simp (Var x) = Var x
simp (BFunc c a b) = BFunc c (simp a) (simp b)
simp (UFunc c a) = UFunc c (simp a)



