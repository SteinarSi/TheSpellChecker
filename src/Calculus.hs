
{-# LANGUAGE OverloadedStrings, TupleSections, RankNTypes #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}


module Calculus where


import Data.Text (Text)
import TextShow (TextShow)
import Data.List (sort, find, delete)
import Control.Monad.Writer

import Data.Either (rights)


import Expr
import Utility



evalAll :: (RealFloat n, Enum n, Show n) => (n, n) -> Function n -> [[(n, n)]]
evalAll it (Function _ [p] ex) = splitRights $ map (\x -> (x,) <$> eval (betaReduce ex [(p, R x)])) $ linspace 1921 it
evalAll _ _ = []


splitRights :: [Either a b] -> [[b]]
splitRights = splitRights' []
    where splitRights' [] [] = []
          splitRights' ys [] = [ys]
          splitRights' ys (Left  _ : xs) = reverse ys : splitRights' [] xs
          splitRights' ys (Right y : xs) = splitRights' (y:ys) xs

simplifyFunction :: RealFloat n => Function n -> Function n
simplifyFunction (Function name params ex) = Function name params (simplify ex)

differentiateFunction :: RealFloat n => Function n -> Text -> Either Text (Function n)
differentiateFunction (Function name params ex) x = Function (name <> "'") params <$> differentiate ex x

differentiate :: (RealFloat n) => Expr n -> Text -> Either Text (Expr n)
differentiate expr x = fmap simplify (diff (simplify expr) x)



diff :: (RealFloat n) => Expr n -> Text -> Either Text (Expr n)

-- a' = 0
diff (Z _) _ = Right (Z 0)
diff (R _) _ = Right (R 0)
diff (Const _) _ = Right (Z 0)

-- x' = 1, y' = 0
diff (BoundedVar v) x | v == x = Right (Z 1)
                      | otherwise = Right (Z 0)

diff (FreeVar _ ex) x = diff ex x

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
diff (FFunc (Function _ params expr) args) x = diff (betaReduce expr (zip params args)) x

-- sinh(f)' = f' * cosh(f)
diff (UFunc Sinh f) x = fmap (* cosh f) (diff f x)

-- cosh(f)' = f' * sin(f)
diff (UFunc Cosh f) x = fmap (* sinh f) (diff f x)

-- [tanh(f)]′ = f'/cosh2(f)
diff (UFunc Tanh f) x = fmap (/ (cosh f * cosh f)) (diff f x)

-- asin(f)' = f'/(sqrt(1-f^2))
diff (UFunc Asin f) x = fmap (/ sqrt (1-f**2)) (diff f x)

-- acos(f)' = - f'/sqrt(1-f^2)
diff (UFunc Acos f) x = UFunc USub <$> fmap (/ (sqrt (1-f**2))) (diff f x)

-- atan(f)' = f'/(1+f^2)
diff (UFunc Atan f) x = fmap (/ (1 + f**2)) (diff f x)

-- atanh(f)' = f'/(sqrt(f^2-1))
diff (UFunc Asinh f) x = fmap (/ sqrt (f**2-1)) (diff f x)

-- acosh(f)' = f'/(sqrt(f^2-1))
diff (UFunc Acosh f) x = fmap (/ sqrt (f**2-1)) (diff f x)

-- atanh(f)' = f'/(sqrt(1-f^2))
diff (UFunc Atanh f) x = fmap (/ sqrt (1-f**2)) (diff f x)


diff (BFunc (Prefix c) _ _) _ = let (_, name, _, _) = bFuncFromConstr (Prefix c) in Left ("The function " <> name <> "() has no defined derivative (yet) (should it?).")
diff (BFunc (Infix c)  _ _) _ = let (_, name, _, _) = bFuncFromConstr (Infix  c) in Left ("The operator " <> name <> " has no defined derivative (yet) (should it?).")
diff (UFunc c _) _ = let (_, name, _, _) = uFuncFromConstr c in Left ("The function " <> name <> "() has no defined derivative (yet) (should it?).")


simplifyWriter :: RealFloat n => Expr n -> Writer [Expr n] (Expr n)
simplifyWriter ex | simp ex /= ex = tell [ex] >> simplifyWriter (simp ex)
                  | simpAddMult ex /= ex = tell [ex] >> simplifyWriter (simpAddMult ex)
                  | otherwise = writer (ex, [ex])


simplify :: (RealFloat n) => Expr n -> Expr n
simplify ex | simp ex /= ex = simplify (simp ex)
            | simpAddMult ex /= ex = simplify (simpAddMult ex)
            | otherwise = ex

simp :: (RealFloat n) => Expr n -> Expr n
simp (R r) | r == fromInteger (round r) = Z (round r)      -- a.0 = a

simp (BFunc (Infix Add) a (Z 0)) = simp a                 -- a + 0 = a
simp (BFunc (Infix Add) (Z 0) b) = simp b                 -- 0 + a = a
simp (BFunc (Infix Add) (Z z) (R r)) = R (r + fromInteger z)
simp (BFunc (Infix Add) (R r) (Z z)) = R (r + fromInteger z)
simp (BFunc (Infix Add) a (UFunc USub b)) = simp (BFunc (Infix BSub) a b)     -- a + (-b) = a - b
simp (BFunc (Infix Add) (UFunc USub b) a) = simp (BFunc (Infix BSub) a b)     -- (-b) + a = a - b
simp (BFunc (Infix Add) a b) | a == b = 2 * a

simp (BFunc (Infix Add) (Z z1) (Z z2)) = Z (z1+z2)
simp (BFunc (Infix Add) (R r1) (R r2)) = R (r1+r2)

simp (UFunc USub (R r)) = R (negate r)
simp (UFunc USub (Z z)) = Z (negate z)
simp (UFunc USub (UFunc USub a)) = simp a               -- -(-a) = a
simp (BFunc (Infix BSub) (Z 0) b) = UFunc USub (simp b) -- 0 - b = -b
simp (BFunc (Infix BSub) b (Z 0)) = simp b              -- b-0 = b
simp (BFunc (Infix BSub) (Z z1) (Z z2)) = Z (z1-z2)
simp (BFunc (Infix BSub) (R r1) (R r2)) = R (r1-r2)

simp (BFunc (Infix Mult) a (Z 1)) = simp a              -- a * 1 = a
simp (BFunc (Infix Mult) (Z 1) b) = simp b              -- 1 * a = a
simp (BFunc (Infix Mult) _ (Z 0)) = Z 0                 -- a * 0 = 0
simp (BFunc (Infix Mult) (Z 0) _) = Z 0                 -- 0 * a = 0
simp (BFunc (Infix Mult) (Z z1) (Z z2)) = Z (z1*z2)
simp (BFunc (Infix Mult) (R r1) (R r2)) = R (r1*r2)

                                                        -- (a/b) * (c/d) = (a*c) / (b*d)
simp (BFunc (Infix Mult) (BFunc (Infix Div) a b) (BFunc (Infix Div) c d)) = (a*c) / (b*d)
simp (BFunc (Infix Mult) a b) | a == b = a ** 2
simp (BFunc (Infix Mult) (BFunc (Infix Expo) a b) c) | a == c = a ** (b+1)
simp (BFunc (Infix Mult) a (BFunc (Infix Expo) b c)) | a == b = a ** (c+1)
simp (BFunc (Infix Mult) (BFunc (Infix Expo) a b) (BFunc (Infix Expo) c d)) | a == c = a ** (b+d)

                                                        -- (a/b) / (c/d) = (a*d) / (b*c)
simp (BFunc (Infix Div) (BFunc (Infix Div) a b) (BFunc (Infix Div) c d)) = (a*d) / (b*c)
simp (BFunc (Infix Div) (Z z1) (Z z2)) | z2 /= 0 && mod z1 z2 == 0 = Z (div z1 z2)
                                       | otherwise = Z (z1 `div` gcd z1 z2) / Z (z2 `div` gcd z1 z2)
simp (BFunc (Infix Div) (R r1) (R r2)) = R (r1 / r2)
simp (BFunc (Infix Div) (Z 0) _) = Z 0
simp (BFunc (Infix Div) a (Z 1)) = a                    -- a/1 = a

simp (BFunc (Infix Div) a b) | a == b = Z 1             -- a/a = 1
simp (BFunc (Infix Div) (BFunc (Infix Expo) a b) c) | a == c = a**(b-1)
simp (BFunc (Infix Div) a (BFunc (Infix Expo) b c)) | a == b = 1 / (b**(c-1))
simp (BFunc (Infix Div) (BFunc (Infix Expo) a b) (BFunc (Infix Expo) c d)) | a == c = a**(b-d)

simp (BFunc (Infix Expo) a 0) = 1
simp (BFunc (Infix Expo) a 1) = a
simp (BFunc (Infix Expo) 0 _) = 0

simp (UFunc Sin (Z 0)) = 0
simp (UFunc Sin (Const Pi)) = 0
simp (UFunc Sin (BFunc (Infix Mult) (Z _) (Const Pi))) = 0

simp (UFunc Cos (Z 0)) = 1
simp (UFunc Cos (Const Pi)) = -1
simp (UFunc Cos (BFunc (Infix Mult) (Z z) (Const Pi))) | even z = 1
                                                       | otherwise = -1


-- Resten av casene, når ingen av reglene gjelder
simp (R n)                                    = R n
simp (Z n)                                    = Z n
simp (Const c)                                = Const c
simp (FreeVar v ex)                           = FreeVar v ex
simp (BoundedVar v)                           = BoundedVar v
simp (BFunc c a b)                            = BFunc c (simp a) (simp b)
simp (UFunc c a)                              = UFunc c (simp a)
simp (FFunc (Function name params expr) args) = FFunc (Function name params (simp expr)) (map simp args)


simpAddMult :: RealFloat n => Expr n -> Expr n
simpAddMult ex | ex /= simpAdd ex = simpAdd ex
               | ex /= simpMult ex = simpMult ex
simpAddMult (R n)                                    = R n
simpAddMult (Z n)                                    = Z n
simpAddMult (Const c)                                = Const c
simpAddMult (FreeVar v ex)                           = FreeVar v ex
simpAddMult (BoundedVar v)                           = BoundedVar v
simpAddMult (BFunc c a b)                            = BFunc c (simpAddMult a) (simpAddMult b)
simpAddMult (UFunc c a)                              = UFunc c (simpAddMult a)
simpAddMult (FFunc (Function name params expr) args) = FFunc (Function name params (simpAddMult expr)) (map simpAddMult args)

simpAdd :: RealFloat n => Expr n -> Expr n
simpAdd = listAdd . simpAddList . addList

simpMult :: RealFloat n => Expr n -> Expr n
simpMult ex = let (xs, ys) = multList ex
              in  listMult $ simpMultDiv (simpMultList xs, simpMultList ys)

simpMultDiv :: RealFloat n => ([Expr n], [Expr n]) -> ([Expr n], [Expr n])
simpMultDiv (xs, []) = (xs, [])
simpMultDiv (xs, y:ys) = case find (\x -> x/y /= simp (x/y)) xs of
    Just  x -> let (xs', ys') = simpMultDiv (delete x xs, ys) in (simp (x/y) : xs', ys')
    Nothing -> fmap (y:) (simpMultDiv (xs, ys))

simpMultList :: RealFloat n => [Expr n] -> [Expr n]
simpMultList [] = []
simpMultList (x:xs) = case find (\y -> x*y /= simp (x*y)) xs of
    Just  y -> simp (x*y) : simpMultList (delete y xs)
    Nothing -> x : simpMultList xs

simpAddList :: RealFloat n => [Expr n] -> [Expr n]
simpAddList [] = []
simpAddList (x:xs) = case find (\y -> x+y /= simp (x+y)) xs of
    Just  y -> simp (x+y) : simpAddList (delete y xs)
    Nothing -> x : simpAddList xs


multList :: Expr n -> ([Expr n], [Expr n])
multList (BFunc (Infix Mult) a b) = let (ms1, ds1) = multList a
                                        (ms2, ds2) = multList b
                                    in  (ms1++ms2, ds1++ds2)
multList (BFunc (Infix Div) a b)  = let (ms1, ds1) = multList a
                                        (ms2, ds2) = multList b
                                    in  (ms1++ds2, ds1++ms2)
multList a = ([a], [])

listMult :: Eq n => ([Expr n], [Expr n]) -> Expr n
listMult (a, []) = foldl1 (BFunc (Infix Mult)) $ sort a
listMult (a, b)  = BFunc (Infix Div) (foldl1 (BFunc (Infix Mult)) (sort a)) (foldl1 (BFunc (Infix Mult)) (sort b))

addList :: Expr n -> [Expr n]
addList (BFunc (Infix Add) a b) = addList a ++ addList b
addList a = [a]

listAdd :: Eq n => [Expr n] -> Expr n
listAdd = foldl1 (BFunc (Infix Add)) . sort


