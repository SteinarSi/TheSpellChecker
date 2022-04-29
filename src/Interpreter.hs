{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Data.Text as T


import Data.Either (either)
import TextShow (showb, toString)
import Data.List (delete)
import Numeric (showFFloat)

import Utility (debug)
import Expr ( Function(Function), eval, inline )
import ParseREPL( parseCommand, Command(..), parseData )
import ParserUtility (UserData(..), parse)
import Calculus (simplify, simplifyData)
import Draw



loop :: [UserData Double] -> [Drawable Double] -> IO ()
loop fs ds = do
    inn <- getLine
    case parseCommand fs (T.pack inn) of 
        Left err -> print err >> loop fs ds
        Right ex -> case ex of
            Help -> readFile "data/help.txt" >>= putStrLn >> loop fs ds
            Quit -> putStrLn "Bye-bye!"
            NoAction -> loop fs ds
            NewData dt -> let sdt = simplifyData dt in loop (sdt : delete sdt fs) ds
            Eval ex -> putStrLn (either T.unpack (\r -> show ex <> " = " <> showFFloat Nothing r "") (eval ex)) >> loop fs ds
            ShowFunction f -> print f >> loop fs ds
            Draw -> draw ds >> loop fs ds
            Save -> saveData fs >> loop fs ds
            Load -> loadData >>= flip loop ds . (++fs)
            Clear -> loop [] []
            AddDrawable d -> loop fs (d:ds)


saveData :: (RealFloat n, Show n) => [UserData n] -> IO ()
saveData = writeFile "data/userdata.txt" . unlines . reverse . map show

loadData :: RealFloat n => IO [UserData n]
loadData = fmap (loadData' [] . lines) $ readFile "data/userdata.txt"
    where loadData' dt [] = dt
          loadData' dt (x:xs) = case parse parseData dt (T.pack x) of
              Left  _ -> loadData' dt xs
              Right d -> loadData' (d:dt) xs
          