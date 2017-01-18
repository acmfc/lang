module Main where

import Lang.Core

main :: IO ()
main = do
    putStrLn . show $ EVar "var"
