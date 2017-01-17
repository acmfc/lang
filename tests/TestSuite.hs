module Main ( main ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Lang.Type.Tests

main :: IO ()
main = defaultMain $ testGroup "Lang" [Lang.Type.Tests.tests]
