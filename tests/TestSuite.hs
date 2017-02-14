module Main ( main ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Lang.Type.Tests
import qualified Lang.DependencyAnalysis.Tests

main :: IO ()
main = defaultMain $ testGroup "Lang"
    [ Lang.Type.Tests.tests
    , Lang.DependencyAnalysis.Tests.tests
    ]
