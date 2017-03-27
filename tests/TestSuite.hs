module Main ( main ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Lang.Type.Tests
import qualified Lang.DependencyAnalysis.Tests
import qualified Lang.Parser.Tests

main :: IO ()
main = defaultMain $ testGroup "Lang"
    [ Lang.Type.Tests.tests
    , Lang.DependencyAnalysis.Tests.tests
    , Lang.Parser.Tests.tests
    ]
