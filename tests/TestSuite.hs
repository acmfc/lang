module Main ( main ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Lang.DependencyAnalysis.Tests
import qualified Lang.Parser.Tests
import qualified Lang.Type.Tests
import qualified Lang.TypeInference.Tests

main :: IO ()
main = defaultMain $ testGroup "Lang"
    [ Lang.DependencyAnalysis.Tests.tests
    , Lang.Parser.Tests.tests
    , Lang.Type.Tests.tests
    , Lang.TypeInference.Tests.tests
    ]

