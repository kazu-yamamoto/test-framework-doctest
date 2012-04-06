{-# LANGUAGE CPP #-}

{-| Wrapper for running DocTests with Test.Framework

First we get the doctests wrapped in 'Test.Framework.Test' using
'docTest'.  The first argument to 'docTest' should be the root modules
i.e., the modules that are not imported by other modules.

>>> doctests <- docTest ["tests/Test.hs"] ["-itests"]

After getting the doctests we can execute the doctests using the
'defaultMain' or 'defaultMainWithOpts' functions.

>>> :m + Data.Monoid
>>> defaultMainWithOpts [doctests] noColors
DocTest:
  Test:
    print "abc": [Failed]
expression `print "abc"'
expected: ["\"fail\""]
 but got: ["\"abc\""]
    print bar: [OK]
<BLANKLINE>
         Test Cases  Total      
 Passed  1           1          
 Failed  1           1          
 Total   2           2          
*** Exception: ExitFailure 1

The @*** Exception: ExitFailure 1@ is caused by
'defaultMainWithOptions' trying to exit after finishing with tests.

-}

module Test.Framework.Providers.DocTest (docTest, xxx) where

import Control.Applicative
import Data.List(groupBy)
import Data.Monoid
import Test.DocTest (Example, Module(..), Interaction(..))
import qualified Test.DocTest as DocTest
import Test.Framework
import Test.Framework.Providers.HUnit

noColors :: RunnerOptions
noColors =  mempty {
    ropt_color_mode = Nothing
}

-- | Note that 'docTest' can be called only once per process execution
--
-- You only need to give paths to modules that are not imported from any other module

xxx rootPaths opts = DocTest.getDocTests opts rootPaths

docTest :: [FilePath] -- ^ Paths to root modules
        -> [String] -- ^ Options passed to ghci
        -> IO Test
docTest rootPaths opts = toTestFrameworkGroup opts <$> getModuleExamples
  where
    options = rootPaths ++ opts
    getModuleExamples = DocTest.getDocTests opts rootPaths

toTestFrameworkGroup :: [String] -> [Module Example] -> Test
toTestFrameworkGroup opts mdls = testGroup "DocTest" tests
  where
    tests = map (toModuleTestGroup opts) mdls
    
toModuleTestGroup :: [String] -> Module Example -> Test
toModuleTestGroup opts mdl = testGroup mdlName tests
  where
    mdlName = moduleName mdl
    examples = moduleContent mdl
    tests = map (toTestFrameworkTest opts mdlName) examples

toTestFrameworkTest :: [String] -> String -> Example -> Test
toTestFrameworkTest opts mdlName example =
    testCase testName . DocTest.withInterpreter opts $ \iptr -> 
      DocTest.toAssertion iptr mdlName example
  where
    testName = expression . head . DocTest.exampleToInteractions $ example
