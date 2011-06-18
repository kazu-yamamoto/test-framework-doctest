{-| Wrapper for running DocTests with Test.Framework 

First we get the doctests wrapped in 'Test.Framework.Test' using
'frameDocTestsFrom'.  The first argument to 'frameDocTestsFrom' should be the root module
i.e., a module that includes all the other modules.

>>> doctests <- frameDocTestsFrom "tests/Test.hs" ["-itests"]

After getting the doctests we can execute the doctests using the
'defaultMain' or 'defaultMainWithOpts' functions.

>>> defaultMainWithOpts [doctests] defaultOptions
DocTest:
  print "abc": [Failed]
Failed: expression `print "abc"'
expected: ["\"fail\""]
 but got: ["\"abc\""]
  print bar: [OK]
<BLANKLINE>
         Test Cases  Total      
 Passed  1           1          
 Failed  1           1          
 Total   2           2          
*** Exception: ExitFailure 1

Above we used 'defaultMainWithOpts' for running the tests so that we
can specify that we want plain output as coloride output is not very
readable:

>>> print $ ropt_plain_output defaultOptions
Just True

-}

module Test.Framework.DocTest (frameDocTestsFrom) where

import Documentation.Haddock
import qualified Test.DocTest as DocTest
import Test.Framework
import Test.Framework.Providers.HUnit

defaultOptions = RunnerOptions { ropt_threads = Nothing
                               , ropt_test_options = Nothing
                               , ropt_test_patterns = Nothing
                               , ropt_xml_output = Nothing 
                               , ropt_plain_output = Just True
                               , ropt_hide_successes = Nothing
                               }

-- | Note that 'frameDocTestsFrom' can be called only once per process execution

frameDocTestsFrom::FilePath -> [String] -> IO Test
frameDocTestsFrom rootPath options = do
  tests <- DocTest.getDocTests [Flag_Verbosity "0", Flag_NoWarnings] [rootPath]
  return $ toTestFrameworkGroup (rootPath:options) tests
  
toTestFrameworkTest :: [String] -> DocTest.DocTest -> Test 
toTestFrameworkTest options test = testCase testName $ DocTest.withInterpreter options $ flip DocTest.toAssertion test
  where
    testName = DocTest.expression $ head $ DocTest.interactions test

toTestFrameworkGroup :: [String] -> [DocTest.DocTest] -> Test
toTestFrameworkGroup options = testGroup "DocTest" . map (toTestFrameworkTest options)