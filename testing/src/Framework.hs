{-# LANGUAGE OverloadedStrings #-}

module Framework
  ( Name
  , Passed
  , Failed
  , Required(..)
  , TestSuite
  , ResultTree
  , TestTree(..)
  , TestResult(..)
  , runTestTree
  , generateReport
  , checkResultsAndExit
  )
where

import           Data.Bifunctor
import           Data.Maybe
import           System.Console.ANSI
import           System.Exit
import           UnliftIO
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Builder  as BB

type Name = String
type Passed = Int
type Failed = Int

data Required = Required | Optional deriving (Eq, Ord, Show)
data TestResult = TestPassed | TestFailed String deriving (Eq, Ord, Show)
data TestTree summary result = TestCase Name Required result
                             | TestTree Name summary [TestTree summary result]
                             deriving (Eq, Ord, Show)
type TestSuite = TestTree () (IO TestResult)
type ResultTree = TestTree (Passed, Failed) TestResult

runTestTree :: TestSuite -> IO ResultTree
runTestTree (TestCase name required action) = do
  result <- action `catchAny` (pure . TestFailed . displayException)
  case result of
    TestPassed -> do
      setSGR [SetColor Foreground Dull Green]
      putStrLn ("  ✔ " <> name)
    TestFailed message -> do
      setSGR [SetColor Foreground Dull Red]
      putStrLn ("  ✗ " <> name)
      putStrLn (unlines . fmap ("    " <>) . lines $ message)
  setSGR []
  pure (TestCase name required result)
runTestTree (TestTree name () subtests) = do
  putStrLn name
  results <- traverse runTestTree subtests
  pure (TestTree name (accumulateTotals results) results)

accumulateTotals :: [ResultTree] -> (Passed, Failed)
accumulateTotals = bimap sum sum . unzip . map totals
 where
  totals (TestCase _ _ TestPassed    ) = (1, 0)
  totals (TestCase _ _ (TestFailed _)) = (0, 1)
  totals (TestTree _ t _             ) = t

failingTests :: ResultTree -> Maybe ResultTree
failingTests result@(TestCase _    Required (TestFailed _)) = Just result
failingTests (       TestCase _    Required TestPassed    ) = Nothing
failingTests (       TestCase _    Optional _             ) = Nothing
failingTests (TestTree name _ tree) = case catMaybes (failingTests <$> tree) of
  []    -> Nothing
  tree' -> Just (TestTree name (accumulateTotals tree') tree')

generateReport :: String -> ResultTree -> LB.ByteString
generateReport title results = BB.toLazyByteString
  (  "<html><head><meta charset='UTF-8'><title>"
  <> BB.stringUtf8 title
  <> "</title></head><body>"
  <> "</body>"
  <> reportResultTree 1 results
  <> "</html>"
  )
 where
  reportResultTree _ TestCase{} = ""
  reportResultTree level (TestTree name (passed, failed) subtrees) =
    (  ("<h" <> BB.intDec level <> " id='" <> makeAnchor name <> "'>")
      <> BB.stringUtf8 name
      <> ("</h" <> BB.intDec level <> ">")
      )
      <> "<table><thead><tr><th>Test</th><th>Passed</th><th>Failed</th></tr></thead>"
      <> (  "<tbody>"
         <> mconcat (reportResult <$> subtrees)
         <> (  "<tr><th>Summary</th>"
            <> ("<td>" <> BB.intDec passed <> "</td>")
            <> ("<td>" <> BB.intDec failed <> "</td>")
            <> "</tr>"
            )
         <> "</tbody>"
         )
      <> "</table>"
      <> mconcat (reportResultTree (level + 1) <$> subtrees)
  reportResult (TestTree name (passed, failed) _) =
    "<tr>"
      <> ("<td><a href='#" <> makeAnchor name <> "'>" <> BB.stringUtf8 name <> "</a></td>")
      <> ("<td>" <> BB.intDec passed <> "</td>")
      <> ("<td>" <> BB.intDec failed <> "</td>")
      <> "</tr>"
  reportResult (TestCase name _ TestPassed) =
    "<tr><td>" <> BB.stringUtf8 name <> "</td><td>✔</td><td></td></tr>"
  reportResult (TestCase name _ (TestFailed _)) =
    "<tr><td>" <> BB.stringUtf8 name <> "</td><td></td><td></td>✗</tr>"
  makeAnchor = BB.stringUtf8 . filter (`elem` alphabetic)
  alphabetic = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']

checkResultsAndExit :: ResultTree -> IO ()
checkResultsAndExit results = do
  putStrLn ""
  let (passed, failed) = totals results
  putStrLn (show passed <> " passed / " <> show failed <> " failed")
  putStrLn ""
  case failingTests results of
    Nothing -> exitSuccess
    Just _  -> exitFailure

 where
  totals (TestTree _ accumulatedTotals _             ) = accumulatedTotals
  totals (TestCase _ _                 TestPassed    ) = (1, 0)
  totals (TestCase _ _                 (TestFailed _)) = (0, 1)
