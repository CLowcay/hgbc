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
  , runTestSuite
  , generateReport
  , checkResultsAndExit
  )
where

import           Data.Bifunctor
import           Data.Maybe
import           Data.Time
import           System.Console.ANSI
import           System.Exit
import           System.FilePath
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

runTestSuite :: TestSuite -> IO ResultTree
runTestSuite = runTestTree Nothing

runTestTree :: Maybe String -> TestSuite -> IO ResultTree
runTestTree parent (TestCase name required action) = do
  let displayName = maybe name (</> name) parent
  result <- action `catchAny` (pure . TestFailed . displayException)
  case result of
    TestPassed -> do
      setSGR [SetColor Foreground Dull Green]
      putStrLn ("  ✔ " <> displayName)
    TestFailed message -> do
      setSGR [SetColor Foreground Dull Red]
      putStrLn ("  ✗ " <> displayName)
      putStrLn (unlines . fmap ("    " <>) . lines $ message)
  setSGR []
  pure (TestCase name required result)
runTestTree parent (TestTree name () subtests) = do
  putStrLn name
  results <- traverse (runTestTree (Just (maybe name (</> name) parent))) subtests
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

generateReport :: String -> UTCTime -> UTCTime -> ResultTree -> LB.ByteString
generateReport title testTime reportTime results = BB.toLazyByteString
  (  "<!DOCTYPE html><html><head><meta charset='UTF-8'>"
  <> style
  <> "<title>"
  <> BB.stringUtf8 title
  <> "</title></head><body>"
  <> reportResultTree Nothing 1 results
  <> "</body></html>"
  )
 where
  time =
    (\x -> "<time>" <> x <> "</time>") . BB.stringUtf8 . formatTime defaultTimeLocale "%F %T%z"
  reportResultTree _ _ TestCase{} = ""
  reportResultTree parent level (TestTree name (passed, failed) subtrees) =
    let
      nextParent = Just (maybe name (</> name) parent)
      summaryClass | passed == 0 = "failed"
                   | failed == 0 = "passed"
                   | otherwise   = "mixed"
    in
      (  ("<h" <> BB.intDec level <> " id='" <> makeAnchor parent name <> "'>")
      <> BB.stringUtf8 name
      <> ("</h" <> BB.intDec level <> ">")
      )
      <> (if isNothing parent
           then
             "<div id=reportTime><div>Test run at</div>"
             <> time testTime
             <> "<div>Report generated at</div>"
             <> time reportTime
             <> "</div>"
           else "<p><a class=smallLink href='#" <> makeAnchor parent "" <> "'>Up one level</a>"
         )
      <> "<p><table><thead><tr><th>Test</th><th>Passed</th><th>Failed</th><th>Required For CI pass</th></tr></thead>"
      <> ("<tbody>" <> mconcat (reportResult nextParent <$> subtrees) <> "</tbody>")
      <> (  "<tfoot><tr class="
         <> summaryClass
         <> "><th>Summary</th>"
         <> makeSummary passed failed
         <> "<td></td></tr></tfoot>"
         )
      <> "</table>"
      <> mconcat (reportResultTree nextParent (level + 1) <$> subtrees)
  makeSummary passed failed =
    let total = passed + failed
        passedTag | failed == 0 = "<td class=passed>"
                  | passed == 0 = "<td class=failed>"
                  | otherwise   = "<td class=mixed>"
        failedTag = if failed > 0 then "<td class=failed>" else "<td class=passed>"
    in  (passedTag <> BB.intDec passed <> " / " <> BB.intDec total <> "</td>")
          <> (failedTag <> BB.intDec failed <> "</td>")
  reportResult parent (TestTree name (passed, failed) _) =
    ("<tr><td><a href='#" <> makeAnchor parent name <> "'>" <> BB.stringUtf8 name <> "</a></td>")
      <> makeSummary passed failed
      <> "<td></td></tr>"
  reportResult _ (TestCase name required TestPassed) =
    "<tr class=passed><td>"
      <> BB.stringUtf8 name
      <> "</td><td>✔</td><td></td><td>"
      <> testStatus required
      <> "</td></tr>"
  reportResult _ (TestCase name required (TestFailed _)) =
    "<tr class=failed><td>"
      <> BB.stringUtf8 name
      <> "</td><td></td><td>✗</td><td>"
      <> testStatus required
      <> "</td></tr>"
  makeAnchor parent = BB.stringUtf8 . filter (`elem` alphabetic) . maybe id (</>) parent
  alphabetic = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']
  testStatus Required = "YES"
  testStatus Optional = "NO"
  style =
    "\n<style>\n"
      <> BB.stringUtf8
           (unlines
             [ "th { text-align: left; }"
             , "table { border-collapse: collapse; }"
             , "td, th { border: 1px solid black; padding: 0.25ch 1ch; }"
             , ".failed { color: red; }"
             , ".passed { color: green; }"
             , ".mixed {color: darkorange; }"
             , "a.smallLink { font-size: small; }"
             , "#reportTime { white-space: nowrap; display: grid; grid-template-columns: min-content min-content; column-gap: 1ch; align-items: end; }"
             , "#reportTime time {grid-column: 2; }"
             , "time { font-weight: bold; }"
             ]
           )
      <> "\n</style>\n"

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
