module CourseScalpel.ProgramPageSpec where

import           Data.Maybe                (fromJust)
import           Data.Text.IO              (readFile)
import           Prelude                   hiding (readFile)
import           Test.Hspec
import           Text.HTML.Scalpel         (attrs, hasClass, scrapeStringLike,
                                            (//), (@:))

import qualified CourseScalpel.ProgramPage as ProgramPage

spec :: SpecWith ()
spec =
  describe "programPageScraper" $
    it "scrapes program 6cddd correctly" $ do

     markup <- readFile "test/markup/program-6cddd.html"
     let epage = fromJust $ scrapeStringLike markup ProgramPage.scraper

     ProgramPage.name <$> epage `shouldBe`
       Right "Civilingenjör i datateknik"

     let eUrls = ProgramPage.courseUrls <$> epage
     let mExpectedURLs = scrapeStringLike markup $
           attrs "href" (("div" @: [hasClass "programplan"]) // "a")

     length <$> eUrls `shouldBe` pure (length $ fromJust mExpectedURLs)
