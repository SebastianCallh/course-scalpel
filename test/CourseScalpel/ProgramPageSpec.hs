module CourseScalpel.ProgramPageSpec where

import           Data.Maybe                (fromJust)
import           Data.Text.IO              (readFile)
import           Data.Validation           (Validation (..))
import           Prelude                   hiding (readFile)
import           Test.Hspec
import           Text.HTML.Scalpel         (attrs, hasClass, scrapeStringLike,
                                            (//), (@:))

import qualified CourseScalpel.ProgramPage as ProgramPage

spec :: SpecWith ()
spec =
  describe "programPageContentScraper" $
    it "scrapes program 6cddd correctly" $ do
     markup <- readFile "test/markup/program-6cddd.html"
     let eProgramPageContent = fromJust $
           scrapeStringLike markup ProgramPage.contentScraper

     ProgramPage.contentName <$> eProgramPageContent `shouldBe`
       Success "Civilingenjör i datateknik"

     let eSpecSecs = ProgramPage.contentSpecs <$> eProgramPageContent
     let eUrls = foldMap ProgramPage.specSecUrls <$> eSpecSecs
     let mExpectedURLs = scrapeStringLike markup $
           attrs "href" (("div" @: [hasClass "programplan"]) // "a")

     let mExpectedSpecSecs = scrapeStringLike markup $
           attrs "data-specialization" (("div" @: [hasClass "programplan"]) //
                                        ("div" @: [hasClass "specialization"]))

     length <$> eUrls     `shouldBe` pure (length $ fromJust mExpectedURLs)
     length <$> eSpecSecs `shouldBe` pure (length $ fromJust mExpectedSpecSecs)
