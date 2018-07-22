module CourseScalpel.ProgramPageSpec where

import           Data.Either               (lefts)
import           Data.List                 (nub)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import           Data.Text.IO              (readFile)
import           Data.Validation           (Validation (..))
import           Prelude                   hiding (readFile)
import           Test.Hspec
import           Text.HTML.Scalpel         (attr, attrs, chroot, hasClass,
                                            scrapeStringLike, (//), (@:))

import           CourseScalpel.Error       (AppError (..))
import qualified CourseScalpel.ProgramPage as ProgramPage

spec :: SpecWith ()
spec = do
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
