module Incentives.AstInterpreterSpec where

import Data.Functor.Identity (Identity, runIdentity)
import Incentives.Ast (Ast (..))
import Incentives.Eligibility (Eligibility (..))
import qualified Incentives.Eligibility as Eligibility
import Incentives.Interpreter (Interpreter (..))
import Incentives.Interpreter.Ast (interpretAst, minimumWitness, minimumPartialWitness)
import Incentives.Interpreter.TwoStage (interpretAstTwoStage)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "AST interpreter composition" $
    it "substitutes a supplied leaf interpreter's result tree" $ do
      let leaf = Interpreter $ \() ruleName -> pure (Not (Pure (ruleName, NotEligible)))
      runIdentity (evaluate (interpretAst leaf) () (And (Pure "a") (Pure "b")))
        `shouldBe` And (Not (Pure ("a" :: String, NotEligible))) (Not (Pure ("b", NotEligible)))

  describe "Two-stage AST interpretation" $ do
    it "interprets only surviving rules and retains their pure supporting evidence" $ do
      let firstPass :: Interpreter Identity () String (Ast (Either (String, Eligibility) String))
          firstPass = Interpreter $ \() ruleName -> pure $ Pure $
            if ruleName == "known" then Left (ruleName, Eligible)
            else Right ruleName
          secondPass = Interpreter $ \() ruleName -> ([ruleName], Pure (ruleName, Eligible))
      evaluate (interpretAstTwoStage snd firstPass secondPass) ()
        (And (Or (Pure "unneeded") (Pure "known")) (Pure "needed"))
        `shouldBe` (["needed"], And (Pure ("known", Eligible)) (Pure ("needed", Eligible)))

    it "leaves an unresolved negation for the second pass" $ do
      let firstPass :: Interpreter Identity () String (Ast (Either (String, Eligibility) String))
          firstPass = Interpreter $ \() ruleName -> pure (Pure (Right ruleName))
          secondPass = Interpreter $ \() ruleName -> ([ruleName], Pure (ruleName, NotEligible))
      evaluate (interpretAstTwoStage snd firstPass secondPass) () (Not (Pure "needed"))
        `shouldBe` (["needed"], Not (Pure ("needed", NotEligible)))

    it "propagates a surviving client failure under negation" $ do
      let firstPass :: Interpreter Identity () () (Ast (Either Eligibility ()))
          firstPass = Interpreter $ \() () -> pure (Pure (Right ()))
          secondPass = Interpreter $ \() () -> fail "Upstream unavailable"
      evaluate (interpretAstTwoStage id firstPass secondPass) () (Not (Pure ()))
        `shouldThrow` anyIOException

  describe "Partial eligibility fold" $ do
    it "handles unknown values on either side of And and Or" $ do
      let values = [Nothing, Just NotEligible, Just Eligible]
          combinations = [(left, right) | left <- values, right <- values]
          apply constructor (left, right) = Eligibility.evaluatePartial (constructor (Pure left) (Pure right))
      map (apply And) combinations `shouldBe`
        [ Nothing, Just NotEligible, Nothing
        , Just NotEligible, Just NotEligible, Just NotEligible
        , Nothing, Just NotEligible, Just Eligible
        ]
      map (apply Or) combinations `shouldBe`
        [ Nothing, Nothing, Just Eligible
        , Nothing, Just NotEligible, Just Eligible
        , Just Eligible, Just Eligible, Just Eligible
        ]
      map (Eligibility.evaluatePartial . Not . Pure) values
        `shouldBe` [Nothing, Just Eligible, Just NotEligible]

    it "agrees with full evaluation when every leaf is known" $
      forAll (sized booleanTree) $ \expression ->
        Eligibility.evaluatePartial (Just <$> expression) == Just (Eligibility.evaluate expression)

    it "preserves the answer for arbitrary completions of unresolved leaves" $
      forAll (sized (treeOf partialLeaf)) $ \expression ->
        Eligibility.evaluate (snd <$> minimumPartialWitness fst expression)
          == Eligibility.evaluate (snd <$> expression)

  describe "Minimum evaluated AST" $ do
    it "retains only the failing branch of an And" $
      minimumWitness snd (And yesA noB) `shouldBe` noB
    it "chooses the smaller sufficient Or branch even when it is on the right" $
      minimumWitness snd (Or (And yesA yesB) yesC) `shouldBe` yesC
    it "keeps both branches when both are needed" $ do
      minimumWitness snd (And yesA yesB) `shouldBe` And yesA yesB
      minimumWitness snd (Or noA noB) `shouldBe` Or noA noB
    it "preserves negation around the decisive evidence" $
      minimumWitness snd (Not (And yesA noB)) `shouldBe` Not noB
    it "preserves the verdict for arbitrary Boolean trees" $
      forAll (sized booleanTree) $ \expression ->
        Eligibility.evaluate (minimumWitness id expression) == Eligibility.evaluate expression

yesA, yesB, yesC, noA, noB :: Ast (String, Eligibility)
yesA = Pure ("a", Eligible)
yesB = Pure ("b", Eligible)
yesC = Pure ("c", Eligible)
noA = Pure ("a", NotEligible)
noB = Pure ("b", NotEligible)

booleanTree :: Int -> Gen (Ast Eligibility)
booleanTree = treeOf (elements [Eligible, NotEligible])

partialLeaf :: Gen (Maybe Eligibility, Eligibility)
partialLeaf = do
  verdict <- elements [Eligible, NotEligible]
  knownResult <- arbitrary
  pure (if knownResult then Just verdict else Nothing, verdict)

treeOf :: Gen a -> Int -> Gen (Ast a)
treeOf leaf size
  | size <= 0 = Pure <$> leaf
  | otherwise = oneof
      [ treeOf leaf 0
      , And <$> child <*> child
      , Or <$> child <*> child
      , Not <$> child
      ]
  where
    child = treeOf leaf (size `div` 2)
