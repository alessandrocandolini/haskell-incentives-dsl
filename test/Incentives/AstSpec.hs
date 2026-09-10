module Incentives.AstSpec where

import Data.Functor.Foldable (embed, project)
import Data.Functor.Identity (Identity (..))
import Incentives.Ast
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

newtype Tree = Tree (Ast Int) deriving (Show)

instance Arbitrary Tree where
  arbitrary = Tree <$> sized tree
    where
      tree 0 = Pure <$> arbitrary
      tree n = oneof
        [ Pure <$> arbitrary
        , And <$> subtree <*> subtree
        , Or <$> subtree <*> subtree
        , Not <$> subtree
        ]
        where
          subtree = tree (n `div` 2)

spec :: Spec
spec = describe "Ast structural instances" $ do
  prop "functor identity preserves the full tree" $ \(Tree ast) ->
    fmap id ast == ast
  prop "applicative identity preserves the full tree" $ \(Tree ast) ->
    (pure id <*> ast) == ast
  prop "applicative application agrees with leaf substitution" $ \(Tree ast) ->
    (And (Pure (+ 1)) (Not (Pure (* 2))) <*> ast)
      == And (fmap (+ 1) ast) (Not (fmap (* 2) ast))
  prop "monad right identity preserves the full tree" $ \(Tree ast) ->
    (ast >>= Pure) == ast
  prop "leaf substitution is associative" $ \(Tree ast) ->
    let f n = And (Pure n) (Not (Pure (n + 1)))
        g n = Or (Pure (n * 2)) (Pure n)
    in ((ast >>= f) >>= g) == (ast >>= (\n -> f n >>= g))
  prop "traversal with Identity preserves the full tree" $ \(Tree ast) ->
    runIdentity (traverse Identity ast) == ast
  prop "project and embed round-trip" $ \(Tree ast) ->
    embed (project ast) == ast
  it "traversal visits leaves left-to-right, including under Not" $
    traverse (\n -> ([n], n + 1)) (And (Pure (1 :: Int)) (Not (Or (Pure 2) (Pure 3))))
      `shouldBe` ([1, 2, 3], And (Pure 2) (Not (Or (Pure 3) (Pure 4))))
