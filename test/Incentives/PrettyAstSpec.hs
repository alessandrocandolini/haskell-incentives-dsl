module Incentives.PrettyAstSpec where

import Incentives.Ast
import Test.Hspec

data Multiline = Multiline

instance Show Multiline where
  show Multiline = "first\nsecond"

spec :: Spec
spec = describe "prettyAst" $ do
  it "renders a leaf using Show" $
    prettyAst (Pure "hello") `shouldBe` "\"hello\"\n"

  it "indents nested operators and keeps sibling connectors aligned" $
    prettyAst
      (And (Or (Pure (1 :: Int)) (Pure 2)) (Not (Not (Pure 3))))
      `shouldBe` unlines
        [ "And"
        , "+- Or"
        , "|  +- 1"
        , "|  \\- 2"
        , "\\- Not"
        , "   \\- Not"
        , "      \\- 3"
        ]

  it "indents every line of a multiline Show result" $
    prettyAst (And (Pure Multiline) (Pure Multiline))
      `shouldBe` unlines
        [ "And"
        , "+- first"
        , "|  second"
        , "\\- first"
        , "   second"
        ]
