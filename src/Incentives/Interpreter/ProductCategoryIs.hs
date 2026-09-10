module Incentives.Interpreter.ProductCategoryIs where

import Data.Text (Text)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Match (Match, match)
import Incentives.Rule (LineRule (ProductCategoryIs))

-- | Context is the observed category; input is the expected category.
productCategoryIs :: Applicative m => Interpreter m Text Text (Match (LineRule, Text))
productCategoryIs = Interpreter $ \observed expected ->
  pure (match (observed == expected) (ProductCategoryIs expected, observed))
