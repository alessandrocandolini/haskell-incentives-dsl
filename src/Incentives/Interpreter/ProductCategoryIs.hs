module Incentives.Interpreter.ProductCategoryIs where

import Data.Text (Text)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Eligibility (Eligibility, fromBool)

-- | Context is the observed category; input is the expected category.
productCategoryIs :: Applicative m => Interpreter m Text Text Eligibility
productCategoryIs = Interpreter $ \observed expected ->
  pure (fromBool (observed == expected))
