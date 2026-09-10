module Incentives.Interpreter.Lines where

import Data.List.NonEmpty (NonEmpty)
import Incentives.CheckoutSummary (Line, LineId, lineId)
import Incentives.Interpreter (Interpreter (..))

-- Keep checkout-line identity at the traversal boundary, including misses.
interpretLines
  :: Applicative m
  => Interpreter m Line input output
  -> Interpreter m (NonEmpty Line) input (NonEmpty (LineId, output))
interpretLines interpreter = Interpreter $ \checkoutLines input ->
  traverse
    (\checkoutLine ->
      (,) (lineId checkoutLine) <$> evaluate interpreter checkoutLine input)
    checkoutLines
