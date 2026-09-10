module Incentives.Eligibility where

import Data.Functor.Foldable (cata)
import Incentives.Ast (Ast, AstF (..))

data Eligibility = Eligible | NotEligible
  deriving (Eq, Ord, Show, Enum, Bounded)

evaluate :: Ast Eligibility -> Eligibility
evaluate = cata eligibilityAlgebra

eligibilityAlgebra :: AstF Eligibility Eligibility -> Eligibility
eligibilityAlgebra (PureF result) = result
eligibilityAlgebra (AndF Eligible right) = right
eligibilityAlgebra (AndF NotEligible _) = NotEligible
eligibilityAlgebra (OrF Eligible _) = Eligible
eligibilityAlgebra (OrF NotEligible right) = right
eligibilityAlgebra (NotF Eligible) = NotEligible
eligibilityAlgebra (NotF NotEligible) = Eligible
