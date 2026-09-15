module Incentives.Eligibility where

import Data.Functor.Foldable (cata)
import Incentives.Ast (Ast, AstF (..))

data Eligibility = Eligible | NotEligible
  deriving (Eq, Ord, Show, Enum, Bounded)

fromBool :: Bool -> Eligibility
fromBool True = Eligible
fromBool False = NotEligible

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

-- Nothing means not evaluated yet, not a failed or missing upstream result.
evaluatePartial :: Ast (Maybe Eligibility) -> Maybe Eligibility
evaluatePartial = cata partialEligibilityAlgebra

partialEligibilityAlgebra :: AstF (Maybe Eligibility) (Maybe Eligibility) -> Maybe Eligibility
partialEligibilityAlgebra (PureF result) = result
partialEligibilityAlgebra (AndF (Just NotEligible) _) = Just NotEligible
partialEligibilityAlgebra (AndF _ (Just NotEligible)) = Just NotEligible
partialEligibilityAlgebra (AndF (Just Eligible) (Just Eligible)) = Just Eligible
partialEligibilityAlgebra (AndF _ _) = Nothing
partialEligibilityAlgebra (OrF (Just Eligible) _) = Just Eligible
partialEligibilityAlgebra (OrF _ (Just Eligible)) = Just Eligible
partialEligibilityAlgebra (OrF (Just NotEligible) (Just NotEligible)) = Just NotEligible
partialEligibilityAlgebra (OrF _ _) = Nothing
partialEligibilityAlgebra (NotF result) = eligibilityAlgebra . NotF <$> result
