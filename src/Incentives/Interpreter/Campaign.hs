{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Incentives.Interpreter.Campaign
  ( CampaignResult (..)
  , interpretOffering
  , interpretCampaign
  , interpretCampaigns
  ) where

import Data.Foldable (fold)
import Incentives.Ast (Ast)
import Incentives.Campaign
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (CheckoutSummary, LineId)
import Incentives.Eligibility (Eligibility (..))
import qualified Incentives.Eligibility as Eligibility
import Incentives.GrantedIncentive
import Incentives.Interpreter (Interpreter (..), interpretMany)
import Incentives.Interpreter.Rule (Context (..), EvaluatedRule, checkoutSummary, ruleEligibility)
import Incentives.Rule (Rule)

data CampaignResult = CampaignResult
  { resultCampaignId :: CampaignId
  , grantedIncentives :: [GrantedIncentive]
  -- One tree per visited gate, including rejections; Nothing denotes a gate
  -- at purchase scope. An unconditional grant has no condition to report.
  , evaluatedConditions :: [(Maybe LineId, Ast EvaluatedRule)]
  }
  deriving (Eq, Show)

-- Campaign structure depends only on supplied condition interpreters.
interpretOffering
  :: forall m target. Monad m
  => (forall scope. Interpreter m (Context scope) (Ast (Rule scope)) (Ast EvaluatedRule))
  -> Interpreter m (Context target) (Offering target)
       ([GrantedIncentive], [(Maybe LineId, Ast EvaluatedRule)])
interpretOffering conditions = Interpreter $ \context (Offering nodes) ->
  let step :: OfferingNode target -> m ([GrantedIncentive], [(Maybe LineId, Ast EvaluatedRule)])
      step (GrantLine incentive) = case context of
        LineContext _ checkoutLine -> pure ([LineGrant (Checkout.lineId checkoutLine) incentive], [])
      step (GrantPurchase incentive) = pure ([PurchaseGrant incentive], [])
      step (When condition body) = do
        evaluated <- evaluate conditions context condition
        let currentLine = case context of
              PurchaseContext _ -> Nothing
              LineContext _ checkoutLine -> Just (Checkout.lineId checkoutLine)
        (grants, children) <- if Eligibility.evaluate (ruleEligibility <$> evaluated) == Eligible
          then evaluate (interpretOffering conditions) context body
          else pure ([], [])
        pure (grants, (currentLine, evaluated) : children)
      step (ForEachLine body) =
        fold <$> traverse
          (\checkoutLine -> evaluate (interpretOffering conditions)
            (LineContext (checkoutSummary context) checkoutLine) body)
          (Checkout.lines (checkoutSummary context))
  in fold <$> traverse step nodes

interpretCampaign
  :: Monad m
  => (forall scope. Interpreter m (Context scope) (Ast (Rule scope)) (Ast EvaluatedRule))
  -> Interpreter m CheckoutSummary Campaign CampaignResult
interpretCampaign conditions = Interpreter $ \checkout campaign -> do
  (grants, trees) <- evaluate (interpretOffering conditions) (PurchaseContext checkout) (offering campaign)
  pure (CampaignResult (campaignId campaign) grants trees)

-- The caller supplies active campaigns.
interpretCampaigns
  :: Monad m
  => (forall scope. Interpreter m (Context scope) (Ast (Rule scope)) (Ast EvaluatedRule))
  -> Interpreter m CheckoutSummary [Campaign] [CampaignResult]
interpretCampaigns conditions = interpretMany (interpretCampaign conditions)
