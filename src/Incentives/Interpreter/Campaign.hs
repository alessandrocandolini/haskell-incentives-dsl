{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Incentives.Interpreter.Campaign
  ( interpretOffering
  , interpretCampaign
  , interpretCampaigns
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import Incentives.Ast (Ast)
import Incentives.Campaign
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (CheckoutSummary)
import Incentives.Eligibility (Eligibility (..))
import Incentives.GrantedIncentive
import Incentives.Interpreter (Interpreter (..), interpretMany)
import Incentives.Interpreter.Rule (Context (..), checkoutSummary)
import Incentives.Rule (Rule)

-- Campaign structure depends only on supplied condition interpreters.
interpretOffering
  :: forall m target. Monad m
  => (forall scope. Interpreter m (Context scope) (Ast (Rule scope)) Eligibility)
  -> Interpreter m (Context target) (Offering target) [GrantedIncentive]
interpretOffering conditions = Interpreter $ \context (Offering nodes) ->
  let step :: OfferingNode target -> m [GrantedIncentive]
      step (GrantLine incentive) = case context of
        LineContext _ checkoutLine -> pure [LineGrant (Checkout.lineId checkoutLine) incentive]
      step (GrantPurchase incentive) = pure [PurchaseGrant incentive]
      step (When condition body) = do
        verdict <- evaluate conditions context condition
        if verdict == Eligible
          then evaluate (interpretOffering conditions) context body
          else pure []
      step (ForEachLine body) =
        concat . NonEmpty.toList <$> traverse
          (\checkoutLine -> evaluate (interpretOffering conditions)
            (LineContext (checkoutSummary context) checkoutLine) body)
          (Checkout.lines (checkoutSummary context))
  in concat . NonEmpty.toList <$> traverse step nodes

interpretCampaign
  :: Monad m
  => (forall scope. Interpreter m (Context scope) (Ast (Rule scope)) Eligibility)
  -> Interpreter m CheckoutSummary Campaign [(CampaignId, GrantedIncentive)]
interpretCampaign conditions = Interpreter $ \checkout campaign ->
  map (\instruction -> (campaignId campaign, instruction))
    <$> evaluate (interpretOffering conditions) (PurchaseContext checkout) (offering campaign)

-- The caller supplies active campaigns.
interpretCampaigns
  :: Monad m
  => (forall scope. Interpreter m (Context scope) (Ast (Rule scope)) Eligibility)
  -> Interpreter m CheckoutSummary [Campaign] [(CampaignId, GrantedIncentive)]
interpretCampaigns conditions = concat <$> interpretMany (interpretCampaign conditions)
