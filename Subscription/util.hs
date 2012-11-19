{-# LANGUAGE FlexibleContexts #-}

module Subscription.Util where

import Subscription.Params

import Data.Machine
import Control.Monad.State.Class

reportError :: MonadState Report m
               => Guilt
               -> String
               -> PlanT k o m a
reportError guilt msg = modify go >> stop
  where
    go (Report _ s) = Report (Just (guilt, msg)) s

changeStrategy :: MonadState Report m
                  => Strategy
                  -> PlanT k o m ()
changeStrategy strategy = modify go
  where
    go (Report e _) = Report e strategy

getCurrentStrategy :: MonadState Report m
                      => PlanT k o m Strategy
getCurrentStrategy = do
  (Report _ strategy) <- get
  return strategy