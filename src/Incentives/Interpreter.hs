{-# LANGUAGE DeriveFunctor #-}

module Incentives.Interpreter where

newtype Interpreter m context input output = Interpreter
  { evaluate :: context -> input -> m output
  }
  deriving (Functor)
