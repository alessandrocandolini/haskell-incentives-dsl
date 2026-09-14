{-# LANGUAGE DeriveFunctor #-}

module Incentives.Interpreter where

newtype Interpreter m context input output = Interpreter
  { evaluate :: context -> input -> m output
  }
  deriving (Functor)

-- Adapt available data to the context a rule actually needs.
contramapContext
  :: (outer -> inner)
  -> Interpreter m inner input output
  -> Interpreter m outer input output
contramapContext project interpreter = Interpreter $ \context ->
  evaluate interpreter (project context)

-- Supply a rule's context without changing its decision logic.
contramapContextM
  :: Monad m
  => (outer -> m inner)
  -> Interpreter m inner input output
  -> Interpreter m outer input output
contramapContextM supply interpreter = Interpreter $ \context input -> do
  supplied <- supply context
  evaluate interpreter supplied input

interpretMany
  :: Applicative m
  => Interpreter m context input output
  -> Interpreter m context [input] [output]
interpretMany interpreter = Interpreter $ \context ->
  traverse (evaluate interpreter context)
