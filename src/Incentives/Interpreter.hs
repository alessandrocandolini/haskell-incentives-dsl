{-# LANGUAGE DeriveFunctor #-}

module Incentives.Interpreter
  ( Interpreter (..)
  , OnContext
  , pack
  , unpack
  , contramapContextM
  , interpretMany
  ) where

import Data.Functor.Contravariant (Contravariant (..))

newtype Interpreter m context input output = Interpreter
  { evaluate :: context -> input -> m output
  }
  deriving (Functor)

newtype OnContext m input output context = OnContext
  { unpack :: Interpreter m context input output
  }

pack :: Interpreter m context input output -> OnContext m input output context
pack = OnContext

instance Contravariant (OnContext m input output) where
  contramap project (OnContext interpreter) = pack $ Interpreter $ \context ->
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
