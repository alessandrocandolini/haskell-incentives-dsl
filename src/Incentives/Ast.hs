{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Incentives.Ast where

import Control.Monad (ap)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..), cata)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

data Ast a
  = Pure a
  | And (Ast a) (Ast a)
  | Or (Ast a) (Ast a)
  | Not (Ast a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Application and bind substitute leaves, preserving Boolean syntax.
instance Applicative Ast where
  pure = Pure
  (<*>) = ap

instance Monad Ast where
  Pure a >>= f = f a
  And left right >>= f = And (left >>= f) (right >>= f)
  Or left right >>= f = Or (left >>= f) (right >>= f)
  Not child >>= f = Not (child >>= f)

data AstF a r
  = PureF a
  | AndF r r
  | OrF r r
  | NotF r
  deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base (Ast a) = AstF a

instance Recursive (Ast a) where
  project (Pure a) = PureF a
  project (And left right) = AndF left right
  project (Or left right) = OrF left right
  project (Not child) = NotF child

instance Corecursive (Ast a) where
  embed (PureF a) = Pure a
  embed (AndF left right) = And left right
  embed (OrF left right) = Or left right
  embed (NotF child) = Not child

infixr 3 .&&.

(.&&.) :: Ast a -> Ast a -> Ast a
(.&&.) = And

infixr 2 .||.

(.||.) :: Ast a -> Ast a -> Ast a
(.||.) = Or

-- Render with a trailing newline, ready for putStr.
prettyAst :: Show a => Ast a -> String
prettyAst = unlines . NonEmpty.toList . cata algebra
  where
    algebra :: Show a => AstF a (NonEmpty String) -> NonEmpty String
    algebra (PureF value) = case lines (show value) of
      [] -> "" :| []
      first : rest -> first :| rest
    algebra (AndF left right) =
      "And" :| (branch "+- " "|  " left ++ branch "\\- " "   " right)
    algebra (OrF left right) =
      "Or" :| (branch "+- " "|  " left ++ branch "\\- " "   " right)
    algebra (NotF child) =
      "Not" :| branch "\\- " "   " child

    branch :: String -> String -> NonEmpty String -> [String]
    branch connector indentation (first :| rest) =
      (connector ++ first) : map (indentation ++) rest
