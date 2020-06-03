module Text.Parsing.Expect
  ( success
  , failure
  , output
  ) where

import Prelude

import Data.Either as E

import Effect (Effect)
import Effect.Exception (throw) as Exception

import Text.Parsing.Parser (Parser, runParser)

-- Helper function for `success`, `failure`, and `expect`.
assertEqual :: forall a. Eq a => a -> a -> Effect Unit
assertEqual x y = case x == y of
  false -> Exception.throw $ "Assertion failure (==)."
  true  -> pure unit

-- Helper function for `success` and `failure`.
successful :: forall a b. Boolean -> a -> Parser a b -> Effect Unit
successful w x y = do
  z  <- pure (E.isRight $ runParser x y)
  _  <- assertEqual z w
  pure unit

-- | Asserts that the result of running a parser `p` with input `x` was successful.
success :: forall a b. a -> Parser a b -> Effect Unit
success = successful true

-- | Asserts that the result of running a parser `p` with input `x` was unsuccessful.
failure :: forall a b. a -> Parser a b -> Effect Unit
failure = successful false

-- | Asserts that the result of running a parser `p` with input `x` is `w`.
output :: forall a b. Eq b => b -> a -> Parser a b -> Effect Unit
output w x = \p -> do
  y <- pure $ runParser x p
  case y of
    (E.Left _)  -> Exception.throw $ "Assertion failure (output)."
    (E.Right z) -> assertEqual z w 
