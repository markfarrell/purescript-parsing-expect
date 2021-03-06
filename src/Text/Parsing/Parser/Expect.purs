module Text.Parsing.Parser.Expect
  ( success
  , failure
  , output
  ) where

import Prelude

import Data.Either as E
import Data.Traversable (class Traversable, sequence)

import Effect (Effect)
import Effect.Exception (throw) as Exception

import Text.Parsing.Parser (ParserT, runParserT)

-- Helper function for `success`, `failure`, and `expect`.
assertEqual :: forall a. Eq a => a -> a -> Effect Unit
assertEqual x y = case x == y of
  false -> Exception.throw $ "Assertion failure (==)."
  true  -> pure unit

-- Helper function for `success` and `failure`.
successful :: forall a m b. Monad m => Traversable m => Boolean -> a -> ParserT a m b -> Effect Unit
successful w x y = do
  z  <- pure (E.isRight $ sequence $ runParserT x y)
  _  <- assertEqual z w
  pure unit

-- | Asserts that the result of running a parser `p` with input `x` was successful.
success :: forall a m b. Monad m => Traversable m => a -> ParserT a m b -> Effect Unit
success = successful true

-- | Asserts that the result of running a parser `p` with input `x` was unsuccessful.
failure :: forall a m b. Monad m => Traversable m => a -> ParserT a m b -> Effect Unit
failure = successful false

-- | Asserts that the result of running a parser `p` with input `x` is `w`.
output :: forall a m b. Monad m => Traversable m => Eq b => b -> a -> ParserT a m b -> Effect Unit
output w x = \p -> do
  y <- pure $ sequence $ runParserT x p
  case y of
    (E.Left _)  -> Exception.throw $ "Assertion failure (output)."
    (E.Right z) -> void $ sequence (assertEqual w <$> z)
