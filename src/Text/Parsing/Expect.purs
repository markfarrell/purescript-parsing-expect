module Text.Parsing.Expect
  ( success
  , failure
  , output
  ) where

import Prelude

import Effect (Effect)

import Text.Parsing.Parser (Parser)

import Text.Parsing.Parser.Expect as E

-- | Asserts that the result of running a parser `p` with input `x` was successful.
success :: forall a b. a -> Parser a b -> Effect Unit
success = E.success

-- | Asserts that the result of running a parser `p` with input `x` was unsuccessful.
failure :: forall a b. a -> Parser a b -> Effect Unit
failure = E.failure

-- | Asserts that the result of running a parser `p` with input `x` is `w`.
output :: forall a b. Eq b => b -> a -> Parser a b -> Effect Unit
output = E.output
