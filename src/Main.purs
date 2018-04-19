module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (any, elem, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (unwrap)
import Data.Record (get, set)
import Data.String (length, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Debug.Trace (traceAnyA)
import Polyform.Validation (V(..), Validation(..), hoistFn, hoistFnMV, hoistFnV, runValidation)
import Type.Prelude (class IsSymbol, SProxy(..))

-- | `Join a` defines (<>) as (<<<)
-- | so later validation result
-- | overrides earlier in our case...
-- | We need this custom Endo
-- | to reverse the order.
newtype Endo a = Endo (a → a)

instance semigroupEndo :: Semigroup (Endo a) where
  append (Endo a) (Endo b) = Endo (a >>> b)

instance monoidEndo ∷ Monoid (Endo a) where
  mempty = Endo id


-- `Nothing` when not validated - I'm not sure if this is acceptable
type Field e a = Maybe (Either e a)

-- Let's build a form which takes record as an input
-- of the same "shape" as our desired output

inputForm
  ∷ ∀ a e input form n m trash1 trash2
  . IsSymbol n
  ⇒ Monad m
  ⇒ RowCons n String trash1 input
  ⇒ RowCons n (Field e a) trash2 form
  ⇒ SProxy n
  → Validation m e String a
  → Validation m (Endo (Record form)) (Record input) a
inputForm name validation = hoistFnMV \input → do
  result ← runValidation validation (get name input)
  -- | For field validation we can use something
  -- | based on purescirpt-validation `V`
  pure $ case result of
    Valid _ r → Valid (Endo $ set name (Just $ Right r)) r
    Invalid e → Invalid (Endo $ set name (Just $ Left e))

runValidation' form initial input = do
  result ← runValidation form input
  pure $ case result of
    Valid (Endo transform) a → Valid (transform initial) a
    Invalid (Endo transform) → Invalid $ transform initial

tooShort :: ∀ m err
  . Monad m
 => Int
 -> Validation m (Array (Variant (tooShort :: Tuple Int String | err))) String String
tooShort min = hoistFnV \str ->
  if length str > min
    then pure str
    else Invalid [ inj (SProxy ∷ SProxy "tooShort") (Tuple min str) ]

tooLong :: ∀ m err
  . Monad m
 => Int
 -> Validation m (Array (Variant (tooLong :: Tuple Int String | err))) String String
tooLong max = hoistFnV \str →
  if length str < max
    then pure str
    else Invalid [ inj (SProxy :: SProxy "tooLong") (Tuple max str) ]

-- Another simple validator; this one ensures that the input contains a digit.
missingDigit :: ∀ m err
  . Monad m
 => Validation m (Array (Variant (missingDigit :: String | err))) String String
missingDigit = hoistFnV \str →
  let
    chars = toCharArray str
  in
    if any (_ `elem` chars) (toCharArray "0123456789")
      then pure str
      else Invalid [ inj (SProxy :: SProxy "missingDigit") str ]

password s = inputForm s (tooLong 20 *> tooShort 4 *> missingDigit)
password1 = password (SProxy ∷ SProxy "password1")
password2 = password (SProxy ∷ SProxy "password2")

-- | Simple form which validates only fields
form = ({p1: _, p2: _} <$> password1 <*> password2) >>> hoistFnV \{ p1, p2} →
  if p1 /= p2
    then
      let
        err = inj (SProxy ∷ SProxy "mismatch") (Tuple p1 p2)
      in
        Invalid $ Endo (\r → r { password2 = case r.password2 of
          Nothing → Just (Left [err])
          Just (Right _) → Just (Left [err])
          Just (Left errs) → Just (Left (err : errs)) })
    else
      Valid (Endo id) p1

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  r ← runValidation' form { password1: Nothing, password2: Nothing } { password1: "est", password2: "lkasdlkfalsdj" }

  r' ← runValidation' form { password1: Nothing, password2: Nothing } { password1: "longenough89", password2: "longenough8" }
  traceAnyA $ r'

  r' ← runValidation' form { password1: Nothing, password2: Nothing } { password1: "longenough8", password2: "longenough8" }
  traceAnyA $ r'
