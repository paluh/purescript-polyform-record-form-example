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


-- | This is mess - we want to use Join here
-- | but validation combines subforms in order
-- | but Join defines (<>) as (<<<)
-- | so later validation result
-- | overrides earlier...
-- | So we need this hack
newtype FunA a = FunA (a → a)

instance semigroupFunA :: Semigroup (FunA a) where
  append (FunA a) (FunA b) = FunA (a >>> b)

instance monoidFunA ∷ Monoid (FunA a) where
  mempty = FunA id

type FormTransform = FunA

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
  → Validation m (FormTransform (Record form)) (Record input) a
inputForm name validation = hoistFnMV \input → do
  result ← runValidation validation (get name input)
  -- | For field validation we can use something
  -- | based on purescirpt-validation `V`
  pure $ case result of
    Valid _ r → Valid (FunA $ set name (Just $ Right r)) r
    Invalid e → Invalid (FunA $ set name (Just $ Left e))

password1 = inputForm (SProxy ∷ SProxy "password1") (tooLong 20 *> tooShort 4 *> missingDigit)
password2 = inputForm (SProxy ∷ SProxy "password2") (tooLong 20 *> tooShort 4 *> missingDigit)

-- | Simple form which validates record
form1 = {p1: _, p2: _} <$> password1 <*> password2

runValidation' form initial input = do
  result ← runValidation form input
  pure $ case result of
    Valid (FunA transform) a → transform initial
    Invalid (FunA transform) → transform initial


form2 = form1 >>> hoistFnV \{ p1, p2} →
  if p1 /= p2
    then
      let
        err = inj (SProxy ∷ SProxy "mismatch") (Tuple p1 p2)
      in
        Invalid $ FunA (\r → r { password2 = case r.password2 of
          Nothing → Just (Left [err])
          Just (Right _) → Just (Left [err])
          Just (Left errs) → Just (Left (err : errs)) })
    else
      Valid (FunA id) p1


-- | For fields we probably don't want
-- | this Validation but something based
-- | on purescript-validation V

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


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  r ← runValidation' form1 { password1: Nothing, password2: Nothing } { password1: "est", password2: "lkasdlkfalsdj" }
  traceAnyA $ r

  r' ← runValidation' form2 { password1: Nothing, password2: Nothing } { password1: "longenough89", password2: "longenough8" }
  traceAnyA $ r'
