module Rx.Validation where

import Control.Monad.Eff
import Data.Validation (V(), invalid, runV)
import Data.Tuple
import qualified Data.String as S
import qualified Data.String.Regex as R
import Rx.Observable

type Result = V [String] 

newtype Validator eff a b = Validator (a -> (Eff eff (Observable (Result b))))

runValidation :: forall eff a b. Validator eff a b -> Observable a -> (Eff eff (Observable (Result b)))
runValidation (Validator v) s = switchLatest <$> (unwrap $ v <$> s)

instance functorValidator :: Functor (Validator eff a) where
  (<$>) f (Validator v) = Validator $ \val -> ((<$>) ((<$>) f)) <$> v val

instance applyValidator :: Apply (Validator eff a) where
  (<*>) (Validator a) (Validator b) = Validator \val -> do
    x <- (a val)
    y <- (b val)
    return $ combineLatest (<*>) x y 

instance applicativeValidator :: Applicative (Validator eff a) where
  pure x = Validator \_ -> return $ return $ pure x

instance semigroupoidValidator :: Semigroupoid (Validator eff) where
  (<<<) (Validator v2) (Validator v1) = Validator \val -> do
    x <- (v1 val)
    switchLatest <$> (unwrap $ resF <$> x)
      where resF = runV (return <<< just <<< invalid) v2

instance categoryValidator :: Category (Validator eff) where
  id = Validator (return <<< return <<< pure)

infixr 3 &>
(&>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
(&>) = (>>>)

infixl 3 <&
(<&) :: forall a b c d. (Semigroupoid a) => a c d -> a b c -> a b d
(<&) = (<<<)

check :: forall eff a b. (a -> Boolean) -> String -> Validator eff a a
check f err = Validator $ return <<< just <<< check'
  where check' v = if f v then pure v else invalid [err]

required :: forall eff. Validator eff String String
required = check required' "required"
  where required' "" = false
        required' v  = true

onlyNumbers :: forall eff. Validator eff String String
onlyNumbers = check isNumbers "format"
  where isNumbers = R.test numberRegex
        numberRegex = R.regex
          "^\\d*$"
          { unicode:    false
          , sticky:     false
          , multiline:  false
          , ignoreCase: false
          , global:     false
          }

length :: forall eff. Number -> Validator eff String String
length n = check (\s -> S.length s == n) "length"

minLength :: forall eff. Number -> Validator eff String String
minLength n = check (\s -> S.length s >= n) "min-length"

maxLength :: forall eff. Number -> Validator eff String String
maxLength n = check (\s -> S.length s <= n) "max-length"

match :: forall eff. Validator eff (Tuple String String) String
match = Validator $ return <<< just <<< match'
  where match' (Tuple x y) = if x == y then pure x else invalid ["no-match"]


-- A type for combining results
newtype Validation a = Validation (Observable (Result a))

instance functorValidation :: Functor Validation where
  (<$>) f (Validation v) = Validation $ (<$>) f <$> v

instance applyValidation :: Apply Validation where
  (<*>) (Validation x) (Validation y) = Validation $ combineLatest (<*>) x y

instance applicativeValidation :: Applicative Validation where
  pure = Validation <<< just <<< pure

subscribeValidation :: forall eff a. Validation a -> (Result a -> Eff eff Unit) -> Eff eff Unit
subscribeValidation (Validation s) f = subscribe s f

