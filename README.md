# Module Documentation

## Module Rx.Validation

### Types

    type Result = V [String]

    newtype Validation a where
      Validation :: Observable (Result a) -> Validation a

    newtype Validator eff a b where
      Validator :: (a -> Eff eff (Observable (Result b))) -> Validator eff a b


### Type Class Instances

    instance applicativeValidation :: Applicative Validation

    instance applicativeValidator :: Applicative (Validator eff a)

    instance applyValidation :: Apply Validation

    instance applyValidator :: Apply (Validator eff a)

    instance functorValidation :: Functor Validation

    instance functorValidator :: Functor (Validator eff a)

    instance semigroupoidValidator :: Semigroupoid (Validator eff)


### Values

    (&>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d

    (<&) :: forall a b c d. (Semigroupoid a) => a c d -> a b c -> a b d

    check :: forall eff a b. (a -> Boolean) -> String -> Validator eff a a

    length :: forall eff. Number -> Validator eff String String

    match :: forall eff. Validator eff (Tuple String String) String

    maxLength :: forall eff. Number -> Validator eff String String

    minLength :: forall eff. Number -> Validator eff String String

    onlyNumbers :: forall eff. Validator eff String String

    required :: forall eff. Validator eff String String

    runValidation :: forall eff a b. Validator eff a b -> Observable a -> Eff eff (Observable (Result b))

    subscribeValidation :: forall eff a. Validation a -> (Result a -> Eff eff Unit) -> Eff eff Unit



