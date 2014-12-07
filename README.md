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

    instance applyValidation :: Apply Validation

    instance applyValidator :: Apply (Validator eff a)

    instance bindValidator :: Bind (Validator eff a)

    instance functorValidation :: Functor Validation

    instance functorValidator :: Functor (Validator eff a)


### Values

    (>>) :: forall eff a. Validator eff a a -> Validator eff a a -> Validator eff a a

    check :: forall eff a b. (a -> Boolean) -> String -> Validator eff a a

    length :: forall eff. Number -> Validator eff String String

    match :: forall eff. Validator eff (Tuple String String) String

    maxLength :: forall eff. Number -> Validator eff String String

    minLength :: forall eff. Number -> Validator eff String String

    onlyNumbers :: forall eff. Validator eff String String

    required :: forall eff. Validator eff String String

    runValidation :: forall eff a b. Validator eff a b -> Observable a -> Eff eff (Observable (Result b))

    subscribeValidation :: forall eff a. Validation a -> (Result a -> Eff eff Unit) -> Eff eff Unit



