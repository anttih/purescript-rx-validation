Applicative style form validation with PureScript and RxJs
----------------------------------------------------------

[Module documentation](/anttih/purescript-rx-validation/blob/master/API.md)

This package introduces two datatypes for working with form validation.
`Validator` is for validating a single form field. It is a newtype wrapper
around a function from the input value to the validation result `Observable`:

```haskell
newtype Validator eff a b = Validator (a -> (Eff eff (Observable (Result b))))
```

This type is a `Semigroup` and an `Applicative`, so you can compose:


```haskell
zipCode :: forall eff a. Validator eff String String
zipCode = minLength 5 *> maxLength 10 *> onlyNumbers
```

All of the failures will be concatted to the `Result`, if *all* of the
validators succeed, the `Result` will hold the input value. `Result` is

```haskell
type Result = Data.Validation.V [String] 
```

The failures are plain strings for now.

`Validator` is also a `Category`, so you can use composition. Improving from
the previous one:
  
```haskell
zipCode = required >>> (minLength 5 *> maxLength 10 *> onlyNumbers)
```

This would first check that the input is non-empty, and only then proceed to
the other validators. There's also aliases for (>>>) and (<<<) with lower
operator precedence:

```haskell
zipCode = required &> minLength 5 *> maxLength 10 *> onlyNumbers
```

These validators didn't do any effects so why is there `Eff` in the type? Well,
Ajax for one. You might also want to do other DOM effects in the validators,
such as show progress. I'd like to replace the `Eff` and `Observable` with a
single type variable (turning it into a Kleisli arrow, I think) but so far
haven't been able to do so. This package should in the best case work as well
with promises or continuations. Any suggestions on this is more than welcome.

You can run the validator with an `Observable` of input values and get an
`Observable` of results:

```haskell
main = do
  zipCodeV <- runValidator zipCode zipCodeInputs
```

### Combining validation results

You may want to eventually combine all the field validators and do something
when all of them are valid, like actually send the data to the server. For this
there is a newtype wrapper around `Observable (Result a)`:

```haskell
newtype Validation a = Validation (Observable (Result a))
```

`runValidator` mentioned above returns these wrapped observables. Validate the
whole form and do something with the result like so:

```haskell

type Address = String
type ZipCode = String

data Address = Address Street ZipCode

main = do
  streetV <- runValidator street streetInputs
  zipCodeV <- runValidator zipCode zipCodeInputs
  subscribeValidation (Address <$> streetV <*> zipCodeV) doSomethingWithAddress
```

