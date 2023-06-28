
In learn-you-a-haskell, MonadPlus is described as a class for data types that are both monads and monoids. It also describes the function `guard` as having the type signature 

`guard :: MonadPlus m => Bool -> m ()

On Hoogle, however, the signature is listed as `guard :: Alternative f => Bool -> f ()` 

My guess is that the book was written before Alternative was a thing, an therefore MonadPlus was used. It seems that Alternative is more narrow, since it only requires the data type to be an applicative functor, not a monad.

## MonadPlus vs. Monoid

It is reasonable that these are different classes since Monoid is defined over data types of kind * while MonadPlus is defined over data types of kind * -> * .

There are two viable laws for how MonadPlus should behave, but no agreed upon standard in the community.

## Alternative vs. MonadPlus

`Applicative f => Alternative f`

`(Alternative m, Monad m) => MonadPlus m`

It seems that before Applicative was made a superclass of Monad, the signature for MonadPlus was `Monad m => MonadPlus m`, so Alternative and MonadPlus were not related. Now, however, it seems that they are.

This is also reflected by the fact that the minimal complete definition for MonadPlus is nothing. `mzero` defaults to the corresponding `empty` from Alternative. `mplus` defaults to the corresponding `<|>` from Alternative. So Alternative is simply a superclass of MonadPlus with the additional requirement that m is a monad. 

## Summary

Monad is a stronger claim than Applicative
Alternative is a stronger claim than Applicative
(MonadPlus m) is basically a shorthand for writing (Alternative m, Monad m)

The difference between

`instance Monoid (f a) where`

and

`instance Alternative f where`

is that with Monoid, we can choose which a's the instance should be defined for. With Alternative, it has to be defined for all a's, or rather the implementation cannot depend on the type of a.