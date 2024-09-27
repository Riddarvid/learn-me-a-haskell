
Three common uses for forall, each with their own extension:

- ScopedTypeVariables
- RankNTypes/Rank2Types
- ExistentialQuantification

## ScopedTypeVariables

Consider the following code:

````haskell
foob :: forall a b. (b -> b) -> b -> (a -> b) -> Maybe a -> b
foob postProcess onNothin onJust mval =
    postProcess val
    where
        val :: b
        val = maybe onNothin onJust mval
````

Notera att vi refererar till typvariabeln b på två ställen: i funktionsignaturen och i signaturen för hjälpfunktionen val.

ScopedTypeVariables låter oss alltså specificera typer för definitioner i where-clauses där vi vill referera till samma typ som huvudfunktionen. Vi skapara lltså ett scope där b alltid refererar till samma variabel om man inte explicit har skrivit forall b.

## RankNTypes

Vi vill kunna tala om att en funktion som tas som input till en annan funktion ska kunna vara definierad för alla värden.

## ExistentialQuantification

Vi vill kunna tala om att en datatyp är definierad oavsett typ. Ser inte riktigt ett use case för detta.