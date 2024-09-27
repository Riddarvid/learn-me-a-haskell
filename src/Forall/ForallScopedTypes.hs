{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Forall.ForallScopedTypes () where

-- Denna kod kompilerar inte och vi får felmeddelandet
-- "Could not match expected type b1 with actual type b."
-- Med explicit forall blir det tydligt att vi säger att val ska kunna ha vilken typ som helst,
-- eftersom den ska ha typen b för alla b.
-- Men detta leder till problem eftersom f x returnerar något av typen b,
-- men res ska kunna returnera vad som helst. I felmeddelandet blir typen av res b1
-- för att särskilja den fårn b, eftersom det faktiskt är olika typer.
-- Problemet blir alltså att vi försöker binda en variabel av typ b1 till ett värde
-- av typen b.
-- test1 :: forall a b. (a -> b) -> a -> b
-- test1 f x = res
--   where
--     res :: forall b. b
--     res = f x

-- Här får vi samma problem som i det övre exemplet, dvs. att b och b1 är olika typer.
-- Min förklaring till detta är att vi på något sätt kan tänka oss ett implicit forall
-- vid varje signatur om vi använder ScopedTypeVariables men inte har skrivit ut något forall.
-- Samma resonemang tror jag kan användas om vi inte använder ScopedTypeVariables.
-- test2 :: (a -> b) -> a -> b
-- test2 f x = res
--   where
--     res :: b
--     res = f x

-- Samma problem här och det är en ganska märklig definition.
-- Gissar återigen på implicit forall.
-- test3 :: (a -> b) -> a -> b
-- test3 f x = res
--   where
--     res :: forall b. b
--     res = f x

-- Denna version kompilerar!
-- Detta beror på att huvudsignaturen är polymorfisk och fungerar för alla typer a, b.
-- res blir däremot satt till samma typ som b, vilket gör att den matchar värdet f x.
test4 :: forall a b. (a -> b) -> a -> b
test4 f x = res
  where
    res :: b
    res = f x
