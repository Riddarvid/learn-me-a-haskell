module MonadExamples (
  myIfM,
  myIfA,
  myWhen,
  nothingIfSevenM,
  arrowComp
) where

-----------------

-- f needs to be applicative since we need a way to create an action of type f () that does nothing, eg. pure ().
-- This is an example where it is important that f respects the applicative laws, since otherwise,
-- pure () might have side effects.
myWhen :: Applicative f => Bool -> f () -> f ()
myWhen p action
  | p = action
  | otherwise = pure ()

--------------------------------------------

myIfM :: Monad m => m Bool -> m a -> m a -> m a
myIfM condM trueM falseM = do
  cond <- condM
  if cond -- The side effects launched by myIfM depend on the !!result!! of condM
    then trueM
    else falseM

myIfA :: Applicative f => f Bool -> f a -> f a -> f a
myIfA condA trueA falseA = myIfA' <$> condA <*> trueA <*> falseA
  where
    myIfA' cond true' false' = if cond then true' else false'
    -- In this example there is no way for us to choose the side effects from just one of trueA and falseA.
    -- We get all of them no matter what. The only thing we can control is the result of myIfA.

-- Den stora skillnaden mellan monader och applicative functors är att med en monad kan vi styra både effekten
-- och resultatet baserat på resultatet från den tidigare monaden.
-- Med applicative functors kan vi endast styra resultatet baserat på resultatet av den tidigare functorn,
-- men sidoeffekterna måste redan vara definierade i definitionen för <*>. Därför är det omöjligt att skapa en
-- ifA som beter sig på samma sätt som ifM.

-- Exempel: Vi har en action som returnerar ett värde och har som sidoeffekt att antingen launcha
-- en missil eller att inte göra det. För att kunna använda <*> måste vi redan ha två actions.
-- Detta innebär att sidoeffekterna redan måste finnas på plats när vi kombinerar dem, alltså
-- kan resultatet från den första inte påverka sidoeffekten av den andra.
-- Annat exempel, säg att vi arbetar i Maybe-monaden.

-- Here we only allow <*>
{-nothingIfSevenA :: Maybe Int -> Maybe Int
nothingIfSevenA action = g <*> action
  where
    g :: Maybe (Int -> Int)
    g = ???-}

-- What should g be? The problem is that we need g to be either a Just or a Nothing. We want this to depend
-- on the result of action. However, looking at the type signature of g, we realize that only the inner
-- function has access to the result. Therefore, whether g is a Just or a Nothing cannot depend on the
-- result. Which perfectly illustrates our issue.

-- Here we allow the use of do / >>=
nothingIfSevenM :: Maybe Int -> Maybe Int
nothingIfSevenM action = do
  res <- action
  if res == 7 then Nothing else action
--nothingIfSevenM action = action >>= (\res -> if res == 7 then Nothing else action)

-------------------

arrowComp :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
arrowComp f g x = f x >>= g
