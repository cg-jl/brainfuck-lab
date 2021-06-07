module Util where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Functor
import Data.List
import Data.Maybe

data Lens comp sub = Lens {view :: comp -> sub, update :: comp -> sub -> comp}

update' :: Lens comp sub -> sub -> comp -> comp
update' = flip . update

over :: Lens comp sub -> (sub -> sub) -> comp -> comp
over lens f a = let v = view lens a in update lens a (f v)

sndL :: Lens (a, b) b
sndL = Lens snd $ \(a, _) b -> (a, b)

fstL :: Lens (a, b) a
fstL = Lens fst $ \(_, b) a -> (a, b)

processWith :: Monad m => Lens comp sub -> StateT sub m a -> StateT comp m a
processWith (Lens view update) sa = do
  c <- get
  (o, s) <- lift $ runStateT sa (view c)
  let c' = update c s
  put c' $> o

advance :: Monad m => MaybeT (StateT [a] m) a
advance = do
  xs <- lift get
  case xs of
    [] -> fail "empty list"
    (x : xs) -> lift (put xs) $> x

runTillNothing :: Monad m => (a -> MaybeT (StateT [a] m) b) -> StateT [a] m [b]
runTillNothing f = next >>= maybe stop again
  where
    next = runMaybeT (advance >>= f)
    again x = (x :) <$> runTillNothing f
    stop = return []

-- run a composed state and use the lens to update the inner stream.
runTillNothingL :: Monad m => Lens comp [a] -> (a -> MaybeT (StateT comp m) b) -> StateT comp m [b]
runTillNothingL lens f = next >>= maybe stop again
  where
    next = processWith lens (runMaybeT advance) >>= maybe wasEmpty hasValue
      where
        wasEmpty = return Nothing
        hasValue = runMaybeT . f

    stop = return []
    again x = (x :) <$> runTillNothingL lens f

runTillNoInput :: Monad m => (a -> StateT [a] m b) -> StateT [a] m [b]
runTillNoInput f = runTillNothing (lift . f)

-- advance filtering out maybes.
runWithFilter :: Monad m => (a -> MaybeT (StateT [a] m) b) -> StateT [a] m [b]
runWithFilter f = concat <$> runTillNoInput next
  where
    next c = maybeToList <$> runMaybeT (f c)
