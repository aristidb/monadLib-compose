{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

{- |
Module       : MonadLib.Compose
Copyright    : 2010 Aristid Breitkreuz
License      : BSD3
Stability    : experimental
Portability  : portable

This module provides Arrow-like monad composition for monadLib. To be more precise, it is "Category-like",
i.e. the parallels are to 'Control.Category.Category'.

'Control.Category.Category' generalises '.' and 'id' to arrows and categories. One such arrow is 'Kleisli',
which represents functions returning monadic values. Incidentally, that's equivalent to 'ReaderT'! So it
turns out that it is possible to generalise '.' and 'id' to 'ReaderT' ('id' is just 'ask'), as well as to
many monad transformer stacks that embed a 'ReaderT' inside.

The motivation to create this module was a nagging feeling when reading the documentation for @hxt@ and @HaXml@:
composing filters is very nice, but the abundance of constant arrows, and the lack of access to the very extensive
set of monad combinators, leads to duplicated effort and unwieldy code (in my humble opinion). I think it is
possible to gain similar functionality with a stack of monad transformers including 'ReaderT', and 'ComposeM',
presented here.
-}

module MonadLib.Compose
(
  mid
, ComposeM(..)
, (<<<)
, (>>>)
)
where
  
import Data.Monoid
import MonadLib
import MonadLib.Monads

-- | Alias for 'ask'. Compare with 'Control.Category.id'.
mid :: ReaderM m s => m s
mid = ask

-- | Composable monads. Compare with 'Control.Category.Category'.
-- Note that there are two different monad types involved in each instance.
class (Monad m, Monad n) => ComposeM m n s t | m -> s, n -> t, n s -> m where
    -- | Compose two monadic values from right to left. @mcompose f g@ is 
    -- comparable to @f . g@ but for monadic values. Compare with 'Control.Category..'.
    mcompose :: m a -> n s -> n a
    mcompose m n = mapply m =<< n
    
    -- | Apply a constant value to a composable monad.
    mapply :: m a -> s -> n a
    mapply m s = mcompose m (return s)

-- | Compose two monadic values from right to left. Compare with 'Control.Category.<<<'.
-- @f <<< g@ is equivalent to @mcompose f g@.
(<<<) :: ComposeM m n s t => m a -> n s -> n a
(<<<) = mcompose
infixr 1 <<<

-- | Compose two monadic values from left to right. Compare with 'Control.Category.>>>'.
-- @g >>> f@ is equivalent to @mcompose f g@.
(>>>) :: ComposeM m n s t => n s -> m a -> n a
(>>>) = flip mcompose
infixl 1 >>>

instance ComposeM ((->) s) ((->) t) s t where
    mcompose = (.)

instance Monad m => ComposeM (ReaderT s m) (ReaderT t m) s t where
    mapply m s = lift (runReaderT s m)

instance ComposeM (Reader s) (Reader t) s t where
    mcompose m n = asks $ flip runReader m . flip runReader n

x_mapply :: (MonadT xt, ComposeM m n s t, Monad (xt n)) 
            => (a -> xt n b) -> (c -> m a) -> c -> s -> xt n b
x_mapply close open m s = do
  u <- lift $ mapply (open m) s
  close u

instance ComposeM m n s t => ComposeM (IdT m) (IdT n) s t where
    mapply = x_mapply return runIdT

instance (ComposeM m n s t, Monoid w) => ComposeM (WriterT w m) (WriterT w n) s t where
    mapply = x_mapply puts runWriterT

instance ComposeM m n s t => ComposeM (ExceptionT e m) (ExceptionT e n) s t where
    mapply = x_mapply raises runExceptionT

instance ComposeM m n s t => ComposeM (StateT i m) (StateT i n) s t where
    mapply m s = do
      i <- get
      (u, i') <- lift $ mapply (runStateT i m) s
      set i'
      return u

instance ComposeM m n s t => ComposeM (ChoiceT m) (ChoiceT n) s t where
    mapply m s = do
      u <- lift $ mapply (runChoiceT m) s
      case u of
        Nothing -> mzero
        Just (a, m') -> return a `mplus` (mapply m' s)
