{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module MonadLib.Compose
where
  
import Data.Monoid
import MonadLib
import MonadLib.Derive

mid :: ReaderM m s => m s
mid = ask

class (Monad m, Monad n) => ComposeM m n s t | m -> s, n -> t, n s -> m where
    mcompose :: m a -> n s -> n a

(<<<) :: ComposeM m n s t => m a -> n s -> n a
(<<<) = mcompose
infixr 1 <<<

(>>>) :: ComposeM m n s t => n s -> m a -> n a
(>>>) = flip mcompose
infixl 1 >>>

instance ComposeM ((->) s) ((->) t) s t where
    mcompose = (.)

instance Monad m => ComposeM (ReaderT s m) (ReaderT t m) s t where
    mcompose m n = do
      s <- n
      lift (runReaderT s m)

derive_mcompose close open m n = do
  s <- n
  u <- lift $ mcompose (open m) (return s)
  close u

instance ComposeM m n s t => ComposeM (IdT m) (IdT n) s t where
    mcompose = derive_mcompose return runIdT

instance (ComposeM m n s t, Monoid w) => ComposeM (WriterT w m) (WriterT w n) s t where
    mcompose = derive_mcompose puts runWriterT

instance ComposeM m n s t => ComposeM (ExceptionT e m) (ExceptionT e n) s t where
    mcompose = derive_mcompose raises runExceptionT

instance ComposeM m n s t => ComposeM (StateT i m) (StateT i n) s t where
    mcompose m n = do
      s <- n
      i <- get
      (u, i') <- lift $ mcompose (runStateT i m) (return s)
      set i'
      return u

instance ComposeM m n s t => ComposeM (ChoiceT m) (ChoiceT n) s t where
    mcompose m n = do
      s <- n
      u <- lift $ mcompose (runChoiceT m) (return s)
      case u of
        Nothing -> mzero
        Just (a, m') -> return a `mplus` (mcompose m' (return s))
