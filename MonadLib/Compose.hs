{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module MonadLib.Compose
where
  
import Data.Monoid
import MonadLib
import MonadLib.Derive

mid :: ReaderM m s => m s
mid = ask

class (Monad m, Monad n, ReaderM m s, ReaderM n t) => ComposeM m n s t | m -> s, n -> t, n s -> m where
    mcompose :: m a -> n s -> n a

(<<<) :: ComposeM m n s t => m a -> n s -> n a
(<<<) = mcompose
infixr 1 <<<

(>>>) :: ComposeM m n s t => n s -> m a -> n a
(>>>) = flip mcompose
infixl 1 >>>

{-
-- No ReaderM for (->)...
instance ComposeM ((->) s) ((->) t) s t where
    mcompose = (.)
-}

instance Monad m => ComposeM (ReaderT s m) (ReaderT t m) s t where
    mcompose m n = do
      s <- n
      lift (runReaderT s m)

instance ComposeM m n s t => ComposeM (IdT m) (IdT n) s t where
    mcompose m n = lift (mcompose (runIdT m) (runIdT n))

instance ComposeM m n s t => ComposeM (ExceptionT e m) (ExceptionT e n) s t where
    mcompose m n = do
      s <- n
      u <- lift $ mcompose (runExceptionT m) (return s)
      case u of
        Left e -> raise e
        Right x -> return x
