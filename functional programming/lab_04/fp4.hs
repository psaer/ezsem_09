-- https://wiki.haskell.org/Functor-Applicative-Monad_Proposal#Missing_superclasses
import Control.Monad       (ap)

data FunMonad a = FunMonad{ fun :: () -> a }

instance (Show fun) => Show (FunMonad fun) where
  show (FunMonad fun) = show "FunMonad " ++ (show $ fun ())

instance Functor FunMonad where
    fmap function functor = FunMonad (\ () -> function $ (fun functor) ())

instance Applicative FunMonad where
    pure x = FunMonad (\() -> x)
    (<*>) = ap

instance Monad FunMonad where
    return a  = FunMonad ( \() -> a )
    m >>= k = k (fun m ())  
    fail = error

test = do
  fm <- FunMonad (\() -> 42)
  return fm >>= (\v -> FunMonad $ \() -> v*42)