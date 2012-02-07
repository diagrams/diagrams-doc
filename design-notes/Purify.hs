import Data.Functor.Apply
import Control.Applicative

data Purify f a = Pure a | Impure (f a)

instance Functor f => Functor (Purify f) where
  fmap f (Pure x)   = Pure (f x)
  fmap f (Impure x) = Impure (fmap f x)

instance Apply f => Applicative (Purify f) where
  pure = Pure
  (Pure f) <*> (Pure x)     = Pure (f x)
  (Pure f) <*> (Impure x)   = Impure (f <$> x)
  (Impure f) <*> (Pure x)   = Impure (($x) <$> f)
  (Impure f) <*> (Impure x) = Impure (f <.> x)

{-

  Apply laws:

    associative composition: (.) <$> u <.> v <.> w = u <.> (v <.> w)

  Applicative laws:

    identity
        pure id <*> v = v 
    composition
        pure (.) <*> u <*> v <*> w = u <*> (v <*> w) 
    homomorphism
        pure f <*> pure x = pure (f x) 
    interchange
        u <*> pure y = pure ($ y) <*> u 


  Clear that Functor (Purify f) is lawful.

  To show: Applicative (Purify f) satisfies Applicative laws, assuming
  f satisfies Apply law.

  Identity:

    pure id <*> v
  =                       { def'n }
    Pure id <*> v
  =                       { case v }
    1.   Pure id <*> Pure x
       =                  { def'n }
         Pure x

    2.   Pure id <*> Impure x
       =                  { def'n }
         Impure (id <$> x)
       =                  { functor }
         Impure x

  Composition:

    pure (.) <*> u <*> v <*> w

  Homomorphism:  

    pure f <*> pure x
  =
    Pure f <*> Pure x
  = 
    Pure (f x)
         
  Interchange:


    u <*> pure y
  =                        { def'n }
    u <*> Pure y
  =                        { case u }
    1.   Pure u' <*> Pure y  
       = 
         Pure (u' y)
       =
         Pure ($y) <*> Pure u'
       = 
         pure ($y) <*> u

    2.   Impure u' <*> Pure y
       = 
         Impure (($y) <$> u')
       =
         Pure ($y) <*> Impure u'
       =
         pure ($y) <*> u

-}