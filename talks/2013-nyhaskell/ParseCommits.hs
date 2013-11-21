{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
import           Data.Aeson
import           Data.Attoparsec.Number
import           Data.Hashable
import           Data.HashMap.Strict

instance (Read k, Hashable k, Eq k, Read v) => Read (HashMap k v) where
  readsPrec d r = [(fromList l,t) | ("fromList",r'') <- lex r, (l,t) <- readsPrec 11 r'']

deriving instance Read Number
deriving instance Read Value


main = return ()
