{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ScopedTypeVariables
           , TypeFamilies
           , ConstraintKinds #-}

module Container (
  Container(..)
) where 

import GHC.Exts
import Data.IntMap.Lazy as IMap
import Data.Map.Lazy as Map

-- | A proof of concept for a Container typeclass.
--   This has just been an annoying thing for me for
--   a while.

-- | Laws so far:
--   isJust (lookup k (insert c k v))
--   forall a :: key. isNothing (lookup empty a)

class Container container where
  type Contains container key :: Constraint

  insert :: (Contains container key) => 
            container key val 
         -> key
         -> val 
         -> container key val

  lookup :: (Contains container key) => 
            container key val 
         -> key 
         -> Maybe val

  empty  :: (Contains container key) => 
            container key val

  build  :: (Contains container key) =>
            [(key, val)] 
         -> container key val

instance Container Map where  
  type Contains Map key = Ord key
  
  insert c k v = Map.insert k v c

  lookup = flip Map.lookup

  empty = Map.empty

  build = Map.fromList

newtype IntMapW a v = IntMapW { unIntMapW :: IntMap v }

instance Container IntMapW where
  type Contains IntMapW val = (val ~ Int)

  insert imapW key val = IntMapW $ IMap.insert key val imap where
    imap = unIntMapW imapW

  lookup imapW key = IMap.lookup key imap where
    imap = unIntMapW imapW

  empty = IntMapW IMap.empty

  build pairs = IntMapW $ IMap.fromList pairs
