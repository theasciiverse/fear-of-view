{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Group where

class Monoid g => Grp g where
    neg :: g -> g
    zero :: g
    zero = mempty

class Action a b where
    (+^) :: a -> b -> b
instance Monoid m => Action m m where
    (+^) = mappend
