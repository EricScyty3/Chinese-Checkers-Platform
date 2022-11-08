{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module State where
-- state monad for game board
newtype SB sb a  = S{runState :: sb -> (a, sb)}
-- apply state tranforms to the board
stApply :: SB sb a -> sb -> (a, sb)
stApply (S f) s = f s
-- return the board state
stState :: SB sb sb
stState = S(\x -> (x,x))
-- update the state, replace the current state
stUpdate :: sb -> SB sb ()
stUpdate s = S(const ((), s))
-- -- just do nothing
-- stIden :: SB sb ()
-- stIden = S((),)

instance Functor (SB sb) where 
    fmap :: (a -> b) -> SB sb a -> SB sb b
    fmap g sta = S(\s -> let (x, s') = stApply sta s 
                         in  (g x, s'))

instance Applicative (SB sb) where
    pure :: a -> SB st a 
    pure x = S(x,)

    (<*>) :: SB sb (a -> b) -> SB sb a -> SB sb b
    stf <*> sta = S(\s0 -> let (f, st1) = stApply stf s0
                               (x, st2) = stApply sta st1
                           in  (f x, st2))

instance Monad (SB sb) where 
    (>>=) :: SB sb a -> (a -> SB sb b) -> SB sb b 
    sta >>= stfab = S(\s0 -> let (x, s1) = stApply sta s0 
                                 (y, s2) = stApply (stfab x) s1 
                             in  (y, s2))