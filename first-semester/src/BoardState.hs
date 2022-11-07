module BoardState where

newtype SB sb a  = S(sb -> (a, sb))
-- apply state tranforms to the board
stApply :: SB sb a -> sb -> (a, sb)
stApply (S f) s = f s
-- return the board state
stState :: SB sb sb 
stState = S(\x -> (x,x))
-- update the state
stUpdate :: sb -> SB sb () 
stUpdate s = S(\_ -> ((),s))