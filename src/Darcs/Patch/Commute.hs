module Darcs.Patch.Commute
    ( Commute(..),
      commuteFL, commuteFLorComplain,
      commuteRL, commuteRLFL,
      toFwdCommute, toRevCommute
    )
    where

import Darcs.Witnesses.Ordered (FL(..), RL(..), 
                                reverseFL, reverseRL,
                                (:>)(..), (:<)(..)
                               )
import Darcs.Witnesses.Sealed ( Sealed2, seal2 )

#include "gadts.h"

-- | Things that can commute.
class Commute p where
    commute :: (p :> p) C(x y) -> Maybe ((p :> p) C(x y))

-- | Swaps the ordered pair type so that commute can be
-- called directly.
toFwdCommute :: (Commute p, Commute q, Monad m)
             => ((p :< q) C(x y) -> m ((q :< p) C(x y)))
             -> (q :> p) C(x y) -> m ((p :> q) C(x y))
toFwdCommute c (x :> y) = do x' :< y' <- c (y :< x)
                             return (y' :> x')

-- | Swaps the ordered pair type from the order expected
-- by commute to the reverse order.
toRevCommute :: (Commute p, Commute q, Monad m)
             => ((p :> q) C(x y) -> m ((q :> p) C(x y)))
             -> (q :< p) C(x y) -> m ((p :< q) C(x y))
toRevCommute c (x :< y) = do x' :> y' <- c (y :> x)
                             return (y' :< x')



instance Commute p => Commute (FL p) where
    commute (NilFL :> x) = Just (x :> NilFL)
    commute (x :> NilFL) = Just (NilFL :> x)
    commute (xs :> ys) = do ys' :> rxs' <- commuteRLFL (reverseFL xs :> ys)
                            return $ ys' :> reverseRL rxs'


commuteRLFL :: Commute p => (RL p :> FL p) C(x y) -> Maybe ((FL p :> RL p) C(x y))
commuteRLFL (NilRL :> ys) = Just (ys :> NilRL)
commuteRLFL (xs :> NilFL) = Just (NilFL :> xs)
commuteRLFL (xs :> y :>: ys) = do y' :> xs' <- commuteRL (xs :> y)
                                  ys' :> xs'' <- commuteRLFL (xs' :> ys)
                                  return (y' :>: ys' :> xs'')

commuteRL :: Commute p => (RL p :> p) C(x y) -> Maybe ((p :> RL p) C(x y))
commuteRL (z :<: zs :> w) = do w' :> z' <- commute (z :> w)
                               w'' :> zs' <- commuteRL (zs :> w')
                               return (w'' :> z' :<: zs')
commuteRL (NilRL :> w) = Just (w :> NilRL)

commuteFLorComplain :: Commute p => (p :> FL p) C(x y) -> Either (Sealed2 p) ((FL p :> p) C(x y))
commuteFLorComplain (p :> NilFL) = Right (NilFL :> p)
commuteFLorComplain (q :> p :>: ps) = case commute (q :> p) of
                            Just (p' :> q') ->
                               case commuteFLorComplain (q' :> ps) of
                               Right (ps' :> q'') -> Right (p' :>: ps' :> q'')
                               Left l -> Left l
                            Nothing -> Left $ seal2 p

commuteFL :: Commute p => (p :> FL p) C(x y) -> Maybe ((FL p :> p) C(x y))
commuteFL = either (const Nothing) Just . commuteFLorComplain

instance Commute p => Commute (RL p) where
    commute (xs :> ys) = do fys' :> xs' <- commuteRLFL (xs :> reverseRL ys)
                            return (reverseFL fys' :> xs')

