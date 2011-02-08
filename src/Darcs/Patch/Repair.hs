module Darcs.Patch.Repair
    ( Repair(..), RepairToFL(..), mapMaybeSnd, Check(..) )
    where

import Darcs.IO ( WriteableDirectory(..) )
import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), mapFL, mapRL, (+>+) )
import Printer ( Doc )

import Data.Maybe ( catMaybes, listToMaybe )

#include "gadts.h"

class Check p where
    isInconsistent :: p C(x y) -> Maybe Doc
    isInconsistent _ = Nothing

-- |'Repair' and 'RepairToFL' deal with repairing old patches that were
-- were written out due to bugs or that we no longer wish to support.
-- 'Repair' is implemented by collections of patches (FL, Named, PatchInfoAnd) that
-- might need repairing.
class Repair p where
    applyAndTryToFix :: WriteableDirectory m => p C(x y) -> m (Maybe (String, p C(x y)))

-- |'RepairToFL' is implemented by single patches that can be repaired (Prim, Patch, RealPatch)
-- There is a default so that patch types with no current legacy problems don't need to
-- have an implementation.
class Apply p => RepairToFL p where
    applyAndTryToFixFL :: WriteableDirectory m => p C(x y) -> m (Maybe (String, FL p C(x y)))
    applyAndTryToFixFL p = do apply p; return Nothing

mapMaybeSnd :: (a -> b) -> Maybe (c, a) -> Maybe (c, b)
mapMaybeSnd f (Just (a,b)) = Just (a,f b)
mapMaybeSnd _ Nothing = Nothing

instance Check p => Check (FL p) where
    isInconsistent = listToMaybe . catMaybes . mapFL isInconsistent

instance Check p => Check (RL p) where
    isInconsistent = listToMaybe . catMaybes . mapRL isInconsistent

instance RepairToFL p => Repair (FL p) where
    applyAndTryToFix NilFL = return Nothing
    applyAndTryToFix (p:>:ps) = do mp <- applyAndTryToFixFL p
                                   mps <- applyAndTryToFix ps
                                   return $ case (mp,mps) of
                                            (Nothing, Nothing) -> Nothing
                                            (Just (e,p'),Nothing) -> Just (e,p'+>+ps)
                                            (Nothing, Just (e,ps')) -> Just (e,p:>:ps')
                                            (Just (e,p'), Just (es,ps')) ->
                                                Just (unlines [e,es], p'+>+ps')

