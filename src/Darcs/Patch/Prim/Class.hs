module Darcs.Patch.Prim.Class
    ( PrimConstruct(..), PrimCanonize(..)
    , PrimClassify(..), PrimDetails(..)
    , PrimShow(..), showPrimFL, PrimRead(..)
    , PrimApply(..)
    , PrimPatch, PrimPatchBase(..)
    , FromPrim(..), FromPrims(..), ToFromPrim(..)
    )
    where

import Darcs.IO ( WriteableDirectory )
import Darcs.Patch.FileHunk ( FileHunk, IsHunk )
import Darcs.Patch.FileName ( FileName )
import Darcs.Patch.Format ( PatchListFormat, FileNameFormat(..) )
import Darcs.Patch.MarkupData ( MarkedUpFile )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.PopulationData ( Population )
import Darcs.Patch.ReadMonads ( ParserM )
import Darcs.Patch.Repair ( RepairToFL )
import Darcs.Patch.SummaryData ( SummDetail )
import Darcs.Witnesses.Ordered
    ( FL(..), RL, (:>), mapFL, mapFL_FL, concatFL, reverseFL, concatRL )
import Darcs.Witnesses.Sealed ( Sealed )

import Printer ( Doc, vcat )

import qualified Data.ByteString as B ( ByteString )

#include "gadts.h"

class (Patchy prim, PatchListFormat prim, IsHunk prim, RepairToFL prim
      ,PrimConstruct prim, PrimCanonize prim
      ,PrimClassify prim, PrimDetails prim
      ,PrimShow prim, PrimRead prim, PrimApply prim
      )
    => PrimPatch prim

class PrimPatch (PrimOf p) => PrimPatchBase p where
    type PrimOf (p :: PATCHKIND) :: PATCHKIND

instance PrimPatchBase p => PrimPatchBase (FL p) where
    type PrimOf (FL p) = PrimOf p

instance PrimPatchBase p => PrimPatchBase (RL p) where
    type PrimOf (RL p) = PrimOf p

class FromPrim p where
   fromPrim :: PrimOf p C(x y) -> p C(x y)

class FromPrim p => ToFromPrim p where
    toPrim :: p C(x y) -> Maybe (PrimOf p C(x y))

class FromPrims p where
    fromPrims :: FL (PrimOf p) C(x y) -> p C(x y)
    joinPatches :: FL p C(x y) -> p C(x y)

instance FromPrim p => FromPrim (FL p) where
    fromPrim p = fromPrim p :>: NilFL

instance FromPrim p => FromPrims (FL p) where
    fromPrims = mapFL_FL fromPrim
    joinPatches = concatFL

instance FromPrim p => FromPrims (RL p) where
    fromPrims = reverseFL . mapFL_FL fromPrim
    joinPatches = concatRL . reverseFL

class PrimClassify prim where
   primIsAddfile :: prim C(x y) -> Bool
   primIsAdddir :: prim C(x y) -> Bool
   primIsHunk :: prim C(x y) -> Bool
   primIsBinary :: prim C(x y) -> Bool
   primIsSetpref :: prim C(x y) -> Bool
   is_filepatch :: prim C(x y) -> Maybe FileName

class PrimConstruct prim where
   addfile :: FilePath -> prim C(x y)
   rmfile :: FilePath -> prim C(x y)
   adddir :: FilePath -> prim C(x y)
   rmdir :: FilePath -> prim C(x y)
   move :: FilePath -> FilePath -> prim C(x y)
   changepref :: String -> String -> String -> prim C(x y)
   hunk :: FilePath -> Int -> [B.ByteString] -> [B.ByteString] -> prim C(x y)
   tokreplace :: FilePath -> String -> String -> String -> prim C(x y)
   binary :: FilePath -> B.ByteString -> B.ByteString -> prim C(x y)
   primFromHunk :: FileHunk C(x y) -> prim C(x y)

class PrimCanonize prim where
   tryToShrink :: FL prim C(x y) -> FL prim C(x y)
   tryShrinkingInverse :: FL prim C(x y) -> Maybe (FL prim C(x y))

   -- | 'sortCoalesceFL' @ps@ coalesces as many patches in @ps@ as
   --   possible, sorting the results in some standard order.
   sortCoalesceFL :: FL prim C(x y) -> FL prim C(x y)

   -- | It can sometimes be handy to have a canonical representation of a given
   -- patch.  We achieve this by defining a canonical form for each patch type,
   -- and a function 'canonize' which takes a patch and puts it into
   -- canonical form.  This routine is used by the diff function to create an
   -- optimal patch (based on an LCS algorithm) from a simple hunk describing the
   -- old and new version of a file.
   canonize :: prim C(x y) -> FL prim C(x y)

   -- | 'canonizeFL' @ps@ puts a sequence of primitive patches into
   -- canonical form. Even if the patches are just hunk patches,
   -- this is not necessarily the same set of results as you would get
   -- if you applied the sequence to a specific tree and recalculated
   -- a diff.
   --
   -- Note that this process does not preserve the commutation behaviour
   -- of the patches and is therefore not appropriate for use when
   -- working with already recorded patches (unless doing amend-record
   -- or the like).
   canonizeFL :: FL prim C(x y) -> FL prim C(x y)

   join :: (prim :> prim) C(x y) -> Maybe (FL prim C(x y))


class PrimDetails prim where
   summarizePrim :: prim C(x y) -> [SummDetail]
   markupPrim :: pi -> prim C(x y)
              -> (FilePath, MarkedUpFile pi) -> (FilePath, MarkedUpFile pi)
   applyToPopPrim :: forall pi C(x y) . Eq pi => pi -> prim C(x y) -> Population pi -> Population pi

class PrimShow prim where
   showPrim :: FileNameFormat -> prim C(a b) -> Doc

showPrimFL :: PrimShow prim => FileNameFormat -> FL prim C(a b) -> Doc
showPrimFL f xs = vcat (mapFL (showPrim f) xs)

class PrimRead prim where
   readPrim :: ParserM m => FileNameFormat -> m (Sealed (prim C(x )))

class PrimApply prim where
   applyPrimFL :: WriteableDirectory m => FL prim C(x y) -> m ()
