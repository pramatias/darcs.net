{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim
       ( showPrim, showPrimFL,
         adddir, addfile, binary, changepref,
         hunk, move, rmdir, rmfile, tokreplace,
         primFromHunk,
         primIsAddfile, primIsHunk, primIsBinary, primIsSetpref,
         primIsAdddir, is_filepatch,
         canonize, tryToShrink,
         sortCoalesceFL, join, canonizeFL,
         tryShrinkingInverse,
         summarizePrim, markupPrim, applyToPopPrim,
         applyPrimFL,
         readPrim,
         FromPrim(..), FromPrims(..), ToFromPrim(..),
         PrimPatch, PrimPatchBase(..)
       )
       where

import Darcs.Patch.Prim.Class
    ( PrimConstruct(..), PrimCanonize(..)
    , PrimClassify(..), PrimDetails(..)
    , PrimShow(..), showPrimFL, PrimRead(..)
    , PrimApply(..)
    , FromPrim(..), FromPrims(..), ToFromPrim(..)
    , PrimPatchBase(..), PrimPatch
    )

