module Darcs.Test.Patch.Examples2
    ( mergeExamples, commuteExamples, tripleExamples
    , realPatchLoopExamples
    )
    where

import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.Merge ( Merge )
import Darcs.Patch.Prim ( PrimPatchBase(..), FromPrim )
import Darcs.Patch.Prim.V1.Core ( Prim(FP), FilePatchType(Hunk) )
import Darcs.Patch.FileName ( FileName, fp2fn )

import Darcs.Witnesses.Ordered ( (:>), (:\/:) )
import Darcs.Witnesses.Sealed ( Sealed(..), Sealed2, seal2, unseal2 )

import qualified Data.ByteString as B ( ByteString )
import qualified Data.ByteString.Char8 as BC ( pack )

import Darcs.Test.Patch.QuickCheck
    ( Tree(..), RepoModel(..), WithStartState(..)
    , TreeWithFlattenPos(..)
    , commutePairFromTree, commuteTripleFromTree
    , mergePairFromCommutePair, commutePairFromTWFP
    , canonizeTree
    )

tripleExamples :: (FromPrim p, Merge p, Invert p, PrimPatchBase p, PrimOf p ~ Prim) => [Sealed2 (p :> p :> p)]
tripleExamples = [commuteTripleFromTree seal2 $
                   WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] })
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "g"]))
                     (SeqTree (FP (fp2fn "./file") (Hunk 2 [] [BC.pack "j"]))
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "s"])) NilTree)))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "e"])) NilTree))
                  ,commuteTripleFromTree seal2 $
                   WithStartState (RepoModel { rmFileName = fp2fn "./file",
                                               rmFileContents = [BC.pack "j"] })
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "s"]))
                     (ParTree
                      (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "j"] [])) NilTree)
                      (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "j"] [])) NilTree)))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "j"] [])) NilTree))
                  ]


mergeExamples :: (FromPrim p, Merge p, Invert p, PrimPatchBase p, PrimOf p ~ Prim) => [Sealed2 (p :\/: p)]
mergeExamples = map (unseal2 (mergePairFromCommutePair seal2)) commuteExamples

commuteExamples :: (FromPrim p, Merge p, PrimPatchBase p, PrimOf p ~ Prim) => [Sealed2 (p :> p)]
commuteExamples = [
                   commutePairFromTWFP seal2 $
                   WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] })
                   (TWFP 3
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "h"])) NilTree)
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "b"]))
                       (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "f"]))
                         (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"]))
                           (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "f"] [])) NilTree)))))),
                   commutePairFromTWFP seal2 $
                   WithStartState
                   (RepoModel { rmFileName = fp2fn "./file",
                                rmFileContents = [BC.pack "f",BC.pack "s",BC.pack "d"] })
                   (TWFP 3
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 3 [BC.pack "d"] [])) NilTree)
                     (ParTree
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f"] [])) NilTree)
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f"] []))
                        (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "s",BC.pack "d"] []))
                          (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"])) NilTree)))))),
{-                   commutePairFromTWFP seal2 $
                   WithStartState
                   (RepoModel { rmFileName = fp2fn "./file",
                                rmFileContents = [BC.pack "f",BC.pack "u",
                                                  BC.pack "s",BC.pack "d"] })
                   (TWFP 5
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 5 [] [BC.pack "x"]))
                      (SeqTree (FP (fp2fn "./file") (Hunk 4 [BC.pack "d"] [])) NilTree))
                     (ParTree
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f",BC.pack "u"] [])) NilTree)
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f"] []))
                       (SeqTree (FP(fp2fn "./file") (Hunk 1 [BC.pack "u",BC.pack "s",BC.pack "d"] []))
                        (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "a"]))
                         (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "a"] [])) NilTree))))))),-}
                   commutePairFromTree seal2 $
                   WithStartState (RepoModel { rmFileName = fp2fn "./file",
                                          rmFileContents = [BC.pack "n",BC.pack "t",BC.pack "h"] })
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "n",BC.pack "t",BC.pack "h"] []))
                     NilTree)
                    (SeqTree (FP (fp2fn "./file") (Hunk 3 [BC.pack "h"] []))
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "n"] []))
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "t"] [])) NilTree)))),
                  commutePairFromTree seal2 $
                  WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] })
                  (ParTree
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "n"])) NilTree)
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "i"]))
                                (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "i"])) NilTree))),
                  commutePairFromTree seal2 $
                  WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] })
                  (ParTree
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "c"]))
                     (ParTree
                       (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "c"] [BC.pack "r"])) NilTree)
                       (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "h"]))
                        (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "d"])) NilTree))))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "f"])) NilTree)),
                  commutePairFromTWFP seal2 $
                  WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] })
                  (TWFP 1
                  (ParTree
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "t"])) NilTree)
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "t"])) NilTree))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "f"])) NilTree))),
                   commutePairFromTWFP seal2 $
                   WithStartState (RepoModel { rmFileName = fp2fn "./file",
                                               rmFileContents = [BC.pack "f",BC.pack " r",
                                                                 BC.pack "c",BC.pack "v"] })
                   (TWFP 4
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 3 [BC.pack "c",BC.pack "v"] []))
                        (ParTree
                         (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "r"] []))
                          (SeqTree (FP (fp2fn "fi le") (Hunk 1 [BC.pack "f"] [])) NilTree))
                         (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "f",BC.pack "r"] []))
                          (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "y"])) NilTree))))
                     (SeqTree (FP (fp2fn "./file") (Hunk 4 [BC.pack "v"] [])) NilTree))),
                   commutePairFromTree seal2 $
                   WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] })
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "z"])) NilTree)
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "f"])) NilTree)
                     (ParTree
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "r"])) NilTree)
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "d"])) NilTree))))
                 , commutePairFromTree seal2 $
                   WithStartState (RepoModel { rmFileName = fp2fn "./file",
                                               rmFileContents = [BC.pack "t",BC.pack "r",
                                                                 BC.pack "h"] })
                   (ParTree
                    (ParTree
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "t",BC.pack "r",BC.pack "h"] []))
                              NilTree)
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "o"])) NilTree))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "t"] []))
                     (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "h"] [])) NilTree)))
                 , commutePairFromTWFP seal2 $
                   WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] }) $
                   TWFP 2
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "h"])) NilTree)
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "y"]))
                     (SeqTree (FP (fp2fn "./file") (Hunk 2 [] [BC.pack "m"]))
                      (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"])) NilTree))))
                 , commutePairFromTree seal2 $
                 WithStartState (RepoModel {rmFileName = fp2fn "./file",rmFileContents = [] })
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "p"]))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "p"] []))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "c"])) NilTree)))
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "z"])) NilTree))
                 , commutePairFromTree seal2 $
                 WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] })
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "j" ]))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "j"] [])) NilTree))
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"])) NilTree))
                 , commutePairFromTree seal2 $
                 WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] })
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "v"])) NilTree)
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "j" ]))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 [BC.pack "j"] [])) NilTree)))
                 , commutePairFromTree seal2 $
                 WithStartState (RepoModel { rmFileName = fp2fn "./file",
                                             rmFileContents = [BC.pack "x",BC.pack "c"] })
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "h"]))
                   (ParTree
                    (SeqTree (FP (fp2fn "./file") (Hunk 3 [BC.pack "c"] [])) NilTree)
                    (SeqTree (FP (fp2fn "./file") (Hunk 2 [BC.pack "x"] []))
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "j"])) NilTree))))
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] [BC.pack "l"])) NilTree))
                 , commutePairFromTree seal2 $
                 WithStartState (RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] })
                 (ParTree
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] (packStringLetters "s"))) NilTree)
                  (SeqTree (FP (fp2fn "./file") (Hunk 1 [] (packStringLetters "k")))
                   (SeqTree (FP (fp2fn "./file") (Hunk 1 (packStringLetters "k") []))
                    (SeqTree (FP (fp2fn "./file") (Hunk 1 [] (packStringLetters "m")))
                     (SeqTree (FP (fp2fn "./file") (Hunk 1 (packStringLetters "m") [])) NilTree)))))
                 ]

packStringLetters :: String -> [B.ByteString]
packStringLetters = map (BC.pack . (:[]))

realPatchLoopExamples :: [Sealed (WithStartState RepoModel (Tree Prim))]
realPatchLoopExamples =
    [Sealed (WithStartState (RepoModel { rmFileName = fx, rmFileContents = [] })
     $ canonizeTree
     (ParTree
      (SeqTree (FP fx (Hunk 1 [] (packStringLetters "pkotufogbvdabnmbzajvolwviqebieonxvcvuvigkfgybmqhzuaaurjspd")))
       (ParTree
        (SeqTree (FP fx (Hunk 47 (packStringLetters "qhzu") (packStringLetters "zafybdcokyjskcgnvhkbzpysaafnjjhcstgrczplxsfwagmh")))
         (ParTree
          (ParTree
           NilTree
           (ParTree
            (ParTree
             (ParTree
              (SeqTree (FP fx (Hunk 15 (packStringLetters "mbzajvolwviqebieonxvcvuvigkfgyb") (packStringLetters "vujnxnhvybvpouyciaabszfmgssezlwwjgnethvrpnfrkubphzvdgymjjoacppqps")))
               (ParTree
                NilTree
                (ParTree
                 (SeqTree (FP fx (Hunk 40 (packStringLetters "ssezlwwjgnethvrpnfrkubphzvdgymjjoacppqpsmzafybdcokyjskcgnvhkbz") (packStringLetters "wnesidpccwoiqiichxaaejdsyrhrusqljlcoro")))
                  (ParTree
                   (ParTree
                    (SeqTree (FP fx (Hunk 12 (packStringLetters "abnvujnxnhvybvpouyciaabszfmgwnesidpccwoiqii") (packStringLetters "czfdhqkipdstfjycqaxwnbxrihrufdeyneqiiiafwzlmg"))) NilTree)
                    NilTree)
                   NilTree))
                 (SeqTree (FP fx (Hunk 25 [] (packStringLetters "dihgmsotezucqdgxczvcivijootyvhlwymbiueufnvpwpeukmskqllalfe"))) NilTree))))
              (SeqTree (FP fx (Hunk 56 (packStringLetters "yjskcgnvhkbzpysaafnjjhcstgrczplxsfwagmhaaurjsp") (packStringLetters "xldhrutyhcyaqeezwujiguawfyawjjqlirxshjddvq"))) NilTree))
             (SeqTree (FP fx (Hunk 20 [] (packStringLetters "ooygwiyogqrqnytixqtmvdxx")))
              (SeqTree (FP fx (Hunk 26 (packStringLetters "yogqrqnytixqtmvdxxvolwviqebieonxvcvuvigkfgybmzafybdcokyjskcgnvhkbz") (packStringLetters "akhsmlbkdxnvfoikmiatfbpzdrsyykkpoxvvddeaspzxe")))
               (SeqTree (FP fx (Hunk 39 [] (packStringLetters "ji")))
                (ParTree
                 NilTree
                 (ParTree
                  NilTree
                  (ParTree
                   (ParTree
                    NilTree
                    (SeqTree (FP fx (Hunk 26 (packStringLetters "akhsmlbkdxnvfjioikmiatfbpzdrsyykkpoxvvddeaspzxepysaafnjjhcstgrczplxs") (packStringLetters "onjbhddskcj")))
                     (SeqTree (FP fx (Hunk 39 [] (packStringLetters "fyscunxxxjjtyqpfxeznhtwvlphmp"))) NilTree)))
                   (ParTree
                    NilTree
                    (SeqTree (FP fx (Hunk 44 [] (packStringLetters "xcchzwmzoezxkmkhcmesplnjpqriypshgiqklgdnbmmkldnydiy")))
                     (ParTree
                      NilTree
                      (SeqTree (FP fx (Hunk 64 (packStringLetters "plnjpqriypshgiqklgdnbmmkldnydiymiatfbpzdrsyykkpoxvvddeaspzxepysaafn") (packStringLetters "anjlzfdqbjqbcplvqvkhwjtkigp"))) NilTree)))))))))))
            (ParTree
             NilTree
             NilTree)))
          NilTree))
        NilTree))
      (ParTree
       NilTree
       (SeqTree (FP fx (Hunk 1 [] (packStringLetters "ti")))
        (SeqTree (FP fx (Hunk 1 (packStringLetters "t") (packStringLetters "ybcop")))
         (SeqTree (FP fx (Hunk 2 [] (packStringLetters "dvlhgwqlpaeweerqrhnjtfolczbqbzoccnvdsyqiefqitrqneralf")))
          (SeqTree (FP fx (Hunk 15 [] (packStringLetters "yairbjphwtnaerccdlfewujvjvmjakbc")))
           (SeqTree (FP fx (Hunk 51 [] (packStringLetters "xayvfuwaiiogginufnhsrmktpmlbvxiakjwllddkiyofyfw")))
            (ParTree
             NilTree
             NilTree)))))))))]

fx :: FileName
fx = fp2fn "./F"
