#!/usr/bin/env bash

# Some tests for 'darcs printer (the output formating)'

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch a
darcs add a
darcs rec -a -m add

env
# clear all output formating environment variables
for e in DARCS_DONT_ESCAPE_ISPRINT DARCS_USE_ISPRINT\
         DARCS_DONT_ESCAPE_8BIT\
         DARCS_DONT_ESCAPE_EXTRA DARCS_ESCAPE_EXTRA\
         DARCS_DONT_ESCAPE_TRAILING_SPACES\
         DARCS_DONT_COLOR DARCS_ALWAYS_COLOR DARCS_ALTERNATIVE_COLOR\
         DARCS_DONT_ESCAPE_ANYTHING; do
    unset $e
done
env

# make sure the locale is c
export LC_ALL=C

test_line () {
    rm -f a
    echo $1 > a
    darcs whatsnew | fgrep $2
}


# First check escaping and coloring.  Use whatsnew, since that is the
# most common use of escapes.

# test the defaults
# - no color to pipe
# - don't escape 7-bit ASCII printables, \n,\t and space (we can't test \n)
# - escape control chars with ^
# - escape other chars with \xXX
test_line " !#%&',-0123456789:;<=>"\
          " !#%&',-0123456789:;<=>"
test_line "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"\
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
test_line "\`abcdefghijklmnopqrstuvwxyz"\
          "\`abcdefghijklmnopqrstuvwxyz"
test_line "\t\"\$()*+./?\@[\\]^{|}"\
          "\t\"\$()*+./?\@[\\]^{|}"
# skip ^@ and ^Z since they make darcs treat the file as binary
# don't put any space control chars at end of line
# ascii control chars are escaped with ^
test_line $(printf '\x01\x02\x03\x04\x05\x06\x07\x08\x0B\x0C\x0D\x0E')\
          '[_^A_][_^B_][_^C_][_^D_][_^E_][_^F_][_^G_][_^H_][_^K_][_^L_][_^M_][_^N_]'
test_line $(printf '\x0F\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19')\
          '[_^O_][_^P_][_^Q_][_^R_][_^S_][_^T_][_^U_][_^V_][_^W_][_^X_][_^Y_]'
test_line $(printf '\x1B') '[_^[_]'
test_line $(printf '\x1C') '[_^\_]'
test_line $(printf '\x1D') '[_^]_]'
test_line $(printf '\x1E') '[_^^_]'
test_line $(printf '\x1F') '[_^__]'
test_line $(printf '\x7F') '[_^?_]'
# other chars are escaped with <U+XXXX>
test_line $(printf '\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F')\
          '[_<U+0080>_][_<U+0081>_][_<U+0082>_][_<U+0083>_][_<U+0084>_][_<U+0085>_][_<U+0086>_][_<U+0087>_][_<U+0088>_][_<U+0089>_][_<U+008A>_][_<U+008B>_][_<U+008C>_][_<U+008D>_][_<U+008E>_][_<U+008F>_]'
test_line $(printf '\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9A\x9B\x9C\x9D\x9E\x9F')\
          '[_<U+0090>_][_<U+0091>_][_<U+0092>_][_<U+0093>_][_<U+0094>_][_<U+0095>_][_<U+0096>_][_<U+0097>_][_<U+0098>_][_<U+0099>_][_<U+009A>_][_<U+009B>_][_<U+009C>_][_<U+009D>_][_<U+009E>_][_<U+009F>_]'
test_line $(printf '\xA0\xA1\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xAB\xAC\xAD\xAE\xAF')\
          '[_<U+00A0>_][_<U+00A1>_][_<U+00A2>_][_<U+00A3>_][_<U+00A4>_][_<U+00A5>_][_<U+00A6>_][_<U+00A7>_][_<U+00A8>_][_<U+00A9>_][_<U+00AA>_][_<U+00AB>_][_<U+00AC>_][_<U+00AD>_][_<U+00AE>_][_<U+00AF>_]'
test_line $(printf '\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBB\xBC\xBD\xBE\xBF')\
          '[_<U+00B0>_][_<U+00B1>_][_<U+00B2>_][_<U+00B3>_][_<U+00B4>_][_<U+00B5>_][_<U+00B6>_][_<U+00B7>_][_<U+00B8>_][_<U+00B9>_][_<U+00BA>_][_<U+00BB>_][_<U+00BC>_][_<U+00BD>_][_<U+00BE>_][_<U+00BF>_]'
test_line $(printf '\xC0\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xCA\xCB\xCC\xCD\xCE\xCF')\
          '[_<U+00C0>_][_<U+00C1>_][_<U+00C2>_][_<U+00C3>_][_<U+00C4>_][_<U+00C5>_][_<U+00C6>_][_<U+00C7>_][_<U+00C8>_][_<U+00C9>_][_<U+00CA>_][_<U+00CB>_][_<U+00CC>_][_<U+00CD>_][_<U+00CE>_][_<U+00CF>_]'
test_line $(printf '\xD0\xD1\xD2\xD3\xD4\xD5\xD6\xD7\xD8\xD9\xDA\xDB\xDC\xDD\xDE\xDF')\
          '[_<U+00D0>_][_<U+00D1>_][_<U+00D2>_][_<U+00D3>_][_<U+00D4>_][_<U+00D5>_][_<U+00D6>_][_<U+00D7>_][_<U+00D8>_][_<U+00D9>_][_<U+00DA>_][_<U+00DB>_][_<U+00DC>_][_<U+00DD>_][_<U+00DE>_][_<U+00DF>_]'
test_line $(printf '\xE0\xE1\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xEA\xEB\xEC\xED\xEE\xEF')\
          '[_<U+00E0>_][_<U+00E1>_][_<U+00E2>_][_<U+00E3>_][_<U+00E4>_][_<U+00E5>_][_<U+00E6>_][_<U+00E7>_][_<U+00E8>_][_<U+00E9>_][_<U+00EA>_][_<U+00EB>_][_<U+00EC>_][_<U+00ED>_][_<U+00EE>_][_<U+00EF>_]'
test_line $(printf '\xF0\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\xFA\xFB\xFC\xFD\xFE\xFF')\
          '[_<U+00F0>_][_<U+00F1>_][_<U+00F2>_][_<U+00F3>_][_<U+00F4>_][_<U+00F5>_][_<U+00F6>_][_<U+00F7>_][_<U+00F8>_][_<U+00F9>_][_<U+00FA>_][_<U+00FB>_][_<U+00FC>_][_<U+00FD>_][_<U+00FE>_][_<U+00FF>_]'
rm -rf temp1
