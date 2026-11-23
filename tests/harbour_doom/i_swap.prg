/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

#include "i_swap.ch"

INIT PROCEDURE init_i_swap
RETURN

FUNCTION ISwapShort( x )
    x := ( x & 0xFFFF )
    IF x >= 32768
        x := x - 65536
    ENDIF
RETURN x

FUNCTION ISwapLong( x )
    x := ( x & 0xFFFFFFFF )
    IF x >= 2147483648
        x := x - 4294967296
    ENDIF
RETURN x

FUNCTION ISwapBytes16( x )
    LOCAL n

    n := ( x & 0xFFFF )
    n := ( ( ( n & 0x00FF ) * 256 ) | Int( ( n & 0xFF00 ) / 256 ) )
    n := ( n & 0xFFFF )
    IF n >= 32768
        n := n - 65536
    ENDIF
RETURN n
