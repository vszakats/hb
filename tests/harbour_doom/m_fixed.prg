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

#include "m_fixed.ch"

#ifndef INT_MIN
#define INT_MIN ( -2147483647 - 1 )
#define INT_MAX 2147483647
#endif

FUNCTION FixedMul( a, b )
RETURN C_FixedMul( a, b )

FUNCTION FixedDiv( a, b )
RETURN C_FixedDiv( a, b )

#pragma BEGINDUMP
#include "hbapi.h"

#ifndef FRACBITS
#define FRACBITS 16
#endif

HB_FUNC( C_FIXEDMUL )
{
    hb_retnl( ( long ) ( ( ( HB_LONGLONG ) hb_parnl( 1 ) * ( HB_LONGLONG ) hb_parnl( 2 ) ) >> FRACBITS ) );
}

HB_FUNC( C_FIXEDDIV )
{
    long a = hb_parnl( 1 );
    long b = hb_parnl( 2 );
    long abs_a = a < 0 ? -a : a;
    long abs_b = b < 0 ? -b : b;

    if( ( abs_a >> 14 ) >= abs_b )
        hb_retnl( ( a ^ b ) < 0 ? ( -2147483647 - 1 ) : 2147483647 );
    else
        hb_retnl( ( long ) ( ( ( HB_LONGLONG ) a << 16 ) / b ) );
}
#pragma ENDDUMP
