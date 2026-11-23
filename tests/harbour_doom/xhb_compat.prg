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

FUNCTION hb_stricmp( s1, s2 )
    LOCAL a := Lower( iif( s1 == NIL, "", s1 ) )
    LOCAL b := Lower( iif( s2 == NIL, "", s2 ) )
    IF a == b
        RETURN 0
    ENDIF
RETURN iif( a < b, -1, 1 )

FUNCTION hb_strnicmp( s1, s2, nLen )
    IF nLen == NIL
        nLen := Max( Len( iif( s1 == NIL, "", s1 ) ), Len( iif( s2 == NIL, "", s2 ) ) )
    ENDIF
RETURN hb_stricmp( Left( iif( s1 == NIL, "", s1 ), nLen ), Left( iif( s2 == NIL, "", s2 ), nLen ) )

FUNCTION LONG( n )
RETURN Int( n )

/*
hb_qbit* substituem hb_bit* sem o frame PRG intermediario e sem a validacao
de tipos (argumento nao numerico vale 0). Usam HB_MAXINT (64 bits): hb_parnl
seria 32 bits no Windows e quebraria mascaras como ( n & 0xFFFFFFFF ).
hb_qLBitShift( n, k ) == n << k ; hb_qRBitShift( n, k ) == n >> k (aritmetico).
*/

/*
Implementacao PRG original de Shar (antes copiada como STATIC em varios fontes),
substituida pela HB_FUNC( SHAR ) abaixo:

STATIC FUNCTION Shar( n, nBits )
    LOCAL nDiv

    IF nBits <= 0
        RETURN n
    ENDIF
    nDiv := 2 ^ nBits
    IF n >= 0
        RETURN Int( n / nDiv )
    ENDIF
RETURN Int( ( n - nDiv + 1 ) / nDiv )
*/
#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC( HB_QBITAND )
{
   hb_retnint( hb_parnint( 1 ) & hb_parnint( 2 ) );
}

HB_FUNC( HB_QBITOR )
{
   hb_retnint( hb_parnint( 1 ) | hb_parnint( 2 ) );
}

HB_FUNC( HB_QBITXOR )
{
   hb_retnint( hb_parnint( 1 ) ^ hb_parnint( 2 ) );
}

HB_FUNC( HB_QBITNOT )
{
   hb_retnint( ~hb_parnint( 1 ) );
}

HB_FUNC( HB_QLBITSHIFT )
{
   hb_retnint( ( HB_MAXINT ) ( ( HB_MAXUINT ) hb_parnint( 1 ) << hb_parni( 2 ) ) );
}

HB_FUNC( HB_QRBITSHIFT )
{
   hb_retnint( hb_parnint( 1 ) >> hb_parni( 2 ) );
}

/* UShr( n, nBits ): shift logico de n tratado como unsigned 32 bits */
HB_FUNC( USHR )
{
   HB_U32 n = ( HB_U32 ) hb_parnint( 1 );
   int nBits = hb_parni( 2 );

   if( nBits <= 0 )
      hb_retnint( n );
   else if( nBits >= 32 )
      hb_retnint( 0 );
   else
      hb_retnint( n >> nBits );
}

/* Shar( n, nBits ): shift aritmetico (floor( n / 2^nBits )) em 64 bits */
HB_FUNC( SHAR )
{
   int nBits = hb_parni( 2 );

   if( nBits <= 0 )
      hb_itemReturn( hb_param( 1, HB_IT_ANY ) );
   else
   {
      HB_MAXINT n = hb_parnint( 1 );

      if( nBits >= 63 )
         hb_retnint( n < 0 ? -1 : 0 );
      else
         hb_retnint( n >> nBits );
   }
}
#pragma ENDDUMP
