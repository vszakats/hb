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

#include "sha1.ch"

CLASS sha1_context_t
    DATA h0
    DATA h1
    DATA h2
    DATA h3
    DATA h4
    DATA nblocks
    DATA buf
    DATA count
    DATA ptr
    METHOD New()
ENDCLASS

METHOD New() CLASS sha1_context_t
    ::h0 := 0
    ::h1 := 0
    ::h2 := 0
    ::h3 := 0
    ::h4 := 0
    ::nblocks := 0
    ::buf := Replicate( Chr( 0 ), 64 )
    ::count := 0
    ::ptr := NIL
RETURN Self

STATIC PROCEDURE Sha1FillZeros( digest )
    LOCAL i

    IF ValType( digest ) != "A"
        RETURN
    ENDIF
    FOR i := 1 TO 20
        IF i <= Len( digest )
            digest[ i ] := 0
        ELSE
            AAdd( digest, 0 )
        ENDIF
    NEXT
RETURN

PROCEDURE SHA1_Init( context )
    HB_SYMBOL_UNUSED( context )
RETURN

PROCEDURE SHA1_Update( context, buf, nLen )
    HB_SYMBOL_UNUSED( context )
    HB_SYMBOL_UNUSED( buf )
    HB_SYMBOL_UNUSED( nLen )
RETURN

PROCEDURE SHA1_Final( digest, context )
    HB_SYMBOL_UNUSED( context )
    Sha1FillZeros( digest )
RETURN

PROCEDURE SHA1_UpdateInt32( context, val )
    HB_SYMBOL_UNUSED( context )
    HB_SYMBOL_UNUSED( val )
RETURN

PROCEDURE SHA1_UpdateString( context, str )
    HB_SYMBOL_UNUSED( context )
    HB_SYMBOL_UNUSED( str )
RETURN
