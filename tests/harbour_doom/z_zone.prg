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

#include "z_zone.ch"

PROCEDURE Z_Init()
RETURN

FUNCTION Z_Malloc( nSize, nTag, xUser )
    LOCAL cBuf
    HB_SYMBOL_UNUSED( nTag )
    HB_SYMBOL_UNUSED( xUser )
    IF nSize == NIL .OR. nSize <= 0
        nSize := 0
    ENDIF
    cBuf := Replicate( Chr( 0 ), nSize )
RETURN cBuf

PROCEDURE Z_Free( ptr )
    HB_SYMBOL_UNUSED( ptr )
RETURN

PROCEDURE Z_FreeTags( lowtag, hightag )
    HB_SYMBOL_UNUSED( lowtag )
    HB_SYMBOL_UNUSED( hightag )
RETURN

PROCEDURE Z_DumpHeap( lowtag, hightag )
    HB_SYMBOL_UNUSED( lowtag )
    HB_SYMBOL_UNUSED( hightag )
RETURN

PROCEDURE Z_FileDumpHeap( f )
    HB_SYMBOL_UNUSED( f )
RETURN

PROCEDURE Z_CheckHeap()
RETURN

PROCEDURE Z_ChangeTag2( ptr, tag, file, line )
    HB_SYMBOL_UNUSED( ptr )
    HB_SYMBOL_UNUSED( tag )
    HB_SYMBOL_UNUSED( file )
    HB_SYMBOL_UNUSED( line )
RETURN

PROCEDURE Z_ChangeUser( ptr, user )
    HB_SYMBOL_UNUSED( ptr )
    HB_SYMBOL_UNUSED( user )
RETURN

FUNCTION Z_FreeMemory()
RETURN 0

FUNCTION Z_ZoneSize()
RETURN 0

INIT PROCEDURE init_z_zone
RETURN
