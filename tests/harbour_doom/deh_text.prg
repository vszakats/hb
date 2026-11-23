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

#include "deh_main.ch"

STATIC FUNCTION TxtMaxStringLength( nLen )
    nLen := nLen + 1
    nLen := nLen + ( ( 4 - ( nLen % 4 ) ) % 4 )
RETURN nLen - 1

FUNCTION DEH_TextStart( oCtx, cLine )
    LOCAL nFrom
    LOCAL nTo
    LOCAL i
    LOCAL nC
    LOCAL cFrom
    LOCAL cTo
    LOCAL aTok

    aTok := hb_ATokens( AllTrim( cLine ), " " )
    IF Len( aTok ) < 3 .OR. hb_stricmp( aTok[ 1 ], "Text" ) != 0
        DEH_Warning( oCtx, "Parse error on section start" )
        RETURN NIL
    ENDIF
    nFrom := Int( Val( aTok[ 2 ] ) )
    nTo := Int( Val( aTok[ 3 ] ) )
    IF ! DEH_AllowLongStrings() .AND. nTo > TxtMaxStringLength( nFrom )
        DEH_Error( oCtx, "Replacement string is longer than the maximum possible in doom.exe" )
        RETURN NIL
    ENDIF
    cFrom := ""
    FOR i := 1 TO nFrom
        nC := DEH_GetChar( oCtx )
        IF nC < 0
            EXIT
        ENDIF
        cFrom += Chr( nC )
    NEXT
    cTo := ""
    FOR i := 1 TO nTo
        nC := DEH_GetChar( oCtx )
        IF nC < 0
            EXIT
        ENDIF
        cTo += Chr( nC )
    NEXT
    DEH_AddStringReplacement( cFrom, cTo )
RETURN NIL

FUNCTION DEH_TextParseLine( oCtx, cLine, xTag )
    HB_SYMBOL_UNUSED( oCtx )
    HB_SYMBOL_UNUSED( cLine )
    HB_SYMBOL_UNUSED( xTag )
RETURN NIL
