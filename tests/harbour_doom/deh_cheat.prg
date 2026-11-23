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

FUNCTION DEH_CheatStart( oCtx, cLine )
    HB_SYMBOL_UNUSED( oCtx )
    HB_SYMBOL_UNUSED( cLine )
RETURN NIL

FUNCTION DEH_CheatParseLine( oCtx, cLine, xTag )
    LOCAL aAsg
    LOCAL oCheat
    LOCAL cSeq
    LOCAL i
    LOCAL nByte
    LOCAL cNew

    HB_SYMBOL_UNUSED( xTag )
    aAsg := DEH_ParseAssignment( cLine )
    IF aAsg == NIL
        DEH_Warning( oCtx, "Failed to parse assignment" )
        RETURN NIL
    ENDIF
    oCheat := DEH_FindCheat( aAsg[ 1 ] )
    IF oCheat == NIL
        DEH_Warning( oCtx, "Unknown cheat '" + aAsg[ 1 ] + "'" )
        RETURN NIL
    ENDIF
    cSeq := aAsg[ 2 ]
    cNew := ""
    i := 0
    DO WHILE i < Len( cSeq )
        i := i + 1
        nByte := Asc( SubStr( cSeq, i, 1 ) )
        IF nByte == 0 .OR. nByte == 0xFF
            EXIT
        ENDIF
        IF ! DEH_AllowLongCheats() .AND. i > oCheat:sequence_len
            DEH_Warning( oCtx, "Cheat sequence longer than supported by Vanilla dehacked" )
            EXIT
        ENDIF
        IF i >= MAX_CHEAT_LEN - oCheat:parameter_chars
            DEH_Error( oCtx, "Cheat sequence too long!" )
            RETURN NIL
        ENDIF
        IF DEH_ApplyCheats()
            cNew += Chr( nByte )
        ENDIF
    ENDDO
    IF DEH_ApplyCheats()
        oCheat:sequence := cNew
    ENDIF
RETURN NIL
