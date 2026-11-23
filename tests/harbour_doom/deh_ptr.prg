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
#include "info.ch"

STATIC deh_codeptrs := {}

FUNCTION DEH_PointerInit()
    LOCAL i
    MEMVAR states

    deh_codeptrs := Array( NUMSTATES )
    FOR i := 1 TO NUMSTATES
        deh_codeptrs[ i ] := states[ i ]:action
    NEXT
RETURN NIL

FUNCTION DEH_PointerStart( oCtx, cLine )
    LOCAL nAt
    LOCAL nFrame
    MEMVAR states

    nAt := At( "Frame", cLine )
    IF nAt == 0
        nAt := At( "frame", Lower( cLine ) )
    ENDIF
    IF nAt == 0
        DEH_Warning( oCtx, "Parse error on section start" )
        RETURN NIL
    ENDIF
    nFrame := Int( Val( SubStr( cLine, nAt + 5 ) ) )
    IF nFrame < 0 .OR. nFrame >= NUMSTATES
        DEH_Warning( oCtx, "Invalid frame number: " + hb_ntos( nFrame ) )
        RETURN NIL
    ENDIF
RETURN states[ nFrame + 1 ]

FUNCTION DEH_PointerParseLine( oCtx, cLine, oState )
    LOCAL aAsg
    LOCAL nVal

    IF oState == NIL
        RETURN NIL
    ENDIF
    aAsg := DEH_ParseAssignment( cLine )
    IF aAsg == NIL
        DEH_Warning( oCtx, "Failed to parse assignment" )
        RETURN NIL
    ENDIF
    IF hb_stricmp( aAsg[ 1 ], "Codep frame" ) != 0
        DEH_Warning( oCtx, "Unknown variable name '" + aAsg[ 1 ] + "'" )
        RETURN NIL
    ENDIF
    nVal := Int( Val( aAsg[ 2 ] ) )
    IF nVal < 0 .OR. nVal >= NUMSTATES
        DEH_Warning( oCtx, "Invalid state '" + hb_ntos( nVal ) + "'" )
        RETURN NIL
    ENDIF
    oState:action := deh_codeptrs[ nVal + 1 ]
RETURN NIL
