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

#include "deh_str.ch"

STATIC deh_replacements := {}

INIT PROCEDURE init_deh_str
    deh_replacements := {}
RETURN

FUNCTION DEH_String( s )
    LOCAL i
    LOCAL nFrom
    LOCAL nTo

    IF s == NIL
        RETURN s
    ENDIF
    FOR i := 1 TO Len( deh_replacements )
        nFrom := deh_replacements[ i, 1 ]
        nTo := deh_replacements[ i, 2 ]
        IF nFrom == s
            RETURN nTo
        ENDIF
    NEXT
RETURN s

FUNCTION DEH_AddStringReplacement( from_text, to_text )
    LOCAL i

    IF from_text == NIL .OR. to_text == NIL
        RETURN NIL
    ENDIF
    FOR i := 1 TO Len( deh_replacements )
        IF deh_replacements[ i, 1 ] == from_text
            deh_replacements[ i, 2 ] := to_text
            RETURN NIL
        ENDIF
    NEXT
    AAdd( deh_replacements, { from_text, to_text } )
RETURN NIL

FUNCTION DEH_printf( cText )
    IF cText != NIL
        OutStd( DEH_String( cText ) )
    ENDIF
RETURN NIL

FUNCTION DEH_fprintf( xHandle, cText )
    LOCAL cOut

    IF cText == NIL
        RETURN NIL
    ENDIF
    cOut := DEH_String( cText )
    IF ValType( xHandle ) == "N"
        FWrite( xHandle, cOut )
    ELSE
        OutStd( cOut )
    ENDIF
RETURN NIL

FUNCTION DEH_snprintf( buffer, nLen, cText )
    LOCAL cOut

    cOut := DEH_String( iif( cText == NIL, "", cText ) )
    IF ValType( nLen ) == "N" .AND. nLen > 0
        cOut := Left( cOut, nLen - 1 )
    ENDIF
    buffer := cOut
RETURN Len( cOut )
