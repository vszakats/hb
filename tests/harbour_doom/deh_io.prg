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

#include "deh_io.ch"

CLASS deh_context_t
    DATA filename
    DATA buffer
    DATA pos
    DATA lumpnum
    DATA linenum
    DATA last_was_newline
    DATA had_error
    METHOD New()
ENDCLASS

METHOD New() CLASS deh_context_t
    ::filename := ""
    ::buffer := ""
    ::pos := 0
    ::lumpnum := -1
    ::linenum := 0
    ::last_was_newline := .T.
    ::had_error := .F.
RETURN Self

STATIC FUNCTION DehNewContext()
RETURN deh_context_t():New()

FUNCTION DEH_OpenFile( filename )
    LOCAL oCtx

    IF Empty( filename ) .OR. ! M_FileExists( filename )
        RETURN NIL
    ENDIF
    oCtx := DehNewContext()
    oCtx:filename := filename
    oCtx:buffer := MemoRead( filename )
    IF oCtx:buffer == NIL
        oCtx:buffer := ""
    ENDIF
RETURN oCtx

FUNCTION DEH_OpenLump( lumpnum )
    LOCAL oCtx
    MEMVAR lumpinfo
    MEMVAR numlumps

    IF lumpnum < 0 .OR. lumpnum >= numlumps
        RETURN NIL
    ENDIF
    oCtx := DehNewContext()
    oCtx:lumpnum := lumpnum
    oCtx:filename := AllTrim( lumpinfo[ lumpnum + 1 ]:name )
    oCtx:buffer := W_CacheLumpNum( lumpnum, 1 )
    IF oCtx:buffer == NIL
        oCtx:buffer := ""
    ENDIF
RETURN oCtx

FUNCTION DEH_CloseFile( oCtx )
    IF oCtx == NIL
        RETURN NIL
    ENDIF
    IF oCtx:lumpnum >= 0
        W_ReleaseLumpNum( oCtx:lumpnum )
    ENDIF
RETURN NIL

FUNCTION DEH_GetChar( oCtx )
    LOCAL nLen
    LOCAL nByte
    LOCAL lLastCr

    IF oCtx == NIL
        RETURN -1
    ENDIF
    IF oCtx:last_was_newline
        oCtx:linenum := oCtx:linenum + 1
    ENDIF
    nLen := Len( oCtx:buffer )
    lLastCr := .F.
    DO WHILE .T.
        IF oCtx:pos >= nLen
            IF lLastCr
                oCtx:last_was_newline := .F.
                RETURN 13
            ENDIF
            oCtx:last_was_newline := .F.
            RETURN -1
        ENDIF
        oCtx:pos := oCtx:pos + 1
        nByte := Asc( SubStr( oCtx:buffer, oCtx:pos, 1 ) )
        IF lLastCr .AND. nByte != 10
            oCtx:pos := oCtx:pos - 1
            oCtx:last_was_newline := .F.
            RETURN 13
        ENDIF
        lLastCr := ( nByte == 13 )
        IF ! lLastCr
            EXIT
        ENDIF
    ENDDO
    oCtx:last_was_newline := ( nByte == 10 )
RETURN nByte

FUNCTION DEH_ReadLine( oCtx, lExtended )
    LOCAL cLine
    LOCAL nC
    LOCAL lEscaped

    HB_SYMBOL_UNUSED( lExtended )
    IF oCtx == NIL
        RETURN NIL
    ENDIF
    lExtended := ( lExtended == .T. )
    cLine := ""
    lEscaped := .F.
    DO WHILE .T.
        nC := DEH_GetChar( oCtx )
        IF nC < 0
            IF Empty( cLine )
                RETURN NIL
            ENDIF
            EXIT
        ENDIF
        IF lExtended .AND. nC == 92
            nC := DEH_GetChar( oCtx )
            IF nC == Asc( "n" ) .OR. nC == Asc( "N" )
                cLine += Chr( 10 )
                LOOP
            ENDIF
            IF nC == 10
                lEscaped := .T.
                LOOP
            ENDIF
            IF nC >= 0
                cLine += "\" + Chr( nC )
            ENDIF
            LOOP
        ENDIF
        IF lEscaped .AND. nC >= 0 .AND. nC != 10 .AND. ( nC == 9 .OR. nC == 32 )
            LOOP
        ENDIF
        lEscaped := .F.
        IF nC == 10
            EXIT
        ENDIF
        IF nC != 0
            cLine += Chr( nC )
        ENDIF
    ENDDO
RETURN cLine

FUNCTION DEH_Warning( oCtx, cMsg )
    LOCAL cFile
    LOCAL nLine

    cFile := iif( oCtx == NIL, "dehacked", oCtx:filename )
    nLine := iif( oCtx == NIL, 0, oCtx:linenum )
    OutStd( cFile + ":" + hb_ntos( nLine ) + ": warning: " + cMsg + hb_eol() )
RETURN NIL

FUNCTION DEH_Error( oCtx, cMsg )
    LOCAL cFile
    LOCAL nLine

    cFile := iif( oCtx == NIL, "dehacked", oCtx:filename )
    nLine := iif( oCtx == NIL, 0, oCtx:linenum )
    OutStd( cFile + ":" + hb_ntos( nLine ) + ": " + cMsg + hb_eol() )
    IF oCtx != NIL
        oCtx:had_error := .T.
    ENDIF
RETURN NIL

FUNCTION DEH_HadError( oCtx )
RETURN iif( oCtx == NIL, .F., oCtx:had_error )
