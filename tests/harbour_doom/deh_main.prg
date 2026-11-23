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

STATIC deh_initialized := .F.
STATIC deh_allow_extended_strings
STATIC deh_allow_long_strings
STATIC deh_allow_long_cheats
STATIC deh_apply_cheats

#include "deh_main.ch"
#include "deh_io.ch"

INIT PROCEDURE init_deh_main
    deh_allow_extended_strings := .F.
    deh_allow_long_strings := .F.
    deh_allow_long_cheats := .F.
    deh_apply_cheats := .T.
    deh_initialized := .F.
RETURN

FUNCTION DEH_AllowLongStrings()
RETURN deh_allow_long_strings

FUNCTION DEH_AllowLongCheats()
RETURN deh_allow_long_cheats

FUNCTION DEH_AllowExtendedStrings()
RETURN deh_allow_extended_strings

FUNCTION DEH_ApplyCheats()
RETURN deh_apply_cheats

FUNCTION DEH_SetAllowLongStrings( lVal )
    deh_allow_long_strings := ( lVal == .T. )
RETURN NIL

FUNCTION DEH_SetAllowLongCheats( lVal )
    deh_allow_long_cheats := ( lVal == .T. )
RETURN NIL

FUNCTION DEH_SetAllowExtendedStrings( lVal )
    deh_allow_extended_strings := ( lVal == .T. )
RETURN NIL

STATIC FUNCTION DehCleanString( s )
    IF s == NIL
        RETURN ""
    ENDIF
    DO WHILE Len( s ) > 0 .AND. ( Left( s, 1 ) == " " .OR. Left( s, 1 ) == Chr( 9 ) )
        s := SubStr( s, 2 )
    ENDDO
    DO WHILE Len( s ) > 0 .AND. ( Right( s, 1 ) == " " .OR. Right( s, 1 ) == Chr( 9 ) .OR. Right( s, 1 ) == Chr( 13 ) )
        s := Left( s, Len( s ) - 1 )
    ENDDO
RETURN s

FUNCTION DEH_ParseAssignment( line )
    LOCAL nEq
    LOCAL cName
    LOCAL cValue

    IF line == NIL
        RETURN NIL
    ENDIF
    nEq := At( "=", line )
    IF nEq == 0
        RETURN NIL
    ENDIF
    cName := DehCleanString( Left( line, nEq - 1 ) )
    cValue := DehCleanString( SubStr( line, nEq + 1 ) )
RETURN { cName, cValue }

STATIC FUNCTION DehIsWhitespace( s )
    LOCAL i
    LOCAL c

    IF s == NIL
        RETURN .T.
    ENDIF
    FOR i := 1 TO Len( s )
        c := SubStr( s, i, 1 )
        IF c != " " .AND. c != Chr( 9 ) .AND. c != Chr( 13 )
            RETURN .F.
        ENDIF
    NEXT
RETURN .T.

STATIC FUNCTION DehFirstWord( s )
    LOCAL nSp

    s := DehCleanString( s )
    nSp := At( " ", s )
    IF nSp == 0
        nSp := At( Chr( 9 ), s )
    ENDIF
    IF nSp == 0
        RETURN s
    ENDIF
RETURN Left( s, nSp - 1 )

STATIC FUNCTION DehParseComment( cComment )
    IF At( "*allow-long-strings*", cComment ) > 0
        deh_allow_long_strings := .T.
    ENDIF
    IF At( "*allow-long-cheats*", cComment ) > 0
        deh_allow_long_cheats := .T.
    ENDIF
    IF At( "*allow-extended-strings*", cComment ) > 0
        deh_allow_extended_strings := .T.
    ENDIF
RETURN NIL

STATIC PROCEDURE DEH_Init()
    MEMVAR myargc
    MEMVAR myargv

    IF M_CheckParm( "-nocheats" ) > 0
        deh_apply_cheats := .F.
    ENDIF
    DEH_PointerInit()
    deh_initialized := .T.
RETURN

STATIC FUNCTION DehCheckSignatures( oCtx )
    LOCAL cLine
    LOCAL i
    LOCAL aSig := { "Patch File for DeHackEd v2.3", "Patch File for DeHackEd v3.0" }

    cLine := DEH_ReadLine( oCtx, .F. )
    IF cLine == NIL
        RETURN .F.
    ENDIF
    cLine := DehCleanString( cLine )
    FOR i := 1 TO Len( aSig )
        IF cLine == aSig[ i ]
            RETURN .T.
        ENDIF
    NEXT
RETURN .F.

STATIC PROCEDURE DEH_ParseContext( oCtx )
    LOCAL oSection
    LOCAL xTag
    LOCAL cLine
    LOCAL cName
    LOCAL lExtended

    IF ! DehCheckSignatures( oCtx )
        DEH_Error( oCtx, "This is not a valid dehacked patch file!" )
        RETURN
    ENDIF
    oSection := NIL
    xTag := NIL
    DO WHILE ! DEH_HadError( oCtx )
        lExtended := ( oSection != NIL .AND. hb_stricmp( oSection[ 1 ], "[STRINGS]" ) == 0 )
        cLine := DEH_ReadLine( oCtx, lExtended )
        IF cLine == NIL
            RETURN
        ENDIF
        DO WHILE Len( cLine ) > 0 .AND. ( Left( cLine, 1 ) == " " .OR. Left( cLine, 1 ) == Chr( 9 ) )
            cLine := SubStr( cLine, 2 )
        ENDDO
        IF Left( cLine, 1 ) == "#"
            DehParseComment( cLine )
            LOOP
        ENDIF
        IF DehIsWhitespace( cLine )
            oSection := NIL
            xTag := NIL
            LOOP
        ENDIF
        IF oSection != NIL
            Eval( oSection[ 3 ], oCtx, cLine, xTag )
        ELSE
            cName := DehFirstWord( cLine )
            oSection := DEH_GetSection( cName )
            IF oSection != NIL
                IF hb_stricmp( cName, "[STRINGS]" ) == 0 .AND. ! deh_allow_extended_strings
                    oSection := NIL
                    LOOP
                ENDIF
                xTag := Eval( oSection[ 2 ], oCtx, cLine )
            ENDIF
        ENDIF
    ENDDO
RETURN

FUNCTION DEH_LoadFile( filename )
    LOCAL oCtx
    LOCAL lHad

    IF ! deh_initialized
        DEH_Init()
    ENDIF
    deh_allow_long_strings := .F.
    deh_allow_long_cheats := .F.
    deh_allow_extended_strings := .F.
    OutStd( " loading " + filename + hb_eol() )
    oCtx := DEH_OpenFile( filename )
    IF oCtx == NIL
        OutStd( "DEH_LoadFile: Unable to open " + filename + hb_eol() )
        RETURN 0
    ENDIF
    DEH_ParseContext( oCtx )
    lHad := DEH_HadError( oCtx )
    DEH_CloseFile( oCtx )
    IF lHad
        I_Error( "Error parsing dehacked file" )
    ENDIF
RETURN 1

FUNCTION DEH_LoadLump( lumpnum, allow_long, allow_error )
    LOCAL oCtx
    LOCAL lHad

    IF ! deh_initialized
        DEH_Init()
    ENDIF
    deh_allow_long_strings := ( allow_long == .T. )
    deh_allow_long_cheats := ( allow_long == .T. )
    deh_allow_extended_strings := .F.
    oCtx := DEH_OpenLump( lumpnum )
    IF oCtx == NIL
        OutStd( "DEH_LoadLump: Unable to open lump " + hb_ntos( lumpnum ) + hb_eol() )
        RETURN 0
    ENDIF
    DEH_ParseContext( oCtx )
    lHad := DEH_HadError( oCtx )
    DEH_CloseFile( oCtx )
    IF lHad .AND. allow_error != .T.
        I_Error( "Error parsing dehacked lump" )
    ENDIF
RETURN 1

FUNCTION DEH_LoadLumpByName( name, allow_long, allow_error )
    LOCAL lumpnum

    lumpnum := W_CheckNumForName( name )
    IF lumpnum < 0
        OutStd( "DEH_LoadLumpByName: '" + name + "' lump not found" + hb_eol() )
        RETURN 0
    ENDIF
RETURN DEH_LoadLump( lumpnum, allow_long, allow_error )

FUNCTION DEH_ParseCommandLine()
    LOCAL p
    LOCAL filename
    MEMVAR myargc
    MEMVAR myargv

    p := M_CheckParm( "-deh" )
    IF p <= 0
        RETURN NIL
    ENDIF
    p := p + 1
    DO WHILE p < myargc .AND. Left( myargv[ p + 1 ], 1 ) != "-"
        filename := D_TryFindWADByName( myargv[ p + 1 ] )
        DEH_LoadFile( filename )
        p := p + 1
    ENDDO
RETURN NIL

FUNCTION DEH_Checksum( digest )
    HB_SYMBOL_UNUSED( digest )
RETURN NIL
