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
#include "fileio.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

#include "doomtype.ch"
#include "m_misc.ch"

FUNCTION M_MakeDirectory( path )
    MakeDir( path )
RETURN NIL

FUNCTION M_FileExists( filename )
    LOCAL nH

    IF hb_FileExists( filename )
        RETURN .T.
    ENDIF
    nH := FOpen( filename, FO_READ )
    IF nH != F_ERROR
        FClose( nH )
        RETURN .T.
    ENDIF
RETURN hb_DirExists( filename )

FUNCTION M_FileLength( handle )
    LOCAL nSaved
    LOCAL nLength

    nSaved := FSeek( handle, 0, FS_RELATIVE )
    nLength := FSeek( handle, 0, FS_END )
    FSeek( handle, nSaved, FS_SET )
RETURN nLength

FUNCTION M_WriteFile( name, source, length )
    LOCAL nH
    LOCAL nCount

    nH := FCreate( name )
    IF nH == F_ERROR
        RETURN .F.
    ENDIF
    IF length == NIL
        length := Len( source )
    ENDIF
    nCount := FWrite( nH, source, length )
    FClose( nH )
    IF nCount < length
        RETURN .F.
    ENDIF
RETURN .T.

FUNCTION M_ReadFile( name, buffer )
    IF ! hb_FileExists( name )
        I_Error( "Couldn't read file " + name )
    ENDIF
    buffer := hb_MemoRead( name )
    IF buffer == NIL
        I_Error( "Couldn't read file " + name )
    ENDIF
RETURN Len( buffer )

FUNCTION M_TempFile( s )
    LOCAL tempdir

#ifdef __PLATFORM__WINDOWS
    tempdir := GetEnv( "TEMP" )
    IF Empty( tempdir )
        tempdir := "."
    ENDIF
#else
    tempdir := "/tmp"
#endif
RETURN M_StringJoin( tempdir, DIR_SEPARATOR_S, s, NIL )

FUNCTION M_StrToInt( str, result )
    LOCAL c
    LOCAL n
    LOCAL i
    LOCAL ch

    c := AllTrim( str )
    IF Len( c ) >= 2 .AND. Lower( Left( c, 2 ) ) == "0x"
        result := hb_HexToNum( SubStr( c, 3 ) )
        RETURN .T.
    ENDIF
    IF Left( c, 1 ) == "0" .AND. Len( c ) > 1
        n := 0
        FOR i := 2 TO Len( c )
            ch := SubStr( c, i, 1 )
            IF ch < "0" .OR. ch > "7"
                EXIT
            ENDIF
            n := n * 8 + ( Asc( ch ) - Asc( "0" ) )
        NEXT
        IF i > Len( c )
            result := n
            RETURN .T.
        ENDIF
    ENDIF
    result := Int( Val( c ) )
RETURN .T.

FUNCTION M_ExtractFileBase( path, dest )
    LOCAL src
    LOCAL filename
    LOCAL nLen
    LOCAL ch

    src := hb_FNameNameExt( path )
    filename := src
    nLen := 0
    dest := ""
    DO WHILE nLen < Len( src )
        ch := SubStr( src, nLen + 1, 1 )
        IF ch == "."
            EXIT
        ENDIF
        IF nLen >= 8
            OutStd( "Warning: Truncated '" + filename + "' lump name to '" + dest + "'." + hb_eol() )
            EXIT
        ENDIF
        dest := dest + Upper( ch )
        nLen := nLen + 1
    ENDDO
RETURN dest

FUNCTION M_ForceUppercase( text )
    text := Upper( text )
RETURN text

FUNCTION M_StrCaseStr( haystack, needle )
    LOCAL nPos

    nPos := hb_AtI( needle, haystack )
    IF nPos == 0
        RETURN NIL
    ENDIF
RETURN SubStr( haystack, nPos )

FUNCTION M_StringDuplicate( orig )
    IF orig == NIL
        I_Error( "Failed to duplicate string (length 0)" + hb_eol() )
    ENDIF
RETURN orig

FUNCTION M_StringReplace( haystack, needle, replacement )
RETURN StrTran( haystack, needle, replacement )

FUNCTION M_StringCopy( dest, src, dest_size )
    IF dest_size < 1
        RETURN .F.
    ENDIF
    dest := Left( src, dest_size - 1 )
RETURN Len( src ) < dest_size

FUNCTION M_StringConcat( dest, src, dest_size )
    LOCAL nOff
    LOCAL cTail := ""
    LOCAL lOk

    nOff := Len( dest )
    IF nOff > dest_size
        nOff := dest_size
    ENDIF
    lOk := M_StringCopy( @cTail, src, dest_size - nOff )
    dest := Left( dest, nOff ) + cTail
RETURN lOk

FUNCTION M_StringStartsWith( s, prefix )
RETURN Len( s ) > Len( prefix ) .AND. Left( s, Len( prefix ) ) == prefix

FUNCTION M_StringEndsWith( s, suffix )
RETURN Len( s ) >= Len( suffix ) .AND. Right( s, Len( suffix ) ) == suffix

FUNCTION M_StringJoin( s )
    LOCAL cRes
    LOCAL i
    LOCAL v

    cRes := iif( s == NIL, "", s )
    FOR i := 2 TO PCount()
        v := hb_PValue( i )
        IF v == NIL
            EXIT
        ENDIF
        cRes += v
    NEXT
RETURN cRes

STATIC FUNCTION MiscFormat( s, p1, p2, p3, p4 )
    LOCAL cOut
    LOCAL aArgs
    LOCAL i
    LOCAL cTok
    LOCAL xArg
    LOCAL nArg

    cOut := iif( s == NIL, "", s )
    aArgs := {}
    IF PCount() >= 2 .AND. p1 != NIL
        AAdd( aArgs, p1 )
    ENDIF
    IF PCount() >= 3 .AND. p2 != NIL
        AAdd( aArgs, p2 )
    ENDIF
    IF PCount() >= 4 .AND. p3 != NIL
        AAdd( aArgs, p3 )
    ENDIF
    IF PCount() >= 5 .AND. p4 != NIL
        AAdd( aArgs, p4 )
    ENDIF

    nArg := 1
    i := 1
    DO WHILE i <= Len( cOut ) .AND. nArg <= Len( aArgs )
        IF SubStr( cOut, i, 1 ) == "%" .AND. i < Len( cOut )
            cTok := SubStr( cOut, i + 1, 1 )
            xArg := aArgs[ nArg ]
            IF cTok == "s" .OR. cTok == "S"
                cOut := Left( cOut, i - 1 ) + hb_ValToStr( xArg ) + SubStr( cOut, i + 2 )
                i := i + Len( hb_ValToStr( xArg ) )
                nArg := nArg + 1
                LOOP
            ELSEIF cTok == "d" .OR. cTok == "i" .OR. cTok == "u"
                cOut := Left( cOut, i - 1 ) + hb_ntos( Int( Val( hb_ValToStr( xArg ) ) ) ) + SubStr( cOut, i + 2 )
                i := i + Len( hb_ntos( Int( Val( hb_ValToStr( xArg ) ) ) ) )
                nArg := nArg + 1
                LOOP
            ELSEIF cTok == "x" .OR. cTok == "X"
                cOut := Left( cOut, i - 1 ) + hb_NumToHex( Int( Val( hb_ValToStr( xArg ) ) ) ) + SubStr( cOut, i + 2 )
                nArg := nArg + 1
                LOOP
            ENDIF
        ENDIF
        i := i + 1
    ENDDO
RETURN cOut

FUNCTION M_vsnprintf( buf, buf_len, s, p1, p2, p3, p4 )
RETURN M_snprintf( @buf, buf_len, s, p1, p2, p3, p4 )

FUNCTION M_snprintf( buf, buf_len, s, p1, p2, p3, p4 )
    LOCAL cOut
    LOCAL nResult

    IF buf_len < 1
        RETURN 0
    ENDIF
    cOut := MiscFormat( s, p1, p2, p3, p4 )
    IF Len( cOut ) >= buf_len
        cOut := Left( cOut, buf_len - 1 )
        nResult := buf_len - 1
    ELSE
        nResult := Len( cOut )
    ENDIF
    buf := cOut
RETURN nResult

#ifdef __PLATFORM__WINDOWS
FUNCTION M_OEMToUTF8( oem )
RETURN MOemToUtf8( oem )
#else
FUNCTION M_OEMToUTF8( oem )
RETURN oem
#endif

#ifdef __PLATFORM__WINDOWS
#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapiitm.h"
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

HB_FUNC( MOEMTOUTF8 )
{
#ifdef _WIN32
    const char * oem;
    unsigned int len;
    wchar_t * tmp;
    char * result;
    int nRes;

    oem = hb_parc( 1 );
    if( oem == NULL )
    {
        hb_retc_null();
        return;
    }
    len = ( unsigned int ) strlen( oem ) + 1;
    tmp = ( wchar_t * ) malloc( len * sizeof( wchar_t ) );
    MultiByteToWideChar( CP_OEMCP, 0, oem, ( int ) len, tmp, ( int ) len );
    result = ( char * ) malloc( len * 4 );
    nRes = WideCharToMultiByte( CP_UTF8, 0, tmp, ( int ) len, result, ( int ) ( len * 4 ), NULL, NULL );
    free( tmp );
    if( nRes > 0 )
        hb_retclen( result, nRes - 1 );
    else
        hb_retc_null();
    free( result );
#else
    hb_retc( hb_parc( 1 ) );
#endif
}
#pragma ENDDUMP
#endif
