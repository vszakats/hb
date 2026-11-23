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

STATIC exit_funcs := NIL
STATIC already_quitting := .F.
STATIC firsttime_mem := .T.
STATIC mem_dump_dos622 := {}
STATIC mem_dump_win98 := {}
STATIC mem_dump_dosbox := {}
STATIC mem_dump_custom := {}
STATIC dos_mem_dump := {}

#include "doomtype.ch"
#include "i_system.ch"

CLASS atexit_listentry_t
    DATA func
    DATA run_on_error
    DATA next
    METHOD New()
ENDCLASS

#ifndef PACKAGE_NAME
#define PACKAGE_NAME "doom_hb"
#endif

#define DEFAULT_RAM 6
#define MIN_RAM     6
#define DOS_MEM_DUMP_SIZE 10



METHOD New() CLASS atexit_listentry_t
    ::func := NIL
    ::run_on_error := .F.
    ::next := NIL
RETURN Self

INIT PROCEDURE init_i_system
    LOCAL i

    exit_funcs := NIL
    already_quitting := .F.
    firsttime_mem := .T.

    mem_dump_dos622 := { 0x57, 0x92, 0x19, 0x00, 0xF4, 0x06, 0x70, 0x00, 0x16, 0x00 }
    mem_dump_win98  := { 0x9E, 0x0F, 0xC9, 0x00, 0x65, 0x04, 0x70, 0x00, 0x16, 0x00 }
    mem_dump_dosbox := { 0x00, 0x00, 0x00, 0xF1, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00 }
    mem_dump_custom := {}
    FOR i := 1 TO DOS_MEM_DUMP_SIZE
        AAdd( mem_dump_custom, 0 )
    NEXT
    dos_mem_dump := mem_dump_dos622
RETURN

STATIC PROCEDURE RunExitFunc( bFunc )
    IF ValType( bFunc ) == "B"
        Eval( bFunc )
    ENDIF
RETURN

FUNCTION I_AtExit( bFunc, lRunOnError )
    LOCAL oEntry

    oEntry := atexit_listentry_t():New()
    oEntry:func := bFunc
    oEntry:run_on_error := lRunOnError
    oEntry:next := exit_funcs
    exit_funcs := oEntry
RETURN NIL

FUNCTION I_Tactile( on, off, total )
RETURN NIL

STATIC FUNCTION AutoAllocMemory( nSize, nDefaultRam, nMinRam )
    LOCAL zonemem := NIL

    DO WHILE zonemem == NIL
        IF nDefaultRam < nMinRam
            I_Error( "Unable to allocate " + hb_ntos( nDefaultRam ) + " MiB of RAM for zone" )
        ENDIF
        nSize := nDefaultRam * 1024 * 1024
        zonemem := ISysMalloc( nSize )
        IF zonemem == NIL
            nDefaultRam := nDefaultRam - 1
        ENDIF
    ENDDO
RETURN zonemem

FUNCTION I_ZoneBase( nSize )
    LOCAL zonemem
    LOCAL nMinRam
    LOCAL nDefaultRam
    LOCAL p
    MEMVAR myargv

    p := M_CheckParmWithArgs( "-mb", 1 )
    IF p > 0
        nDefaultRam := Int( Val( myargv[ p + 1 + 1 ] ) )
        nMinRam := nDefaultRam
    ELSE
        nDefaultRam := DEFAULT_RAM
        nMinRam := MIN_RAM
    ENDIF

    zonemem := AutoAllocMemory( @nSize, nDefaultRam, nMinRam )
    OutStd( "zone memory: " + ISysPtrHex( zonemem ) + ", " + ;
        Lower( hb_NumToHex( nSize ) ) + " allocated for zone" + hb_eol() )
RETURN zonemem

FUNCTION I_PrintBanner( msg )
    LOCAL i
    LOCAL nSpaces

    nSpaces := 35 - Int( Len( msg ) / 2 )
    FOR i := 0 TO nSpaces - 1
        OutStd( " " )
    NEXT
    OutStd( msg + hb_eol() )
RETURN NIL

FUNCTION I_PrintDivider()
    OutStd( Replicate( "=", 75 ) + hb_eol() )
RETURN NIL

FUNCTION I_PrintStartupBanner( gamedescription )
    I_PrintDivider()
    I_PrintBanner( gamedescription )
    I_PrintDivider()
    OutStd( " " + PACKAGE_NAME + " is free software, covered by the GNU General Public" + hb_eol() )
    OutStd( " License.  There is NO warranty; not even for MERCHANTABILITY or FITNESS" + hb_eol() )
    OutStd( " FOR A PARTICULAR PURPOSE. You are welcome to change and distribute" + hb_eol() )
    OutStd( " copies under certain conditions. See the source for more information." + hb_eol() )
    I_PrintDivider()
RETURN NIL

FUNCTION I_ConsoleStdout()
RETURN .F.

FUNCTION I_Quit()
    LOCAL oEntry

    oEntry := exit_funcs
    DO WHILE oEntry != NIL
        RunExitFunc( oEntry:func )
        oEntry := oEntry:next
    ENDDO
    ErrorLevel( 0 )
    QUIT
RETURN NIL

STATIC PROCEDURE LogIError( cMsg )
    LOCAL nH

    nH := FOpen( "doom_debug.log", FO_READWRITE )
    IF nH == F_ERROR
        nH := FCreate( "doom_debug.log" )
    ENDIF
    IF nH != F_ERROR
        FSeek( nH, 0, FS_END )
        FWrite( nH, "I_Error: " + cMsg + hb_eol() )
        FClose( nH )
    ENDIF
RETURN

STATIC FUNCTION FormatIError( cFmt, x1, x2, x3, x4, x5 )
    LOCAL cOut
    LOCAL i
    LOCAL nLen
    LOCAL ch
    LOCAL cCode
    LOCAL aArg
    LOCAL nArg

    IF ValType( cFmt ) != "C"
        RETURN hb_ValToStr( cFmt )
    ENDIF

    aArg := { x1, x2, x3, x4, x5 }
    nArg := 1
    cOut := ""
    nLen := Len( cFmt )
    i := 1
    DO WHILE i <= nLen
        ch := SubStr( cFmt, i, 1 )
        IF ch == "%" .AND. i < nLen
            cCode := SubStr( cFmt, i + 1, 1 )
            IF cCode == "%"
                cOut += "%"
                i += 2
                LOOP
            ENDIF
            IF nArg <= 5 .AND. aArg[ nArg ] != NIL
                SWITCH cCode
                CASE "s"
                    cOut += hb_ValToStr( aArg[ nArg ] )
                    nArg++
                    i += 2
                    LOOP
                CASE "d"
                CASE "i"
                CASE "u"
                    cOut += hb_ntos( aArg[ nArg ] )
                    nArg++
                    i += 2
                    LOOP
                ENDSWITCH
            ENDIF
        ENDIF
        cOut += ch
        i++
    ENDDO
RETURN cOut

FUNCTION I_Error( error, x1, x2, x3, x4, x5 )
    LOCAL cMsg
    LOCAL oEntry
    LOCAL lExitGuiPopup

    IF already_quitting
        OutErr( "Warning: recursive call to I_Error detected." + hb_eol() )
    ELSE
        already_quitting := .T.
    ENDIF

    cMsg := FormatIError( error, x1, x2, x3, x4, x5 )
    OutErr( cMsg + hb_eol() + hb_eol() )
    LogIError( cMsg )

    oEntry := exit_funcs
    DO WHILE oEntry != NIL
        IF oEntry:run_on_error
            RunExitFunc( oEntry:func )
        ENDIF
        oEntry := oEntry:next
    ENDDO

    lExitGuiPopup := ! M_ParmExists( "-nogui" )
    IF lExitGuiPopup .AND. ! I_ConsoleStdout()
        ISysErrorBox( Left( cMsg, 511 ) )
    ENDIF

    ErrorLevel( -1 )
    QUIT
RETURN NIL

STATIC PROCEDURE LoadMemDump()
    LOCAL p
    LOCAL i
    LOCAL nVal
    LOCAL cArg
    MEMVAR myargc
    MEMVAR myargv

    firsttime_mem := .F.
    i := 0
    p := M_CheckParmWithArgs( "-setmem", 1 )
    IF p > 0
        cArg := myargv[ p + 1 + 1 ]
        IF hb_stricmp( cArg, "dos622" ) == 0
            dos_mem_dump := mem_dump_dos622
        ENDIF
        IF hb_stricmp( cArg, "dos71" ) == 0
            dos_mem_dump := mem_dump_win98
        ELSEIF hb_stricmp( cArg, "dosbox" ) == 0
            dos_mem_dump := mem_dump_dosbox
        ELSE
            i := 0
            DO WHILE i < DOS_MEM_DUMP_SIZE
                p := p + 1
                IF p >= myargc .OR. Left( myargv[ p + 1 ], 1 ) == "-"
                    EXIT
                ENDIF
                nVal := 0
                M_StrToInt( myargv[ p + 1 ], @nVal )
                mem_dump_custom[ i + 1 ] := ( nVal & 0xFF )
                i := i + 1
                i := i + 1
            ENDDO
            dos_mem_dump := mem_dump_custom
        ENDIF
    ENDIF
RETURN

FUNCTION I_GetMemoryValue( nOffset, nValue, nSize )
    LOCAL nOff

    IF firsttime_mem
        LoadMemDump()
    ENDIF

    nOff := nOffset + 1
    SWITCH nSize
    CASE 1
        nValue := ( dos_mem_dump[ nOff ] & 0xFF )
        RETURN .T.
    CASE 2
        nValue := ( dos_mem_dump[ nOff ] & 0xFF ) ;
            + ( dos_mem_dump[ nOff + 1 ] & 0xFF ) * 256
        nValue := ( nValue & 0xFFFF )
        RETURN .T.
    CASE 4
        nValue := ( dos_mem_dump[ nOff ] & 0xFF ) ;
            + ( dos_mem_dump[ nOff + 1 ] & 0xFF ) * 256 ;
            + ( dos_mem_dump[ nOff + 2 ] & 0xFF ) * 65536 ;
            + ( dos_mem_dump[ nOff + 3 ] & 0xFF ) * 16777216
        nValue := ( nValue & 0xFFFFFFFF )
        RETURN .T.
    ENDSWITCH
RETURN .F.

#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapiitm.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

HB_FUNC( ISYSMALLOC )
{
   int size = hb_parni( 1 );
   void *p;

   if( size <= 0 )
   {
      hb_ret();
      return;
   }
   p = malloc( ( size_t ) size );
   hb_retptr( p );
}

HB_FUNC( ISYSPTRHEX )
{
   char buf[ 32 ];

   sprintf( buf, "%p", hb_parptr( 1 ) );
   hb_retc( buf );
}

HB_FUNC( ISYSERRORBOX )
{
#ifdef _WIN32
   wchar_t wmsgbuf[ 512 ];
   const char *msg = hb_parc( 1 );

   if( msg == NULL )
      msg = "";
   MultiByteToWideChar( CP_ACP, 0, msg, -1, wmsgbuf, 512 );
   MessageBoxW( NULL, wmsgbuf, L"Error",
                MB_OK | MB_ICONERROR | MB_TOPMOST | MB_SETFOREGROUND );
#endif
}

#pragma ENDDUMP
