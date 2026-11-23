/*
DOOM generic portado para Harbour. Janela, teclado, relogio e som usam SDL3.

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

STATIC s_fullscreen := 0
STATIC s_scale := 2

#include "doomgeneric.ch"

STATIC PROCEDURE ParseVideoArgs()
    LOCAL p
    MEMVAR myargv

    s_fullscreen := iif( M_ParmExists( "-fullscreen" ), 1, 0 )

    p := M_CheckParmWithArgs( "-scaling", 1 )
    IF p > 0
        s_scale := Val( myargv[ p + 1 + 1 ] )
        IF s_scale < 1
            s_scale := 1
        ENDIF
        IF s_scale > 8
            s_scale := 8
        ENDIF
    ENDIF
RETURN

FUNCTION DG_Init()
    LOCAL nW
    LOCAL nH

    OutStd( "Initializing SDL3" + hb_eol() )

    I_AtExit( {|| I_ShutdownSound(), SdlShutdown() }, .T. )

    ParseVideoArgs()
    nW := 320 * s_scale
    nH := 200 * s_scale
    IF SdlOpen( nW, nH, s_fullscreen != 0 ) == 0
        I_Error( "SDL3: " + SdlLastError() + hb_eol() )
    ENDIF
    OutStd( "SDL3 video: " + hb_ntos( nW ) + "x" + hb_ntos( nH ) + ;
        iif( s_fullscreen != 0, " fullscreen", " windowed" ) + hb_eol() )
RETURN NIL

FUNCTION DG_Fullscreen()
RETURN SdlIsFullscreen()

FUNCTION DG_DrawFrame()
    MEMVAR I_VideoBuffer

    IF ValType( I_VideoBuffer ) == "C"
        SdlPresent( I_VideoBuffer )
    ENDIF
    IF SdlQuitRequested()
        I_Quit()
    ENDIF
RETURN NIL

FUNCTION DG_SleepMs( ms )
    SdlDelay( ms )
RETURN NIL

FUNCTION DG_GetTicksMs()
RETURN SdlTicks()

FUNCTION DG_GetKey( pressed, doomKey )
    LOCAL aKey

    aKey := SdlKeyPop()
    IF aKey == NIL
        IF SdlQuitRequested()
            I_Quit()
        ENDIF
        RETURN 0
    ENDIF
    pressed := aKey[ 1 ]
    doomKey := aKey[ 2 ]
RETURN 1

FUNCTION DG_SetWindowTitle( title )
    SdlSetTitle( title )
RETURN NIL

#pragma BEGINDUMP
#include "hbapi.h"
#include <stdlib.h>
#include <string.h>

/* Buffer legado do blit escalado. A imagem na tela sai pela SDL3. */
unsigned char *DG_ScreenBuffer = NULL;

HB_FUNC( DG_ALLOCSCREEN )
{
   if( DG_ScreenBuffer == NULL )
      DG_ScreenBuffer = ( unsigned char * ) calloc( ( size_t ) 320 * 8 * 200 * 8 * 4, 1 );
   hb_retptr( DG_ScreenBuffer );
}
#pragma ENDDUMP
