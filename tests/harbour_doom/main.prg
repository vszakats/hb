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

#include "doomgeneric.ch"

PROCEDURE Main()
    LOCAL nMaxFps
    LOCAL nFrameMs
    LOCAL nNext
    LOCAL nNow
    LOCAL nWait
    LOCAL i
    MEMVAR myargv

    doom_hb_Create()

    nMaxFps := 0
    nFrameMs := 0
    nNext := 0
    i := M_CheckParmWithArgs( "-maxfps", 1 )
    IF i > 0
        nMaxFps := Int( Val( myargv[ i + 1 + 1 ] ) )
        IF nMaxFps < 1
            nMaxFps := 0
        ENDIF
    ENDIF
    IF nMaxFps > 0 .AND. ! M_ParmExists( "-timedemo" )
        OutStd( "max fps: " + hb_ntos( nMaxFps ) + hb_eol() )
        nFrameMs := Int( 1000 / nMaxFps )
        IF nFrameMs < 1
            nFrameMs := 1
        ENDIF
        nNext := I_GetTimeMS()
    ELSE
        nMaxFps := 0
    ENDIF

    DO WHILE .T.
        doomgeneric_Tick()
        IF nMaxFps > 0
            nNext += nFrameMs
            nNow := I_GetTimeMS()
            nWait := nNext - nNow
            IF nWait > 1
                I_Sleep( nWait )
            ELSEIF nWait < -nFrameMs
                nNext := I_GetTimeMS()
            ENDIF
        ENDIF
    ENDDO

RETURN
