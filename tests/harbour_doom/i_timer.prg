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

STATIC basetime := 0

#include "i_timer.ch"


INIT PROCEDURE init_i_timer
    basetime := 0
RETURN

FUNCTION I_GetTicks()
RETURN DG_GetTicksMs()

FUNCTION I_GetTime()
    LOCAL nTicks

    nTicks := ( I_GetTicks() & 0xFFFFFFFF )
    IF basetime == 0
        basetime := nTicks
    ENDIF
    nTicks := ( ( nTicks - basetime ) & 0xFFFFFFFF )
RETURN Int( ( ( nTicks * TICRATE ) & 0xFFFFFFFF ) / 1000 )

FUNCTION I_GetTimeMS()
    LOCAL nTicks

    nTicks := ( I_GetTicks() & 0xFFFFFFFF )
    IF basetime == 0
        basetime := nTicks
    ENDIF
RETURN ( ( nTicks - basetime ) & 0xFFFFFFFF )

FUNCTION I_Sleep( ms )
    DG_SleepMs( ms )
RETURN NIL

FUNCTION I_WaitVBL( count )
RETURN NIL

FUNCTION I_InitTimer()
RETURN NIL
