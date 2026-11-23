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

#include "p_telept.ch"
#include "p_local.ch"
#include "p_mobj.ch"
#include "p_tick.ch"
#include "p_pspr.ch"
#include "doomstat.ch"
#include "doomdef.ch"
#include "info.ch"
#include "d_mode.ch"


STATIC FUNCTION ThinkName( o )
    LOCAL x

    IF o == NIL
        RETURN ""
    ENDIF
    IF __objHasMsg( o, "THINKFN" ) .AND. ValType( o:thinkfn ) == "C" .AND. ! Empty( o:thinkfn )
        RETURN o:thinkfn
    ENDIF
    IF __objHasMsg( o, "THINKER" ) .AND. o:thinker != NIL
        IF __objHasMsg( o:thinker, "THINKFN" ) .AND. ValType( o:thinker:thinkfn ) == "C" .AND. ! Empty( o:thinker:thinkfn )
            RETURN o:thinker:thinkfn
        ENDIF
        IF o:thinker:function != NIL
            x := o:thinker:function:acp1
            IF ValType( x ) == "C"
                RETURN x
            ENDIF
        ENDIF
    ENDIF
    IF __objHasMsg( o, "FUNCTION" ) .AND. o:function != NIL .AND. ValType( o:function ) == "O"
        x := o:function:acp1
        IF ValType( x ) == "C"
            RETURN x
        ENDIF
    ENDIF
RETURN ""

STATIC FUNCTION ThinkerToMobj( th )
    IF th == NIL
        RETURN NIL
    ENDIF
    IF __objHasMsg( th, "OWNER" ) .AND. th:owner != NIL
        RETURN th:owner
    ENDIF
    IF __objHasMsg( th, "TYPE" )
        RETURN th
    ENDIF
RETURN NIL

FUNCTION EV_Teleport( line, side, thing )
    LOCAL i
    LOCAL tag
    LOCAL m
    LOCAL fog
    LOCAL an
    LOCAL thinker
    LOCAL sector
    LOCAL oldx
    LOCAL oldy
    LOCAL oldz
    MEMVAR finecosine
    MEMVAR finesine
    MEMVAR gameversion
    MEMVAR numsectors
    MEMVAR sectors
    MEMVAR thinkercap

    IF ( thing:flags & MF_MISSILE ) != 0
        RETURN 0
    ENDIF

    IF side == 1
        RETURN 0
    ENDIF

    tag := line:tag
    FOR i := 0 TO numsectors - 1
        IF sectors[ i + 1 ]:tag == tag
            thinker := thinkercap:next
            DO WHILE thinker != NIL .AND. !( thinker == thinkercap )
                IF ThinkName( thinker ) != "P_MobjThinker"
                    thinker := thinker:next
                    LOOP
                ENDIF

                m := ThinkerToMobj( thinker )
                IF m == NIL
                    thinker := thinker:next
                    LOOP
                ENDIF

                IF m:type != MT_TELEPORTMAN
                    thinker := thinker:next
                    LOOP
                ENDIF

                sector := m:subsector:sector
                IF sector:iSector != i
                    thinker := thinker:next
                    LOOP
                ENDIF

                oldx := thing:x
                oldy := thing:y
                oldz := thing:z

                IF ! P_TeleportMove( thing, m:x, m:y )
                    RETURN 0
                ENDIF

                IF gameversion != exe_final
                    thing:z := thing:floorz
                ENDIF

                IF thing:player != NIL
                    thing:player:viewz := thing:z + thing:player:viewheight
                ENDIF

                fog := P_SpawnMobj( oldx, oldy, oldz, MT_TFOG )
                S_StartSound( fog, sfx_telept )
                an := UShr( m:angle, ANGLETOFINESHIFT )
                fog := P_SpawnMobj( m:x + 20 * finecosine[ an + 1 ], ;
                                    m:y + 20 * finesine[ an + 1 ], ;
                                    thing:z, MT_TFOG )

                S_StartSound( fog, sfx_telept )

                IF thing:player != NIL
                    thing:reactiontime := 18
                ENDIF

                thing:angle := m:angle
                thing:momx := 0
                thing:momy := 0
                thing:momz := 0
                RETURN 1
            ENDDO
        ENDIF
    NEXT
RETURN 0
