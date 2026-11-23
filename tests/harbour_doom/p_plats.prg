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

#include "p_plats.ch"

CLASS plat_t
    DATA thinker
    DATA thinkfn
    DATA sector
    DATA speed
    DATA low
    DATA high
    DATA wait
    DATA count
    DATA status
    DATA oldstatus
    DATA crush
    DATA tag
    DATA type
    METHOD New()
ENDCLASS

METHOD New() CLASS plat_t
    ::thinker   := thinker_t():New()
    ::thinkfn   := ""
    ::sector    := NIL
    ::speed     := 0
    ::low       := 0
    ::high      := 0
    ::wait      := 0
    ::count     := 0
    ::status    := 0
    ::oldstatus := 0
    ::crush     := .F.
    ::tag       := 0
    ::type      := 0
RETURN Self

INIT PROCEDURE init_p_plats
    LOCAL i
    PUBLIC activeplats
    activeplats := {}
    FOR i := 1 TO MAXPLATS
        AAdd( activeplats, NIL )
    NEXT
RETURN

STATIC PROCEDURE BindPlatRaise( plat )
    LOCAL oPlat := plat
    plat:thinkfn := "T_PlatRaise"
    plat:thinker:thinkfn := "T_PlatRaise"
    plat:thinker:owner := plat
    plat:thinker:function:acp1 := {|| T_PlatRaise( oPlat ) }
RETURN

FUNCTION T_PlatRaise( plat )
    LOCAL res
    MEMVAR leveltime
    SWITCH plat:status
    CASE plat_up
        res := T_MovePlane( plat:sector, plat:speed, plat:high, plat:crush, 0, 1 )
        IF plat:type == raiseAndChange .OR. plat:type == raiseToNearestAndChange
            IF ( leveltime & 7 ) == 0
                S_StartSound( plat:sector:soundorg, sfx_stnmov )
            ENDIF
        ENDIF
        IF res == result_crushed .AND. ! plat:crush
            plat:count := plat:wait
            plat:status := plat_down
            S_StartSound( plat:sector:soundorg, sfx_pstart )
        ELSE
            IF res == result_pastdest
                plat:count := plat:wait
                plat:status := plat_waiting
                S_StartSound( plat:sector:soundorg, sfx_pstop )
                SWITCH plat:type
                CASE blazeDWUS
                    P_RemoveActivePlat( plat )
                    EXIT
                CASE downWaitUpStay
                    P_RemoveActivePlat( plat )
                    EXIT
                CASE raiseAndChange
                    P_RemoveActivePlat( plat )
                    EXIT
                CASE raiseToNearestAndChange
                    P_RemoveActivePlat( plat )
                    EXIT
                OTHERWISE
                    EXIT
                ENDSWITCH
            ENDIF
        ENDIF
        EXIT
    CASE plat_down
        res := T_MovePlane( plat:sector, plat:speed, plat:low, .F., 0, -1 )
        IF res == result_pastdest
            plat:count := plat:wait
            plat:status := plat_waiting
            S_StartSound( plat:sector:soundorg, sfx_pstop )
        ENDIF
        EXIT
    CASE plat_waiting
        plat:count := plat:count - 1
        IF plat:count == 0
            IF plat:sector:floorheight == plat:low
                plat:status := plat_up
            ELSE
                plat:status := plat_down
            ENDIF
            S_StartSound( plat:sector:soundorg, sfx_pstart )
        ENDIF
        EXIT
    CASE plat_in_stasis
        EXIT
    ENDSWITCH
RETURN NIL

FUNCTION EV_DoPlat( line, type, amount )
    LOCAL plat
    LOCAL secnum
    LOCAL rtn
    LOCAL sec
    MEMVAR sectors
    MEMVAR sides
    secnum := -1
    rtn := 0
    SWITCH type
    CASE perpetualRaise
        P_ActivateInStasis( line:tag )
        EXIT
    OTHERWISE
        EXIT
    ENDSWITCH
    DO WHILE .T.
        secnum := P_FindSectorFromLineTag( line, secnum )
        IF secnum < 0
            EXIT
        ENDIF
        sec := sectors[ secnum + 1 ]
        IF sec:specialdata != NIL
            LOOP
        ENDIF
        rtn := 1
        plat := plat_t():New()
        P_AddThinker( plat:thinker )
        plat:type := type
        plat:sector := sec
        plat:sector:specialdata := plat
        BindPlatRaise( plat )
        plat:crush := .F.
        plat:tag := line:tag
        SWITCH type
        CASE raiseToNearestAndChange
            plat:speed := Int( PLATSPEED / 2 )
            sec:floorpic := sides[ line:sidenum[ 1 ] + 1 ]:sector:floorpic
            plat:high := P_FindNextHighestFloor( sec, sec:floorheight )
            plat:wait := 0
            plat:status := plat_up
            sec:special := 0
            S_StartSound( sec:soundorg, sfx_stnmov )
            EXIT
        CASE raiseAndChange
            plat:speed := Int( PLATSPEED / 2 )
            sec:floorpic := sides[ line:sidenum[ 1 ] + 1 ]:sector:floorpic
            plat:high := sec:floorheight + amount * FRACUNIT
            plat:wait := 0
            plat:status := plat_up
            S_StartSound( sec:soundorg, sfx_stnmov )
            EXIT
        CASE downWaitUpStay
            plat:speed := PLATSPEED * 4
            plat:low := P_FindLowestFloorSurrounding( sec )
            IF plat:low > sec:floorheight
                plat:low := sec:floorheight
            ENDIF
            plat:high := sec:floorheight
            plat:wait := TICRATE * PLATWAIT
            plat:status := plat_down
            S_StartSound( sec:soundorg, sfx_pstart )
            EXIT
        CASE blazeDWUS
            plat:speed := PLATSPEED * 8
            plat:low := P_FindLowestFloorSurrounding( sec )
            IF plat:low > sec:floorheight
                plat:low := sec:floorheight
            ENDIF
            plat:high := sec:floorheight
            plat:wait := TICRATE * PLATWAIT
            plat:status := plat_down
            S_StartSound( sec:soundorg, sfx_pstart )
            EXIT
        CASE perpetualRaise
            plat:speed := PLATSPEED
            plat:low := P_FindLowestFloorSurrounding( sec )
            IF plat:low > sec:floorheight
                plat:low := sec:floorheight
            ENDIF
            plat:high := P_FindHighestFloorSurrounding( sec )
            IF plat:high < sec:floorheight
                plat:high := sec:floorheight
            ENDIF
            plat:wait := TICRATE * PLATWAIT
            plat:status := ( P_Random() & 1 )
            S_StartSound( sec:soundorg, sfx_pstart )
            EXIT
        ENDSWITCH
        P_AddActivePlat( plat )
    ENDDO
RETURN rtn

FUNCTION P_ActivateInStasis( tag )
    LOCAL i
    MEMVAR activeplats
    FOR i := 0 TO MAXPLATS - 1
        IF activeplats[ i + 1 ] != NIL ;
           .AND. activeplats[ i + 1 ]:tag == tag ;
           .AND. activeplats[ i + 1 ]:status == plat_in_stasis
            activeplats[ i + 1 ]:status := activeplats[ i + 1 ]:oldstatus
            BindPlatRaise( activeplats[ i + 1 ] )
        ENDIF
    NEXT
RETURN NIL

FUNCTION EV_StopPlat( line )
    LOCAL j
    MEMVAR activeplats
    FOR j := 0 TO MAXPLATS - 1
        IF activeplats[ j + 1 ] != NIL ;
           .AND. activeplats[ j + 1 ]:status != plat_in_stasis ;
           .AND. activeplats[ j + 1 ]:tag == line:tag
            activeplats[ j + 1 ]:oldstatus := activeplats[ j + 1 ]:status
            activeplats[ j + 1 ]:status := plat_in_stasis
            activeplats[ j + 1 ]:thinker:function:acv := NIL
            activeplats[ j + 1 ]:thinkfn := ""
            activeplats[ j + 1 ]:thinker:thinkfn := ""
        ENDIF
    NEXT
RETURN NIL

FUNCTION P_AddActivePlat( plat )
    LOCAL i
    MEMVAR activeplats
    FOR i := 0 TO MAXPLATS - 1
        IF activeplats[ i + 1 ] == NIL
            activeplats[ i + 1 ] := plat
            RETURN NIL
        ENDIF
    NEXT
    I_Error( "P_AddActivePlat: no more plats!" )
RETURN NIL

FUNCTION P_RemoveActivePlat( plat )
    LOCAL i
    MEMVAR activeplats
    FOR i := 0 TO MAXPLATS - 1
        IF plat == activeplats[ i + 1 ]
            activeplats[ i + 1 ]:sector:specialdata := NIL
            P_RemoveThinker( activeplats[ i + 1 ]:thinker )
            activeplats[ i + 1 ] := NIL
            RETURN NIL
        ENDIF
    NEXT
    I_Error( "P_RemoveActivePlat: can't find plat!" )
RETURN NIL
