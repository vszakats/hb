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

#include "p_doors.ch"

CLASS vldoor_t
    DATA thinker
    DATA thinkfn
    DATA type
    DATA sector
    DATA topheight
    DATA speed
    DATA direction
    DATA topwait
    DATA topcountdown
    DATA wait
    METHOD New()
ENDCLASS

METHOD New() CLASS vldoor_t
    ::thinker      := thinker_t():New()
    ::thinkfn      := ""
    ::type         := 0
    ::sector       := NIL
    ::topheight    := 0
    ::speed        := 0
    ::direction    := 0
    ::topwait      := 0
    ::topcountdown := 0
    ::wait         := 0
RETURN Self

STATIC PROCEDURE BindVerticalDoor( door )
    LOCAL oDoor := door

    door:thinkfn := "T_VerticalDoor"
    door:thinker:thinkfn := "T_VerticalDoor"
    door:thinker:owner := door
    door:thinker:function:acp1 := {|| T_VerticalDoor( oDoor ) }
RETURN

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

FUNCTION T_VerticalDoor( door )
    LOCAL res

    SWITCH door:direction
    CASE 0
        door:topcountdown := door:topcountdown - 1
        IF door:topcountdown == 0
            SWITCH door:type
            CASE vld_blazeRaise
                door:direction := -1
                S_StartSound( door:sector:soundorg, sfx_bdcls )
                EXIT

            CASE vld_normal
                door:direction := -1
                S_StartSound( door:sector:soundorg, sfx_dorcls )
                EXIT

            CASE vld_close30ThenOpen
                door:direction := 1
                S_StartSound( door:sector:soundorg, sfx_doropn )
                EXIT

            OTHERWISE
                EXIT
            ENDSWITCH
        ENDIF
        EXIT

    CASE 2
        door:topcountdown := door:topcountdown - 1
        IF door:topcountdown == 0
            SWITCH door:type
            CASE vld_raiseIn5Mins
                door:direction := 1
                door:type := vld_normal
                S_StartSound( door:sector:soundorg, sfx_doropn )
                EXIT

            OTHERWISE
                EXIT
            ENDSWITCH
        ENDIF
        EXIT

    CASE -1
        res := T_MovePlane( door:sector, ;
                            door:speed, ;
                            door:sector:floorheight, ;
                            .F., 1, door:direction )
        IF res == result_pastdest
            SWITCH door:type
            CASE vld_blazeRaise
                door:sector:specialdata := NIL
                P_RemoveThinker( door:thinker )
                S_StartSound( door:sector:soundorg, sfx_bdcls )
                EXIT
            CASE vld_blazeClose
                door:sector:specialdata := NIL
                P_RemoveThinker( door:thinker )
                S_StartSound( door:sector:soundorg, sfx_bdcls )
                EXIT

            CASE vld_normal
                door:sector:specialdata := NIL
                P_RemoveThinker( door:thinker )
                EXIT
            CASE vld_close
                door:sector:specialdata := NIL
                P_RemoveThinker( door:thinker )
                EXIT

            CASE vld_close30ThenOpen
                door:direction := 0
                door:topcountdown := TICRATE * 30
                EXIT

            OTHERWISE
                EXIT
            ENDSWITCH
        ELSEIF res == result_crushed
            SWITCH door:type
            CASE vld_blazeClose
                EXIT
            CASE vld_close
                EXIT
            OTHERWISE
                door:direction := 1
                S_StartSound( door:sector:soundorg, sfx_doropn )
                EXIT
            ENDSWITCH
        ENDIF
        EXIT

    CASE 1
        res := T_MovePlane( door:sector, ;
                            door:speed, ;
                            door:topheight, ;
                            .F., 1, door:direction )

        IF res == result_pastdest
            SWITCH door:type
            CASE vld_blazeRaise
                door:direction := 0
                door:topcountdown := door:topwait
                EXIT
            CASE vld_normal
                door:direction := 0
                door:topcountdown := door:topwait
                EXIT

            CASE vld_close30ThenOpen
                door:sector:specialdata := NIL
                P_RemoveThinker( door:thinker )
                EXIT
            CASE vld_blazeOpen
                door:sector:specialdata := NIL
                P_RemoveThinker( door:thinker )
                EXIT
            CASE vld_open
                door:sector:specialdata := NIL
                P_RemoveThinker( door:thinker )
                EXIT

            OTHERWISE
                EXIT
            ENDSWITCH
        ENDIF
        EXIT
    ENDSWITCH
RETURN NIL

FUNCTION EV_DoLockedDoor( line, type, thing )
    LOCAL p

    p := thing:player

    IF p == NIL
        RETURN 0
    ENDIF

    SWITCH line:special
    CASE 99
    CASE 133
        IF p == NIL
            RETURN 0
        ENDIF
        IF ! p:cards[ it_bluecard + 1 ] .AND. ! p:cards[ it_blueskull + 1 ]
            p:message := DEH_String( PD_BLUEO )
            S_StartSound( NIL, sfx_oof )
            RETURN 0
        ENDIF
        EXIT

    CASE 134
    CASE 135
        IF p == NIL
            RETURN 0
        ENDIF
        IF ! p:cards[ it_redcard + 1 ] .AND. ! p:cards[ it_redskull + 1 ]
            p:message := DEH_String( PD_REDO )
            S_StartSound( NIL, sfx_oof )
            RETURN 0
        ENDIF
        EXIT

    CASE 136
    CASE 137
        IF p == NIL
            RETURN 0
        ENDIF
        IF ! p:cards[ it_yellowcard + 1 ] .AND. ! p:cards[ it_yellowskull + 1 ]
            p:message := DEH_String( PD_YELLOWO )
            S_StartSound( NIL, sfx_oof )
            RETURN 0
        ENDIF
        EXIT
    ENDSWITCH

RETURN EV_DoDoor( line, type )

FUNCTION EV_DoDoor( line, type )
    LOCAL secnum
    LOCAL rtn
    LOCAL sec
    LOCAL door
    MEMVAR sectors

    secnum := -1
    rtn := 0

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
        door := vldoor_t():New()
        BindVerticalDoor( door )
        P_AddThinker( door:thinker )
        sec:specialdata := door

        door:sector  := sec
        door:type    := type
        door:topwait := VDOORWAIT
        door:speed   := VDOORSPEED

        SWITCH type
        CASE vld_blazeClose
            door:topheight := P_FindLowestCeilingSurrounding( sec )
            door:topheight -= 4 * FRACUNIT
            door:direction := -1
            door:speed := VDOORSPEED * 4
            S_StartSound( door:sector:soundorg, sfx_bdcls )
            EXIT

        CASE vld_close
            door:topheight := P_FindLowestCeilingSurrounding( sec )
            door:topheight -= 4 * FRACUNIT
            door:direction := -1
            S_StartSound( door:sector:soundorg, sfx_dorcls )
            EXIT

        CASE vld_close30ThenOpen
            door:topheight := sec:ceilingheight
            door:direction := -1
            S_StartSound( door:sector:soundorg, sfx_dorcls )
            EXIT

        CASE vld_blazeRaise
            door:direction := 1
            door:topheight := P_FindLowestCeilingSurrounding( sec )
            door:topheight -= 4 * FRACUNIT
            door:speed := VDOORSPEED * 4
            IF door:topheight != sec:ceilingheight
                S_StartSound( door:sector:soundorg, sfx_bdopn )
            ENDIF
            EXIT

        CASE vld_blazeOpen
            door:direction := 1
            door:topheight := P_FindLowestCeilingSurrounding( sec )
            door:topheight -= 4 * FRACUNIT
            door:speed := VDOORSPEED * 4
            IF door:topheight != sec:ceilingheight
                S_StartSound( door:sector:soundorg, sfx_bdopn )
            ENDIF
            EXIT

        CASE vld_normal
            door:direction := 1
            door:topheight := P_FindLowestCeilingSurrounding( sec )
            door:topheight -= 4 * FRACUNIT
            IF door:topheight != sec:ceilingheight
                S_StartSound( door:sector:soundorg, sfx_doropn )
            ENDIF
            EXIT

        CASE vld_open
            door:direction := 1
            door:topheight := P_FindLowestCeilingSurrounding( sec )
            door:topheight -= 4 * FRACUNIT
            IF door:topheight != sec:ceilingheight
                S_StartSound( door:sector:soundorg, sfx_doropn )
            ENDIF
            EXIT

        OTHERWISE
            EXIT
        ENDSWITCH
    ENDDO
RETURN rtn

FUNCTION EV_VerticalDoor( line, thing )
    LOCAL player
    LOCAL sec
    LOCAL door
    LOCAL side
    LOCAL cName
    MEMVAR sides

    side := 0

    player := thing:player

    SWITCH line:special
    CASE 26
    CASE 32
        IF player == NIL
            RETURN NIL
        ENDIF
        IF ! player:cards[ it_bluecard + 1 ] .AND. ! player:cards[ it_blueskull + 1 ]
            player:message := DEH_String( PD_BLUEK )
            S_StartSound( NIL, sfx_oof )
            RETURN NIL
        ENDIF
        EXIT

    CASE 27
    CASE 34
        IF player == NIL
            RETURN NIL
        ENDIF
        IF ! player:cards[ it_yellowcard + 1 ] .AND. ! player:cards[ it_yellowskull + 1 ]
            player:message := DEH_String( PD_YELLOWK )
            S_StartSound( NIL, sfx_oof )
            RETURN NIL
        ENDIF
        EXIT

    CASE 28
    CASE 33
        IF player == NIL
            RETURN NIL
        ENDIF
        IF ! player:cards[ it_redcard + 1 ] .AND. ! player:cards[ it_redskull + 1 ]
            player:message := DEH_String( PD_REDK )
            S_StartSound( NIL, sfx_oof )
            RETURN NIL
        ENDIF
        EXIT
    ENDSWITCH

    sec := sides[ line:sidenum[ ( side ^^ 1 ) + 1 ] + 1 ]:sector

    IF sec:specialdata != NIL
        door := sec:specialdata
        SWITCH line:special
        CASE 1
        CASE 26
        CASE 27
        CASE 28
        CASE 117
            IF door:direction == -1
                door:direction := 1
            ELSE
                IF thing:player == NIL
                    RETURN NIL
                ENDIF

                cName := ThinkName( door )
                IF cName == "T_VerticalDoor"
                    door:direction := -1
                ELSEIF cName == "T_PlatRaise"
                    door:wait := -1
                ELSE
                    OutErr( "EV_VerticalDoor: Tried to close something that wasn't a door." + hb_eol() )
                    door:direction := -1
                ENDIF
            ENDIF
            RETURN NIL
        ENDSWITCH
    ENDIF

    SWITCH line:special
    CASE 117
    CASE 118
        S_StartSound( sec:soundorg, sfx_bdopn )
        EXIT

    CASE 1
    CASE 31
        S_StartSound( sec:soundorg, sfx_doropn )
        EXIT

    OTHERWISE
        S_StartSound( sec:soundorg, sfx_doropn )
        EXIT
    ENDSWITCH

    door := vldoor_t():New()
    BindVerticalDoor( door )
    P_AddThinker( door:thinker )
    sec:specialdata := door
    door:sector    := sec
    door:direction := 1
    door:speed     := VDOORSPEED
    door:topwait   := VDOORWAIT

    SWITCH line:special
    CASE 1
    CASE 26
    CASE 27
    CASE 28
        door:type := vld_normal
        EXIT

    CASE 31
    CASE 32
    CASE 33
    CASE 34
        door:type := vld_open
        line:special := 0
        EXIT

    CASE 117
        door:type := vld_blazeRaise
        door:speed := VDOORSPEED * 4
        EXIT

    CASE 118
        door:type := vld_blazeOpen
        line:special := 0
        door:speed := VDOORSPEED * 4
        EXIT
    ENDSWITCH

    door:topheight := P_FindLowestCeilingSurrounding( sec )
    door:topheight -= 4 * FRACUNIT
RETURN NIL

FUNCTION P_SpawnDoorCloseIn30( sec )
    LOCAL door

    door := vldoor_t():New()
    BindVerticalDoor( door )
    P_AddThinker( door:thinker )

    sec:specialdata := door
    sec:special := 0

    door:sector       := sec
    door:direction    := 0
    door:type         := vld_normal
    door:speed        := VDOORSPEED
    door:topcountdown := 30 * TICRATE
RETURN NIL

FUNCTION P_SpawnDoorRaiseIn5Mins( sec, secnum )
    LOCAL door

    HB_SYMBOL_UNUSED( secnum )

    door := vldoor_t():New()
    BindVerticalDoor( door )
    P_AddThinker( door:thinker )

    sec:specialdata := door
    sec:special := 0

    door:sector       := sec
    door:direction    := 2
    door:type         := vld_raiseIn5Mins
    door:speed        := VDOORSPEED
    door:topheight    := P_FindLowestCeilingSurrounding( sec )
    door:topheight    -= 4 * FRACUNIT
    door:topwait      := VDOORWAIT
    door:topcountdown := 5 * 60 * TICRATE
RETURN NIL
