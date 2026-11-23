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

#include "p_ceilng.ch"

CLASS ceiling_t
    DATA thinker
    DATA thinkfn
    DATA type
    DATA sector
    DATA bottomheight
    DATA topheight
    DATA speed
    DATA crush
    DATA direction
    DATA tag
    DATA olddirection
    METHOD New()
ENDCLASS

METHOD New() CLASS ceiling_t
    ::thinker      := thinker_t():New()
    ::thinkfn      := ""
    ::type         := 0
    ::sector       := NIL
    ::bottomheight := 0
    ::topheight    := 0
    ::speed        := 0
    ::crush        := .F.
    ::direction    := 0
    ::tag          := 0
    ::olddirection := 0
RETURN Self

INIT PROCEDURE init_p_ceilng
    LOCAL i

    PUBLIC activeceilings

    activeceilings := {}
    FOR i := 1 TO MAXCEILINGS
        AAdd( activeceilings, NIL )
    NEXT
RETURN

STATIC PROCEDURE BindMoveCeiling( ceiling )
    LOCAL oCeil := ceiling

    ceiling:thinkfn := "T_MoveCeiling"
    ceiling:thinker:thinkfn := "T_MoveCeiling"
    ceiling:thinker:owner := ceiling
    ceiling:thinker:function:acp1 := {|| T_MoveCeiling( oCeil ) }
RETURN

FUNCTION T_MoveCeiling( ceiling )
    LOCAL res
    MEMVAR leveltime

    SWITCH ceiling:direction
    CASE 0
        EXIT

    CASE 1
        res := T_MovePlane( ceiling:sector, ;
                            ceiling:speed, ;
                            ceiling:topheight, ;
                            .F., 1, ceiling:direction )

        IF ( leveltime & 7 ) == 0
            SWITCH ceiling:type
            CASE silentCrushAndRaise
                EXIT
            OTHERWISE
                S_StartSound( ceiling:sector:soundorg, sfx_stnmov )
                EXIT
            ENDSWITCH
        ENDIF

        IF res == result_pastdest
            SWITCH ceiling:type
            CASE raiseToHighest
                P_RemoveActiveCeiling( ceiling )
                EXIT

            CASE silentCrushAndRaise
                S_StartSound( ceiling:sector:soundorg, sfx_pstop )
                ceiling:direction := -1
                EXIT

            CASE fastCrushAndRaise
                ceiling:direction := -1
                EXIT

            CASE crushAndRaise
                ceiling:direction := -1
                EXIT

            OTHERWISE
                EXIT
            ENDSWITCH
        ENDIF
        EXIT

    CASE -1
        res := T_MovePlane( ceiling:sector, ;
                            ceiling:speed, ;
                            ceiling:bottomheight, ;
                            ceiling:crush, 1, ceiling:direction )

        IF ( leveltime & 7 ) == 0
            SWITCH ceiling:type
            CASE silentCrushAndRaise
                EXIT
            OTHERWISE
                S_StartSound( ceiling:sector:soundorg, sfx_stnmov )
                EXIT
            ENDSWITCH
        ENDIF

        IF res == result_pastdest
            SWITCH ceiling:type
            CASE silentCrushAndRaise
                S_StartSound( ceiling:sector:soundorg, sfx_pstop )
                ceiling:speed := CEILSPEED
                ceiling:direction := 1
                EXIT

            CASE crushAndRaise
                ceiling:speed := CEILSPEED
                ceiling:direction := 1
                EXIT

            CASE fastCrushAndRaise
                ceiling:direction := 1
                EXIT

            CASE lowerAndCrush
                P_RemoveActiveCeiling( ceiling )
                EXIT

            CASE lowerToFloor
                P_RemoveActiveCeiling( ceiling )
                EXIT

            OTHERWISE
                EXIT
            ENDSWITCH
        ELSE
            IF res == result_crushed
                SWITCH ceiling:type
                CASE silentCrushAndRaise
                    ceiling:speed := Int( CEILSPEED / 8 )
                    EXIT
                CASE crushAndRaise
                    ceiling:speed := Int( CEILSPEED / 8 )
                    EXIT
                CASE lowerAndCrush
                    ceiling:speed := Int( CEILSPEED / 8 )
                    EXIT
                OTHERWISE
                    EXIT
                ENDSWITCH
            ENDIF
        ENDIF
        EXIT
    ENDSWITCH
RETURN NIL

FUNCTION EV_DoCeiling( line, type )
    LOCAL secnum
    LOCAL rtn
    LOCAL sec
    LOCAL ceiling
    MEMVAR sectors

    secnum := -1
    rtn := 0

    SWITCH type
    CASE fastCrushAndRaise
        P_ActivateInStasisCeiling( line )
        EXIT
    CASE silentCrushAndRaise
        P_ActivateInStasisCeiling( line )
        EXIT
    CASE crushAndRaise
        P_ActivateInStasisCeiling( line )
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
        ceiling := ceiling_t():New()
        BindMoveCeiling( ceiling )
        P_AddThinker( ceiling:thinker )
        sec:specialdata := ceiling
        ceiling:sector := sec
        ceiling:crush := .F.

        SWITCH type
        CASE fastCrushAndRaise
            ceiling:crush := .T.
            ceiling:topheight := sec:ceilingheight
            ceiling:bottomheight := sec:floorheight + ( 8 * FRACUNIT )
            ceiling:direction := -1
            ceiling:speed := CEILSPEED * 2
            EXIT

        CASE silentCrushAndRaise
            ceiling:crush := .T.
            ceiling:topheight := sec:ceilingheight
            ceiling:bottomheight := sec:floorheight
            IF type != lowerToFloor
                ceiling:bottomheight += 8 * FRACUNIT
            ENDIF
            ceiling:direction := -1
            ceiling:speed := CEILSPEED
            EXIT

        CASE crushAndRaise
            ceiling:crush := .T.
            ceiling:topheight := sec:ceilingheight
            ceiling:bottomheight := sec:floorheight
            IF type != lowerToFloor
                ceiling:bottomheight += 8 * FRACUNIT
            ENDIF
            ceiling:direction := -1
            ceiling:speed := CEILSPEED
            EXIT

        CASE lowerAndCrush
            ceiling:bottomheight := sec:floorheight
            IF type != lowerToFloor
                ceiling:bottomheight += 8 * FRACUNIT
            ENDIF
            ceiling:direction := -1
            ceiling:speed := CEILSPEED
            EXIT

        CASE lowerToFloor
            ceiling:bottomheight := sec:floorheight
            IF type != lowerToFloor
                ceiling:bottomheight += 8 * FRACUNIT
            ENDIF
            ceiling:direction := -1
            ceiling:speed := CEILSPEED
            EXIT

        CASE raiseToHighest
            ceiling:topheight := P_FindHighestCeilingSurrounding( sec )
            ceiling:direction := 1
            ceiling:speed := CEILSPEED
            EXIT
        ENDSWITCH

        ceiling:tag := sec:tag
        ceiling:type := type
        P_AddActiveCeiling( ceiling )
    ENDDO
RETURN rtn

FUNCTION P_AddActiveCeiling( c )
    LOCAL i
    MEMVAR activeceilings

    FOR i := 0 TO MAXCEILINGS - 1
        IF activeceilings[ i + 1 ] == NIL
            activeceilings[ i + 1 ] := c
            RETURN NIL
        ENDIF
    NEXT
RETURN NIL

FUNCTION P_RemoveActiveCeiling( c )
    LOCAL i
    MEMVAR activeceilings

    FOR i := 0 TO MAXCEILINGS - 1
        IF activeceilings[ i + 1 ] == c
            activeceilings[ i + 1 ]:sector:specialdata := NIL
            P_RemoveThinker( activeceilings[ i + 1 ]:thinker )
            activeceilings[ i + 1 ] := NIL
            EXIT
        ENDIF
    NEXT
RETURN NIL

FUNCTION P_ActivateInStasisCeiling( line )
    LOCAL i
    MEMVAR activeceilings

    FOR i := 0 TO MAXCEILINGS - 1
        IF activeceilings[ i + 1 ] != NIL ;
           .AND. activeceilings[ i + 1 ]:tag == line:tag ;
           .AND. activeceilings[ i + 1 ]:direction == 0
            activeceilings[ i + 1 ]:direction := activeceilings[ i + 1 ]:olddirection
            BindMoveCeiling( activeceilings[ i + 1 ] )
        ENDIF
    NEXT
RETURN NIL

FUNCTION EV_CeilingCrushStop( line )
    LOCAL i
    LOCAL rtn
    MEMVAR activeceilings

    rtn := 0
    FOR i := 0 TO MAXCEILINGS - 1
        IF activeceilings[ i + 1 ] != NIL ;
           .AND. activeceilings[ i + 1 ]:tag == line:tag ;
           .AND. activeceilings[ i + 1 ]:direction != 0
            activeceilings[ i + 1 ]:olddirection := activeceilings[ i + 1 ]:direction
            activeceilings[ i + 1 ]:thinker:function:acv := NIL
            activeceilings[ i + 1 ]:thinkfn := ""
            activeceilings[ i + 1 ]:thinker:thinkfn := ""
            activeceilings[ i + 1 ]:direction := 0
            rtn := 1
        ENDIF
    NEXT
RETURN rtn
