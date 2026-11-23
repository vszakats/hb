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

#include "p_floor.ch"

CLASS floormove_t
    DATA thinker
    DATA thinkfn
    DATA type
    DATA crush
    DATA sector
    DATA direction
    DATA newspecial
    DATA texture
    DATA floordestheight
    DATA speed
    METHOD New()
ENDCLASS

METHOD New() CLASS floormove_t
    ::thinker         := thinker_t():New()
    ::thinkfn         := ""
    ::type            := 0
    ::crush           := .F.
    ::sector          := NIL
    ::direction       := 0
    ::newspecial      := 0
    ::texture         := 0
    ::floordestheight := 0
    ::speed           := 0
RETURN Self

STATIC PROCEDURE BindMoveFloor( floor )
    LOCAL oFloor := floor
    floor:thinkfn := "T_MoveFloor"
    floor:thinker:thinkfn := "T_MoveFloor"
    floor:thinker:owner := floor
    floor:thinker:function:acp1 := {|| T_MoveFloor( oFloor ) }
RETURN

FUNCTION T_MovePlane( sector, speed, dest, crush, floorOrCeiling, direction )
    LOCAL flag
    LOCAL lastpos

    SWITCH floorOrCeiling
    CASE 0
        SWITCH direction
        CASE -1
            IF sector:floorheight - speed < dest
                lastpos := sector:floorheight
                sector:floorheight := dest
                flag := P_ChangeSector( sector, crush )
                IF flag
                    sector:floorheight := lastpos
                    P_ChangeSector( sector, crush )
                ENDIF
                RETURN result_pastdest
            ELSE
                lastpos := sector:floorheight
                sector:floorheight -= speed
                flag := P_ChangeSector( sector, crush )
                IF flag
                    sector:floorheight := lastpos
                    P_ChangeSector( sector, crush )
                    RETURN result_crushed
                ENDIF
            ENDIF
            EXIT

        CASE 1
            IF sector:floorheight + speed > dest
                lastpos := sector:floorheight
                sector:floorheight := dest
                flag := P_ChangeSector( sector, crush )
                IF flag
                    sector:floorheight := lastpos
                    P_ChangeSector( sector, crush )
                ENDIF
                RETURN result_pastdest
            ELSE
                lastpos := sector:floorheight
                sector:floorheight += speed
                flag := P_ChangeSector( sector, crush )
                IF flag
                    IF crush
                        RETURN result_crushed
                    ENDIF
                    sector:floorheight := lastpos
                    P_ChangeSector( sector, crush )
                    RETURN result_crushed
                ENDIF
            ENDIF
            EXIT
        ENDSWITCH
        EXIT

    CASE 1
        SWITCH direction
        CASE -1
            IF sector:ceilingheight - speed < dest
                lastpos := sector:ceilingheight
                sector:ceilingheight := dest
                flag := P_ChangeSector( sector, crush )
                IF flag
                    sector:ceilingheight := lastpos
                    P_ChangeSector( sector, crush )
                ENDIF
                RETURN result_pastdest
            ELSE
                lastpos := sector:ceilingheight
                sector:ceilingheight -= speed
                flag := P_ChangeSector( sector, crush )
                IF flag
                    IF crush
                        RETURN result_crushed
                    ENDIF
                    sector:ceilingheight := lastpos
                    P_ChangeSector( sector, crush )
                    RETURN result_crushed
                ENDIF
            ENDIF
            EXIT

        CASE 1
            IF sector:ceilingheight + speed > dest
                lastpos := sector:ceilingheight
                sector:ceilingheight := dest
                flag := P_ChangeSector( sector, crush )
                IF flag
                    sector:ceilingheight := lastpos
                    P_ChangeSector( sector, crush )
                ENDIF
                RETURN result_pastdest
            ELSE
                lastpos := sector:ceilingheight
                sector:ceilingheight += speed
                flag := P_ChangeSector( sector, crush )
            ENDIF
            EXIT
        ENDSWITCH
        EXIT
    ENDSWITCH
RETURN result_ok

FUNCTION T_MoveFloor( floor )
    LOCAL res
    MEMVAR leveltime

    res := T_MovePlane( floor:sector, floor:speed, floor:floordestheight, ;
                        floor:crush, 0, floor:direction )

    IF ( leveltime & 7 ) == 0
        S_StartSound( floor:sector:soundorg, sfx_stnmov )
    ENDIF

    IF res == result_pastdest
        floor:sector:specialdata := NIL

        IF floor:direction == 1
            SWITCH floor:type
            CASE donutRaise
                floor:sector:special := floor:newspecial
                floor:sector:floorpic := floor:texture
                EXIT
            OTHERWISE
                EXIT
            ENDSWITCH
        ELSEIF floor:direction == -1
            SWITCH floor:type
            CASE lowerAndChange
                floor:sector:special := floor:newspecial
                floor:sector:floorpic := floor:texture
                EXIT
            OTHERWISE
                EXIT
            ENDSWITCH
        ENDIF
        P_RemoveThinker( floor:thinker )
        S_StartSound( floor:sector:soundorg, sfx_pstop )
    ENDIF
RETURN NIL

FUNCTION EV_DoFloor( line, floortype )
    LOCAL secnum
    LOCAL rtn
    LOCAL i
    LOCAL sec
    LOCAL floor
    LOCAL minsize
    LOCAL side
    LOCAL oSec
    MEMVAR sectors
    MEMVAR textureheight

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
        floor := floormove_t():New()
        BindMoveFloor( floor )
        P_AddThinker( floor:thinker )
        sec:specialdata := floor
        floor:type := floortype
        floor:crush := .F.

        SWITCH floortype
        CASE lowerFloor
            floor:direction := -1
            floor:sector := sec
            floor:speed := FLOORSPEED
            floor:floordestheight := P_FindHighestFloorSurrounding( sec )
            EXIT

        CASE lowerFloorToLowest
            floor:direction := -1
            floor:sector := sec
            floor:speed := FLOORSPEED
            floor:floordestheight := P_FindLowestFloorSurrounding( sec )
            EXIT

        CASE turboLower
            floor:direction := -1
            floor:sector := sec
            floor:speed := FLOORSPEED * 4
            floor:floordestheight := P_FindHighestFloorSurrounding( sec )
            IF floor:floordestheight != sec:floorheight
                floor:floordestheight += 8 * FRACUNIT
            ENDIF
            EXIT

        CASE raiseFloorCrush
            floor:crush := .T.
            floor:direction := 1
            floor:sector := sec
            floor:speed := FLOORSPEED
            floor:floordestheight := P_FindLowestCeilingSurrounding( sec )
            IF floor:floordestheight > sec:ceilingheight
                floor:floordestheight := sec:ceilingheight
            ENDIF
            floor:floordestheight -= ( 8 * FRACUNIT ) * iif( floortype == raiseFloorCrush, 1, 0 )
            EXIT

        CASE raiseFloor
            floor:direction := 1
            floor:sector := sec
            floor:speed := FLOORSPEED
            floor:floordestheight := P_FindLowestCeilingSurrounding( sec )
            IF floor:floordestheight > sec:ceilingheight
                floor:floordestheight := sec:ceilingheight
            ENDIF
            floor:floordestheight -= ( 8 * FRACUNIT ) * iif( floortype == raiseFloorCrush, 1, 0 )
            EXIT

        CASE raiseFloorTurbo
            floor:direction := 1
            floor:sector := sec
            floor:speed := FLOORSPEED * 4
            floor:floordestheight := P_FindNextHighestFloor( sec, sec:floorheight )
            EXIT

        CASE raiseFloorToNearest
            floor:direction := 1
            floor:sector := sec
            floor:speed := FLOORSPEED
            floor:floordestheight := P_FindNextHighestFloor( sec, sec:floorheight )
            EXIT

        CASE raiseFloor24
            floor:direction := 1
            floor:sector := sec
            floor:speed := FLOORSPEED
            floor:floordestheight := floor:sector:floorheight + 24 * FRACUNIT
            EXIT

        CASE raiseFloor512
            floor:direction := 1
            floor:sector := sec
            floor:speed := FLOORSPEED
            floor:floordestheight := floor:sector:floorheight + 512 * FRACUNIT
            EXIT

        CASE raiseFloor24AndChange
            floor:direction := 1
            floor:sector := sec
            floor:speed := FLOORSPEED
            floor:floordestheight := floor:sector:floorheight + 24 * FRACUNIT
            sec:floorpic := line:frontsector:floorpic
            sec:special := line:frontsector:special
            EXIT

        CASE raiseToTexture
            minsize := INT_MAX
            floor:direction := 1
            floor:sector := sec
            floor:speed := FLOORSPEED
            FOR i := 0 TO sec:linecount - 1
                IF twoSided( secnum, i )
                    side := getSide( secnum, i, 0 )
                    IF side:bottomtexture >= 0
                        IF textureheight[ side:bottomtexture + 1 ] < minsize
                            minsize := textureheight[ side:bottomtexture + 1 ]
                        ENDIF
                    ENDIF
                    side := getSide( secnum, i, 1 )
                    IF side:bottomtexture >= 0
                        IF textureheight[ side:bottomtexture + 1 ] < minsize
                            minsize := textureheight[ side:bottomtexture + 1 ]
                        ENDIF
                    ENDIF
                ENDIF
            NEXT
            floor:floordestheight := floor:sector:floorheight + minsize
            EXIT

        CASE lowerAndChange
            floor:direction := -1
            floor:sector := sec
            floor:speed := FLOORSPEED
            floor:floordestheight := P_FindLowestFloorSurrounding( sec )
            floor:texture := sec:floorpic
            FOR i := 0 TO sec:linecount - 1
                IF twoSided( secnum, i )
                    IF getSide( secnum, i, 0 ):sector:iSector == secnum
                        oSec := getSector( secnum, i, 1 )
                        IF oSec:floorheight == floor:floordestheight
                            floor:texture := oSec:floorpic
                            floor:newspecial := oSec:special
                            EXIT
                        ENDIF
                    ELSE
                        oSec := getSector( secnum, i, 0 )
                        IF oSec:floorheight == floor:floordestheight
                            floor:texture := oSec:floorpic
                            floor:newspecial := oSec:special
                            EXIT
                        ENDIF
                    ENDIF
                ENDIF
            NEXT
            EXIT

        OTHERWISE
            EXIT
        ENDSWITCH
    ENDDO
RETURN rtn

FUNCTION EV_BuildStairs( line, type )
    LOCAL secnum
    LOCAL height
    LOCAL i
    LOCAL newsecnum
    LOCAL texture
    LOCAL ok
    LOCAL rtn
    LOCAL sec
    LOCAL tsec
    LOCAL floor
    LOCAL stairsize
    LOCAL speed
    MEMVAR sectors

    stairsize := 0
    speed := 0
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
        floor := floormove_t():New()
        BindMoveFloor( floor )
        P_AddThinker( floor:thinker )
        sec:specialdata := floor
        floor:direction := 1
        floor:sector := sec
        SWITCH type
        CASE build8
            speed := Int( FLOORSPEED / 4 )
            stairsize := 8 * FRACUNIT
            EXIT
        CASE turbo16
            speed := FLOORSPEED * 4
            stairsize := 16 * FRACUNIT
            EXIT
        ENDSWITCH
        floor:speed := speed
        height := sec:floorheight + stairsize
        floor:floordestheight := height
        texture := sec:floorpic

        DO WHILE .T.
            ok := 0
            FOR i := 0 TO sec:linecount - 1
                IF ( sec:lines[ i + 1 ]:flags & ML_TWOSIDED ) == 0
                    LOOP
                ENDIF
                tsec := sec:lines[ i + 1 ]:frontsector
                newsecnum := tsec:iSector
                IF secnum != newsecnum
                    LOOP
                ENDIF
                tsec := sec:lines[ i + 1 ]:backsector
                newsecnum := tsec:iSector
                IF tsec:floorpic != texture
                    LOOP
                ENDIF
                height += stairsize
                IF tsec:specialdata != NIL
                    LOOP
                ENDIF
                sec := tsec
                secnum := newsecnum
                floor := floormove_t():New()
                BindMoveFloor( floor )
                P_AddThinker( floor:thinker )
                sec:specialdata := floor
                floor:direction := 1
                floor:sector := sec
                floor:speed := speed
                floor:floordestheight := height
                ok := 1
                EXIT
            NEXT
            IF ok == 0
                EXIT
            ENDIF
        ENDDO
    ENDDO
RETURN rtn
