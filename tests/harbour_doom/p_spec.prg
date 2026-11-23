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

STATIC lastanim
STATIC anims
STATIC animdefs
STATIC numlinespecials
STATIC linespeciallist
STATIC levelTimer
STATIC levelTimeCount

#include "p_spec.ch"

CLASS anim_t
    DATA istexture
    DATA picnum
    DATA basepic
    DATA numpics
    DATA speed
    METHOD New()
ENDCLASS
CLASS switchlist_t
    DATA name1
    DATA name2
    DATA episode
    METHOD New()
ENDCLASS
CLASS button_t
    DATA line
    DATA where
    DATA btexture
    DATA btimer
    DATA soundorg
    METHOD New()
ENDCLASS
#include "p_local.ch"
#include "p_tick.ch"
#include "p_mobj.ch"
#include "info.ch"
#include "doomstat.ch"
#include "doomdef.ch"
#include "d_mode.ch"
#include "m_bbox.ch"
#include "r_state.ch"


#define MAX_ADJOINING_SECTORS     20
#define DONUT_FLOORHEIGHT_DEFAULT 0x00000000
#define DONUT_FLOORPIC_DEFAULT    0x16


METHOD New() CLASS anim_t
    ::istexture := 0
    ::picnum := 0
    ::basepic := 0
    ::numpics := 0
    ::speed := 0
RETURN Self

METHOD New() CLASS switchlist_t
    ::name1 := ""
    ::name2 := ""
    ::episode := 0
RETURN Self

METHOD New() CLASS button_t
    ::line := NIL
    ::where := 0
    ::btexture := 0
    ::btimer := 0
    ::soundorg := NIL
RETURN Self

STATIC PROCEDURE BindMoveFloor( floor )
    LOCAL oFloor := floor
    floor:thinkfn := "T_MoveFloor"
    floor:thinker:thinkfn := "T_MoveFloor"
    floor:thinker:owner := floor
    floor:thinker:function:acp1 := {|| T_MoveFloor( oFloor ) }
RETURN

PROCEDURE P_InitPicAnims()
    LOCAL i
    LOCAL startname
    LOCAL endname
    LOCAL oAnim
    lastanim := 0
    i := 1
    DO WHILE animdefs[ i, 1 ] != -1
        startname := DEH_String( animdefs[ i, 3 ] )
        endname := DEH_String( animdefs[ i, 2 ] )

        IF animdefs[ i, 1 ] != 0
            IF R_CheckTextureNumForName( startname ) == -1
                i++
                LOOP
            ENDIF
            lastanim++
            oAnim := anims[ lastanim ]
            oAnim:picnum := R_TextureNumForName( endname )
            oAnim:basepic := R_TextureNumForName( startname )
        ELSE
            IF W_CheckNumForName( startname ) == -1
                i++
                LOOP
            ENDIF
            lastanim++
            oAnim := anims[ lastanim ]
            oAnim:picnum := R_FlatNumForName( endname )
            oAnim:basepic := R_FlatNumForName( startname )
        ENDIF

        oAnim:istexture := animdefs[ i, 1 ]
        oAnim:numpics := oAnim:picnum - oAnim:basepic + 1

        IF oAnim:numpics < 2
            I_Error( "P_InitPicAnims: bad cycle from " + startname + " to " + endname )
        ENDIF

        oAnim:speed := animdefs[ i, 4 ]
        i++
    ENDDO
RETURN

FUNCTION getSide( currentSector, line, side )
    LOCAL sec := sectors[ currentSector + 1 ]
    LOCAL ld := sec:lines[ line + 1 ]
    MEMVAR sides
RETURN sides[ ld:sidenum[ side + 1 ] + 1 ]

FUNCTION getSector( currentSector, line, side )
RETURN getSide( currentSector, line, side ):sector

FUNCTION twoSided( sector, line )
    MEMVAR sectors
RETURN ( sectors[ sector + 1 ]:lines[ line + 1 ]:flags & ML_TWOSIDED )

FUNCTION getNextSector( line, sec )
    IF ( line:flags & ML_TWOSIDED ) == 0
        RETURN NIL
    ENDIF
    IF line:frontsector == sec
        RETURN line:backsector
    ENDIF
RETURN line:frontsector

FUNCTION P_FindLowestFloorSurrounding( sec )
    LOCAL i
    LOCAL check
    LOCAL other
    LOCAL floor := sec:floorheight

    FOR i := 0 TO sec:linecount - 1
        check := sec:lines[ i + 1 ]
        other := getNextSector( check, sec )
        IF other == NIL
            LOOP
        ENDIF
        IF other:floorheight < floor
            floor := other:floorheight
        ENDIF
    NEXT
RETURN floor

FUNCTION P_FindHighestFloorSurrounding( sec )
    LOCAL i
    LOCAL check
    LOCAL other
    LOCAL floor := -500 * FRACUNIT

    FOR i := 0 TO sec:linecount - 1
        check := sec:lines[ i + 1 ]
        other := getNextSector( check, sec )
        IF other == NIL
            LOOP
        ENDIF
        IF other:floorheight > floor
            floor := other:floorheight
        ENDIF
    NEXT
RETURN floor

FUNCTION P_FindNextHighestFloor( sec, currentheight )
    LOCAL i
    LOCAL h
    LOCAL min
    LOCAL check
    LOCAL other
    LOCAL height := currentheight
    LOCAL heightlist := Array( MAX_ADJOINING_SECTORS + 2 )

    h := 0
    FOR i := 0 TO sec:linecount - 1
        check := sec:lines[ i + 1 ]
        other := getNextSector( check, sec )
        IF other == NIL
            LOOP
        ENDIF
        IF other:floorheight > height
            IF h == MAX_ADJOINING_SECTORS + 1
                height := other:floorheight
            ELSEIF h == MAX_ADJOINING_SECTORS + 2
                I_Error( "Sector with more than 22 adjoining sectors. Vanilla will crash here" )
            ENDIF
            heightlist[ h + 1 ] := other:floorheight
            h++
        ENDIF
    NEXT

    IF h == 0
        RETURN currentheight
    ENDIF

    min := heightlist[ 1 ]
    FOR i := 1 TO h - 1
        IF heightlist[ i + 1 ] < min
            min := heightlist[ i + 1 ]
        ENDIF
    NEXT
RETURN min

FUNCTION P_FindLowestCeilingSurrounding( sec )
    LOCAL i
    LOCAL check
    LOCAL other
    LOCAL height := INT_MAX

    FOR i := 0 TO sec:linecount - 1
        check := sec:lines[ i + 1 ]
        other := getNextSector( check, sec )
        IF other == NIL
            LOOP
        ENDIF
        IF other:ceilingheight < height
            height := other:ceilingheight
        ENDIF
    NEXT
RETURN height

FUNCTION P_FindHighestCeilingSurrounding( sec )
    LOCAL i
    LOCAL check
    LOCAL other
    LOCAL height := 0

    FOR i := 0 TO sec:linecount - 1
        check := sec:lines[ i + 1 ]
        other := getNextSector( check, sec )
        IF other == NIL
            LOOP
        ENDIF
        IF other:ceilingheight > height
            height := other:ceilingheight
        ENDIF
    NEXT
RETURN height

FUNCTION P_FindSectorFromLineTag( line, start )
    LOCAL i
    MEMVAR numsectors
    MEMVAR sectors

    FOR i := start + 1 TO numsectors - 1
        IF sectors[ i + 1 ]:tag == line:tag
            RETURN i
        ENDIF
    NEXT
RETURN -1

FUNCTION P_FindMinSurroundingLight( sector, max )
    LOCAL i
    LOCAL min := max
    LOCAL line
    LOCAL check

    FOR i := 0 TO sector:linecount - 1
        line := sector:lines[ i + 1 ]
        check := getNextSector( line, sector )
        IF check == NIL
            LOOP
        ENDIF
        IF check:lightlevel < min
            min := check:lightlevel
        ENDIF
    NEXT
RETURN min

PROCEDURE P_CrossSpecialLine( linenum, side, thing )
    LOCAL line
    LOCAL ok
    MEMVAR lines

    line := lines[ linenum + 1 ]

    IF thing:player == NIL
        SWITCH thing:type
        CASE MT_ROCKET
        CASE MT_PLASMA
        CASE MT_BFG
        CASE MT_TROOPSHOT
        CASE MT_HEADSHOT
        CASE MT_BRUISERSHOT
            RETURN
        ENDSWITCH

        ok := 0
        SWITCH line:special
        CASE 39
        CASE 97
        CASE 125
        CASE 126
        CASE 4
        CASE 10
        CASE 88
            ok := 1
        ENDSWITCH
        IF ok == 0
            RETURN
        ENDIF
    ENDIF

    SWITCH line:special
    CASE 2
        EV_DoDoor( line, vld_open )
        line:special := 0
        EXIT
    CASE 3
        EV_DoDoor( line, vld_close )
        line:special := 0
        EXIT
    CASE 4
        EV_DoDoor( line, vld_normal )
        line:special := 0
        EXIT
    CASE 5
        EV_DoFloor( line, raiseFloor )
        line:special := 0
        EXIT
    CASE 6
        EV_DoCeiling( line, fastCrushAndRaise )
        line:special := 0
        EXIT
    CASE 8
        EV_BuildStairs( line, build8 )
        line:special := 0
        EXIT
    CASE 10
        EV_DoPlat( line, downWaitUpStay, 0 )
        line:special := 0
        EXIT
    CASE 12
        EV_LightTurnOn( line, 0 )
        line:special := 0
        EXIT
    CASE 13
        EV_LightTurnOn( line, 255 )
        line:special := 0
        EXIT
    CASE 16
        EV_DoDoor( line, vld_close30ThenOpen )
        line:special := 0
        EXIT
    CASE 17
        EV_StartLightStrobing( line )
        line:special := 0
        EXIT
    CASE 19
        EV_DoFloor( line, lowerFloor )
        line:special := 0
        EXIT
    CASE 22
        EV_DoPlat( line, raiseToNearestAndChange, 0 )
        line:special := 0
        EXIT
    CASE 25
        EV_DoCeiling( line, crushAndRaise )
        line:special := 0
        EXIT
    CASE 30
        EV_DoFloor( line, raiseToTexture )
        line:special := 0
        EXIT
    CASE 35
        EV_LightTurnOn( line, 35 )
        line:special := 0
        EXIT
    CASE 36
        EV_DoFloor( line, turboLower )
        line:special := 0
        EXIT
    CASE 37
        EV_DoFloor( line, lowerAndChange )
        line:special := 0
        EXIT
    CASE 38
        EV_DoFloor( line, lowerFloorToLowest )
        line:special := 0
        EXIT
    CASE 39
        EV_Teleport( line, side, thing )
        line:special := 0
        EXIT
    CASE 40
        EV_DoCeiling( line, raiseToHighest )
        EV_DoFloor( line, lowerFloorToLowest )
        line:special := 0
        EXIT
    CASE 44
        EV_DoCeiling( line, lowerAndCrush )
        line:special := 0
        EXIT
    CASE 52
        G_ExitLevel()
        EXIT
    CASE 53
        EV_DoPlat( line, perpetualRaise, 0 )
        line:special := 0
        EXIT
    CASE 54
        EV_StopPlat( line )
        line:special := 0
        EXIT
    CASE 56
        EV_DoFloor( line, raiseFloorCrush )
        line:special := 0
        EXIT
    CASE 57
        EV_CeilingCrushStop( line )
        line:special := 0
        EXIT
    CASE 58
        EV_DoFloor( line, raiseFloor24 )
        line:special := 0
        EXIT
    CASE 59
        EV_DoFloor( line, raiseFloor24AndChange )
        line:special := 0
        EXIT
    CASE 104
        EV_TurnTagLightsOff( line )
        line:special := 0
        EXIT
    CASE 108
        EV_DoDoor( line, vld_blazeRaise )
        line:special := 0
        EXIT
    CASE 109
        EV_DoDoor( line, vld_blazeOpen )
        line:special := 0
        EXIT
    CASE 100
        EV_BuildStairs( line, turbo16 )
        line:special := 0
        EXIT
    CASE 110
        EV_DoDoor( line, vld_blazeClose )
        line:special := 0
        EXIT
    CASE 119
        EV_DoFloor( line, raiseFloorToNearest )
        line:special := 0
        EXIT
    CASE 121
        EV_DoPlat( line, blazeDWUS, 0 )
        line:special := 0
        EXIT
    CASE 124
        G_SecretExitLevel()
        EXIT
    CASE 125
        IF thing:player == NIL
            EV_Teleport( line, side, thing )
            line:special := 0
        ENDIF
        EXIT
    CASE 130
        EV_DoFloor( line, raiseFloorTurbo )
        line:special := 0
        EXIT
    CASE 141
        EV_DoCeiling( line, silentCrushAndRaise )
        line:special := 0
        EXIT
    CASE 72
        EV_DoCeiling( line, lowerAndCrush )
        EXIT
    CASE 73
        EV_DoCeiling( line, crushAndRaise )
        EXIT
    CASE 74
        EV_CeilingCrushStop( line )
        EXIT
    CASE 75
        EV_DoDoor( line, vld_close )
        EXIT
    CASE 76
        EV_DoDoor( line, vld_close30ThenOpen )
        EXIT
    CASE 77
        EV_DoCeiling( line, fastCrushAndRaise )
        EXIT
    CASE 79
        EV_LightTurnOn( line, 35 )
        EXIT
    CASE 80
        EV_LightTurnOn( line, 0 )
        EXIT
    CASE 81
        EV_LightTurnOn( line, 255 )
        EXIT
    CASE 82
        EV_DoFloor( line, lowerFloorToLowest )
        EXIT
    CASE 83
        EV_DoFloor( line, lowerFloor )
        EXIT
    CASE 84
        EV_DoFloor( line, lowerAndChange )
        EXIT
    CASE 86
        EV_DoDoor( line, vld_open )
        EXIT
    CASE 87
        EV_DoPlat( line, perpetualRaise, 0 )
        EXIT
    CASE 88
        EV_DoPlat( line, downWaitUpStay, 0 )
        EXIT
    CASE 89
        EV_StopPlat( line )
        EXIT
    CASE 90
        EV_DoDoor( line, vld_normal )
        EXIT
    CASE 91
        EV_DoFloor( line, raiseFloor )
        EXIT
    CASE 92
        EV_DoFloor( line, raiseFloor24 )
        EXIT
    CASE 93
        EV_DoFloor( line, raiseFloor24AndChange )
        EXIT
    CASE 94
        EV_DoFloor( line, raiseFloorCrush )
        EXIT
    CASE 95
        EV_DoPlat( line, raiseToNearestAndChange, 0 )
        EXIT
    CASE 96
        EV_DoFloor( line, raiseToTexture )
        EXIT
    CASE 97
        EV_Teleport( line, side, thing )
        EXIT
    CASE 98
        EV_DoFloor( line, turboLower )
        EXIT
    CASE 105
        EV_DoDoor( line, vld_blazeRaise )
        EXIT
    CASE 106
        EV_DoDoor( line, vld_blazeOpen )
        EXIT
    CASE 107
        EV_DoDoor( line, vld_blazeClose )
        EXIT
    CASE 120
        EV_DoPlat( line, blazeDWUS, 0 )
        EXIT
    CASE 126
        IF thing:player == NIL
            EV_Teleport( line, side, thing )
        ENDIF
        EXIT
    CASE 128
        EV_DoFloor( line, raiseFloorToNearest )
        EXIT
    CASE 129
        EV_DoFloor( line, raiseFloorTurbo )
        EXIT
    ENDSWITCH
RETURN

PROCEDURE P_ShootSpecialLine( thing, line )
    LOCAL ok

    IF thing:player == NIL
        ok := 0
        SWITCH line:special
        CASE 46
            ok := 1
        ENDSWITCH
        IF ok == 0
            RETURN
        ENDIF
    ENDIF

    SWITCH line:special
    CASE 24
        EV_DoFloor( line, raiseFloor )
        P_ChangeSwitchTexture( line, 0 )
        EXIT
    CASE 46
        EV_DoDoor( line, vld_open )
        P_ChangeSwitchTexture( line, 1 )
        EXIT
    CASE 47
        EV_DoPlat( line, raiseToNearestAndChange, 0 )
        P_ChangeSwitchTexture( line, 0 )
        EXIT
    ENDSWITCH
RETURN

PROCEDURE P_PlayerInSpecialSector( player )
    LOCAL sector
    MEMVAR leveltime

    sector := player:mo:subsector:sector

    IF player:mo:z != sector:floorheight
        RETURN
    ENDIF

    SWITCH sector:special
    CASE 5
        IF player:powers[ pw_ironfeet + 1 ] == 0
            IF ( leveltime & 0x1f ) == 0
                P_DamageMobj( player:mo, NIL, NIL, 10 )
            ENDIF
        ENDIF
        EXIT
    CASE 7
        IF player:powers[ pw_ironfeet + 1 ] == 0
            IF ( leveltime & 0x1f ) == 0
                P_DamageMobj( player:mo, NIL, NIL, 5 )
            ENDIF
        ENDIF
        EXIT
    CASE 16
    CASE 4
        IF player:powers[ pw_ironfeet + 1 ] == 0 .OR. P_Random() < 5
            IF ( leveltime & 0x1f ) == 0
                P_DamageMobj( player:mo, NIL, NIL, 20 )
            ENDIF
        ENDIF
        EXIT
    CASE 9
        player:secretcount++
        sector:special := 0
        EXIT
    CASE 11
        player:cheats := ( player:cheats & ( CF_GODMODE ^^ 0xFFFFFFFF ) )
        IF ( leveltime & 0x1f ) == 0
            P_DamageMobj( player:mo, NIL, NIL, 20 )
        ENDIF
        IF player:health <= 10
            G_ExitLevel()
        ENDIF
        EXIT
    OTHERWISE
        I_Error( "P_PlayerInSpecialSector: unknown special " + LTrim( Str( sector:special ) ) )
    ENDSWITCH
RETURN

PROCEDURE P_UpdateSpecials()
    LOCAL i
    LOCAL pic
    LOCAL anim
    LOCAL line
    LOCAL sid
    MEMVAR buttonlist
    MEMVAR flattranslation
    MEMVAR leveltime
    MEMVAR sides
    MEMVAR texturetranslation

    IF levelTimer
        levelTimeCount--
        IF levelTimeCount == 0
            G_ExitLevel()
        ENDIF
    ENDIF

    FOR i := 1 TO lastanim
        anim := anims[ i ]
        FOR pic := anim:basepic TO anim:basepic + anim:numpics - 1
            IF anim:istexture != 0
                texturetranslation[ pic + 1 ] := anim:basepic + ( ( Int( leveltime / anim:speed ) + pic ) % anim:numpics )
            ELSE
                flattranslation[ pic + 1 ] := anim:basepic + ( ( Int( leveltime / anim:speed ) + pic ) % anim:numpics )
            ENDIF
        NEXT
    NEXT

    FOR i := 0 TO numlinespecials - 1
        line := linespeciallist[ i + 1 ]
        SWITCH line:special
        CASE 48
            sides[ line:sidenum[ 1 ] + 1 ]:textureoffset += FRACUNIT
        ENDSWITCH
    NEXT

    FOR i := 1 TO MAXBUTTONS
        IF buttonlist[ i ]:btimer != 0
            buttonlist[ i ]:btimer--
            IF buttonlist[ i ]:btimer == 0
                sid := sides[ buttonlist[ i ]:line:sidenum[ 1 ] + 1 ]
                SWITCH buttonlist[ i ]:where
                CASE sw_top
                    sid:toptexture := buttonlist[ i ]:btexture
                    EXIT
                CASE sw_middle
                    sid:midtexture := buttonlist[ i ]:btexture
                    EXIT
                CASE sw_bottom
                    sid:bottomtexture := buttonlist[ i ]:btexture
                    EXIT
                ENDSWITCH
                S_StartSound( buttonlist[ i ]:soundorg, sfx_swtchn )
                buttonlist[ i ] := button_t():New()
            ENDIF
        ENDIF
    NEXT
RETURN

STATIC FUNCTION DonutOverrun( line, pillar_sector )
    STATIC first := .T.
    STATIC tmp_s3_floorheight := DONUT_FLOORHEIGHT_DEFAULT
    STATIC tmp_s3_floorpic := DONUT_FLOORPIC_DEFAULT
    LOCAL p
    MEMVAR myargv
    MEMVAR numflats

    HB_SYMBOL_UNUSED( line )
    HB_SYMBOL_UNUSED( pillar_sector )

    IF first
        first := .F.
        tmp_s3_floorheight := DONUT_FLOORHEIGHT_DEFAULT
        tmp_s3_floorpic := DONUT_FLOORPIC_DEFAULT
        p := M_CheckParmWithArgs( "-donut", 2 )
        IF p > 0
            M_StrToInt( myargv[ p + 1 + 1 ], @tmp_s3_floorheight )
            M_StrToInt( myargv[ p + 2 + 1 ], @tmp_s3_floorpic )
            IF tmp_s3_floorpic >= numflats
                OutStd( "DonutOverrun: The second parameter for " + Chr( 34 ) + "-donut" + Chr( 34 ) + ;
                        " switch should be greater than 0 and less than number of flats (" + ;
                        LTrim( Str( numflats ) ) + "). Using default value (" + ;
                        LTrim( Str( DONUT_FLOORPIC_DEFAULT ) ) + ") instead." + hb_eol() )
                tmp_s3_floorpic := DONUT_FLOORPIC_DEFAULT
            ENDIF
        ENDIF
    ENDIF
RETURN { tmp_s3_floorheight, tmp_s3_floorpic }

FUNCTION EV_DoDonut( line )
    LOCAL s1
    LOCAL s2
    LOCAL s3
    LOCAL secnum
    LOCAL rtn
    LOCAL i
    LOCAL floor
    LOCAL s3_floorheight
    LOCAL s3_floorpic
    LOCAL overrun
    MEMVAR sectors

    secnum := -1
    rtn := 0
    DO WHILE .T.
        secnum := P_FindSectorFromLineTag( line, secnum )
        IF secnum < 0
            EXIT
        ENDIF
        s1 := sectors[ secnum + 1 ]
        IF s1:specialdata != NIL
            LOOP
        ENDIF
        rtn := 1
        s2 := getNextSector( s1:lines[ 1 ], s1 )
        IF s2 == NIL
            OutStd( "EV_DoDonut: linedef had no second sidedef! Unexpected behavior may occur in Vanilla Doom." + hb_eol() )
            EXIT
        ENDIF

        FOR i := 0 TO s2:linecount - 1
            s3 := s2:lines[ i + 1 ]:backsector
            IF s3 == s1
                LOOP
            ENDIF
            IF s3 == NIL
                OutStd( "EV_DoDonut: WARNING: emulating buffer overrun due to NULL back sector. Unexpected behavior may occur in Vanilla Doom." + hb_eol() )
                overrun := DonutOverrun( line, s1 )
                s3_floorheight := overrun[ 1 ]
                s3_floorpic := overrun[ 2 ]
            ELSE
                s3_floorheight := s3:floorheight
                s3_floorpic := s3:floorpic
            ENDIF

            floor := floormove_t():New()
            BindMoveFloor( floor )
            P_AddThinker( floor:thinker )
            s2:specialdata := floor
            floor:type := donutRaise
            floor:crush := .F.
            floor:direction := 1
            floor:sector := s2
            floor:speed := Int( FLOORSPEED / 2 )
            floor:texture := s3_floorpic
            floor:newspecial := 0
            floor:floordestheight := s3_floorheight

            floor := floormove_t():New()
            BindMoveFloor( floor )
            P_AddThinker( floor:thinker )
            s1:specialdata := floor
            floor:type := lowerFloor
            floor:crush := .F.
            floor:direction := -1
            floor:sector := s1
            floor:speed := Int( FLOORSPEED / 2 )
            floor:floordestheight := s3_floorheight
            EXIT
        NEXT
    ENDDO
RETURN rtn

PROCEDURE P_SpawnSpecials()
    LOCAL sector
    LOCAL i
    MEMVAR activeceilings
    MEMVAR activeplats
    MEMVAR buttonlist
    MEMVAR deathmatch
    MEMVAR lines
    MEMVAR numlines
    MEMVAR numsectors
    MEMVAR sectors
    MEMVAR timelimit
    MEMVAR totalsecret

    IF timelimit > 0 .AND. deathmatch != 0
        levelTimer := .T.
        levelTimeCount := timelimit * 60 * TICRATE
    ELSE
        levelTimer := .F.
    ENDIF

    FOR i := 0 TO numsectors - 1
        sector := sectors[ i + 1 ]
        IF sector:special == 0
            LOOP
        ENDIF
        SWITCH sector:special
        CASE 1
            P_SpawnLightFlash( sector )
            EXIT
        CASE 2
            P_SpawnStrobeFlash( sector, FASTDARK, 0 )
            EXIT
        CASE 3
            P_SpawnStrobeFlash( sector, SLOWDARK, 0 )
            EXIT
        CASE 4
            P_SpawnStrobeFlash( sector, FASTDARK, 0 )
            sector:special := 4
            EXIT
        CASE 8
            P_SpawnGlowingLight( sector )
            EXIT
        CASE 9
            totalsecret++
            EXIT
        CASE 10
            P_SpawnDoorCloseIn30( sector )
            EXIT
        CASE 12
            P_SpawnStrobeFlash( sector, SLOWDARK, 1 )
            EXIT
        CASE 13
            P_SpawnStrobeFlash( sector, FASTDARK, 1 )
            EXIT
        CASE 14
            P_SpawnDoorRaiseIn5Mins( sector, i )
            EXIT
        CASE 17
            P_SpawnFireFlicker( sector )
            EXIT
        ENDSWITCH
    NEXT

    numlinespecials := 0
    FOR i := 0 TO numlines - 1
        SWITCH lines[ i + 1 ]:special
        CASE 48
            IF numlinespecials >= MAXLINEANIMS
                I_Error( "Too many scrolling wall linedefs! (Vanilla limit is 64)" )
            ENDIF
            linespeciallist[ numlinespecials + 1 ] := lines[ i + 1 ]
            numlinespecials++
        ENDSWITCH
    NEXT

    FOR i := 1 TO MAXCEILINGS
        activeceilings[ i ] := NIL
    NEXT
    FOR i := 1 TO MAXPLATS
        activeplats[ i ] := NIL
    NEXT
    FOR i := 1 TO MAXBUTTONS
        buttonlist[ i ] := button_t():New()
    NEXT
RETURN

INIT PROCEDURE init_p_spec
    LOCAL i

    PUBLIC buttonlist

    anims := {}
    FOR i := 1 TO MAXANIMS
        AAdd( anims, anim_t():New() )
    NEXT
    lastanim := 0
    animdefs := { ;
        { 0, "NUKAGE3", "NUKAGE1", 8 }, ;
        { 0, "FWATER4", "FWATER1", 8 }, ;
        { 0, "SWATER4", "SWATER1", 8 }, ;
        { 0, "LAVA4", "LAVA1", 8 }, ;
        { 0, "BLOOD3", "BLOOD1", 8 }, ;
        { 0, "RROCK08", "RROCK05", 8 }, ;
        { 0, "SLIME04", "SLIME01", 8 }, ;
        { 0, "SLIME08", "SLIME05", 8 }, ;
        { 0, "SLIME12", "SLIME09", 8 }, ;
        { 1, "BLODGR4", "BLODGR1", 8 }, ;
        { 1, "SLADRIP3", "SLADRIP1", 8 }, ;
        { 1, "BLODRIP4", "BLODRIP1", 8 }, ;
        { 1, "FIREWALL", "FIREWALA", 8 }, ;
        { 1, "GSTFONT3", "GSTFONT1", 8 }, ;
        { 1, "FIRELAVA", "FIRELAV3", 8 }, ;
        { 1, "FIREMAG3", "FIREMAG1", 8 }, ;
        { 1, "FIREBLU2", "FIREBLU1", 8 }, ;
        { 1, "ROCKRED3", "ROCKRED1", 8 }, ;
        { 1, "BFALL4", "BFALL1", 8 }, ;
        { 1, "SFALL4", "SFALL1", 8 }, ;
        { 1, "WFALL4", "WFALL1", 8 }, ;
        { 1, "DBRAIN4", "DBRAIN1", 8 }, ;
        { -1, "", "", 0 } }

    numlinespecials := 0
    linespeciallist := Array( MAXLINEANIMS )
    buttonlist := {}
    FOR i := 1 TO MAXBUTTONS
        AAdd( buttonlist, button_t():New() )
    NEXT
    levelTimer := .F.
    levelTimeCount := 0
RETURN
