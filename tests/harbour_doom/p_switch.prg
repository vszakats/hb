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

STATIC switchlist
STATIC numswitches
STATIC alphSwitchList

#include "p_spec.ch"
#include "p_local.ch"
#include "p_mobj.ch"
#include "doomstat.ch"
#include "doomdef.ch"
#include "d_mode.ch"



PROCEDURE P_InitSwitchList()
    LOCAL i
    LOCAL index
    LOCAL episode
    MEMVAR gamemode

    episode := 1
    IF gamemode == registered .OR. gamemode == retail
        episode := 2
    ELSEIF gamemode == commercial
        episode := 3
    ENDIF

    index := 0
    FOR i := 0 TO MAXSWITCHES - 1
        IF alphSwitchList[ i + 1, 3 ] == 0
            numswitches := Int( index / 2 )
            switchlist[ index + 1 ] := -1
            EXIT
        ENDIF
        IF alphSwitchList[ i + 1, 3 ] <= episode
            switchlist[ index + 1 ] := R_TextureNumForName( DEH_String( alphSwitchList[ i + 1, 1 ] ) )
            index++
            switchlist[ index + 1 ] := R_TextureNumForName( DEH_String( alphSwitchList[ i + 1, 2 ] ) )
            index++
        ENDIF
    NEXT
RETURN

PROCEDURE P_StartButton( line, w, texture, time )
    LOCAL i
    MEMVAR buttonlist

    FOR i := 1 TO MAXBUTTONS
        IF buttonlist[ i ]:btimer != 0 .AND. buttonlist[ i ]:line == line
            RETURN
        ENDIF
    NEXT

    FOR i := 1 TO MAXBUTTONS
        IF buttonlist[ i ]:btimer == 0
            buttonlist[ i ]:line := line
            buttonlist[ i ]:where := w
            buttonlist[ i ]:btexture := texture
            buttonlist[ i ]:btimer := time
            buttonlist[ i ]:soundorg := line:frontsector:soundorg
            RETURN
        ENDIF
    NEXT

    I_Error( "P_StartButton: no button slots left!" )
RETURN

PROCEDURE P_ChangeSwitchTexture( line, useAgain )
    LOCAL texTop
    LOCAL texMid
    LOCAL texBot
    LOCAL i
    LOCAL sound
    LOCAL sid
    MEMVAR buttonlist
    MEMVAR sides

    IF useAgain == 0
        line:special := 0
    ENDIF

    sid := sides[ line:sidenum[ 1 ] + 1 ]
    texTop := sid:toptexture
    texMid := sid:midtexture
    texBot := sid:bottomtexture

    sound := sfx_swtchn
    IF line:special == 11
        sound := sfx_swtchx
    ENDIF

    FOR i := 0 TO numswitches * 2 - 1
        IF switchlist[ i + 1 ] == texTop
            S_StartSound( buttonlist[ 1 ]:soundorg, sound )
            sid:toptexture := switchlist[ ( i ^^ 1 ) + 1 ]
            IF useAgain != 0
                P_StartButton( line, sw_top, switchlist[ i + 1 ], BUTTONTIME )
            ENDIF
            RETURN
        ELSEIF switchlist[ i + 1 ] == texMid
            S_StartSound( buttonlist[ 1 ]:soundorg, sound )
            sid:midtexture := switchlist[ ( i ^^ 1 ) + 1 ]
            IF useAgain != 0
                P_StartButton( line, sw_middle, switchlist[ i + 1 ], BUTTONTIME )
            ENDIF
            RETURN
        ELSEIF switchlist[ i + 1 ] == texBot
            S_StartSound( buttonlist[ 1 ]:soundorg, sound )
            sid:bottomtexture := switchlist[ ( i ^^ 1 ) + 1 ]
            IF useAgain != 0
                P_StartButton( line, sw_bottom, switchlist[ i + 1 ], BUTTONTIME )
            ENDIF
            RETURN
        ENDIF
    NEXT
RETURN

FUNCTION P_UseSpecialLine( thing, line, side )
    IF side != 0
        SWITCH line:special
        CASE 124
            EXIT
        OTHERWISE
            RETURN .F.
        ENDSWITCH
    ENDIF

    IF thing:player == NIL
        IF ( line:flags & ML_SECRET ) != 0
            RETURN .F.
        ENDIF
        SWITCH line:special
        CASE 1
        CASE 32
        CASE 33
        CASE 34
            EXIT
        OTHERWISE
            RETURN .F.
        ENDSWITCH
    ENDIF

    SWITCH line:special
    CASE 1
    CASE 26
    CASE 27
    CASE 28
    CASE 31
    CASE 32
    CASE 33
    CASE 34
    CASE 117
    CASE 118
        EV_VerticalDoor( line, thing )
        EXIT
    CASE 7
        IF EV_BuildStairs( line, build8 ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 9
        IF EV_DoDonut( line ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 11
        P_ChangeSwitchTexture( line, 0 )
        G_ExitLevel()
        EXIT
    CASE 14
        IF EV_DoPlat( line, raiseAndChange, 32 ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 15
        IF EV_DoPlat( line, raiseAndChange, 24 ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 18
        IF EV_DoFloor( line, raiseFloorToNearest ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 20
        IF EV_DoPlat( line, raiseToNearestAndChange, 0 ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 21
        IF EV_DoPlat( line, downWaitUpStay, 0 ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 23
        IF EV_DoFloor( line, lowerFloorToLowest ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 29
        IF EV_DoDoor( line, vld_normal ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 41
        IF EV_DoCeiling( line, lowerToFloor ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 71
        IF EV_DoFloor( line, turboLower ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 49
        IF EV_DoCeiling( line, crushAndRaise ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 50
        IF EV_DoDoor( line, vld_close ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 51
        P_ChangeSwitchTexture( line, 0 )
        G_SecretExitLevel()
        EXIT
    CASE 55
        IF EV_DoFloor( line, raiseFloorCrush ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 101
        IF EV_DoFloor( line, raiseFloor ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 102
        IF EV_DoFloor( line, lowerFloor ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 103
        IF EV_DoDoor( line, vld_open ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 111
        IF EV_DoDoor( line, vld_blazeRaise ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 112
        IF EV_DoDoor( line, vld_blazeOpen ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 113
        IF EV_DoDoor( line, vld_blazeClose ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 122
        IF EV_DoPlat( line, blazeDWUS, 0 ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 127
        IF EV_BuildStairs( line, turbo16 ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 131
        IF EV_DoFloor( line, raiseFloorTurbo ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 133
    CASE 135
    CASE 137
        IF EV_DoLockedDoor( line, vld_blazeOpen, thing ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 140
        IF EV_DoFloor( line, raiseFloor512 ) != 0
            P_ChangeSwitchTexture( line, 0 )
        ENDIF
        EXIT
    CASE 42
        IF EV_DoDoor( line, vld_close ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 43
        IF EV_DoCeiling( line, lowerToFloor ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 45
        IF EV_DoFloor( line, lowerFloor ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 60
        IF EV_DoFloor( line, lowerFloorToLowest ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 61
        IF EV_DoDoor( line, vld_open ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 62
        IF EV_DoPlat( line, downWaitUpStay, 1 ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 63
        IF EV_DoDoor( line, vld_normal ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 64
        IF EV_DoFloor( line, raiseFloor ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 66
        IF EV_DoPlat( line, raiseAndChange, 24 ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 67
        IF EV_DoPlat( line, raiseAndChange, 32 ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 65
        IF EV_DoFloor( line, raiseFloorCrush ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 68
        IF EV_DoPlat( line, raiseToNearestAndChange, 0 ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 69
        IF EV_DoFloor( line, raiseFloorToNearest ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 70
        IF EV_DoFloor( line, turboLower ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 114
        IF EV_DoDoor( line, vld_blazeRaise ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 115
        IF EV_DoDoor( line, vld_blazeOpen ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 116
        IF EV_DoDoor( line, vld_blazeClose ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 123
        IF EV_DoPlat( line, blazeDWUS, 0 ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 132
        IF EV_DoFloor( line, raiseFloorTurbo ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 99
    CASE 134
    CASE 136
        IF EV_DoLockedDoor( line, vld_blazeOpen, thing ) != 0
            P_ChangeSwitchTexture( line, 1 )
        ENDIF
        EXIT
    CASE 138
        EV_LightTurnOn( line, 255 )
        P_ChangeSwitchTexture( line, 1 )
        EXIT
    CASE 139
        EV_LightTurnOn( line, 35 )
        P_ChangeSwitchTexture( line, 1 )
        EXIT
    ENDSWITCH
RETURN .T.

INIT PROCEDURE init_p_switch
    LOCAL i


    switchlist := Array( MAXSWITCHES * 2 )
    FOR i := 1 TO MAXSWITCHES * 2
        switchlist[ i ] := 0
    NEXT
    numswitches := 0
    alphSwitchList := { ;
        { "SW1BRCOM", "SW2BRCOM", 1 }, ;
        { "SW1BRN1", "SW2BRN1", 1 }, ;
        { "SW1BRN2", "SW2BRN2", 1 }, ;
        { "SW1BRNGN", "SW2BRNGN", 1 }, ;
        { "SW1BROWN", "SW2BROWN", 1 }, ;
        { "SW1COMM", "SW2COMM", 1 }, ;
        { "SW1COMP", "SW2COMP", 1 }, ;
        { "SW1DIRT", "SW2DIRT", 1 }, ;
        { "SW1EXIT", "SW2EXIT", 1 }, ;
        { "SW1GRAY", "SW2GRAY", 1 }, ;
        { "SW1GRAY1", "SW2GRAY1", 1 }, ;
        { "SW1METAL", "SW2METAL", 1 }, ;
        { "SW1PIPE", "SW2PIPE", 1 }, ;
        { "SW1SLAD", "SW2SLAD", 1 }, ;
        { "SW1STARG", "SW2STARG", 1 }, ;
        { "SW1STON1", "SW2STON1", 1 }, ;
        { "SW1STON2", "SW2STON2", 1 }, ;
        { "SW1STONE", "SW2STONE", 1 }, ;
        { "SW1STRTN", "SW2STRTN", 1 }, ;
        { "SW1BLUE", "SW2BLUE", 2 }, ;
        { "SW1CMT", "SW2CMT", 2 }, ;
        { "SW1GARG", "SW2GARG", 2 }, ;
        { "SW1GSTON", "SW2GSTON", 2 }, ;
        { "SW1HOT", "SW2HOT", 2 }, ;
        { "SW1LION", "SW2LION", 2 }, ;
        { "SW1SATYR", "SW2SATYR", 2 }, ;
        { "SW1SKIN", "SW2SKIN", 2 }, ;
        { "SW1VINE", "SW2VINE", 2 }, ;
        { "SW1WOOD", "SW2WOOD", 2 }, ;
        { "SW1PANEL", "SW2PANEL", 3 }, ;
        { "SW1ROCK", "SW2ROCK", 3 }, ;
        { "SW1MET2", "SW2MET2", 3 }, ;
        { "SW1WDMET", "SW2WDMET", 3 }, ;
        { "SW1BRIK", "SW2BRIK", 3 }, ;
        { "SW1MOD1", "SW2MOD1", 3 }, ;
        { "SW1ZIM", "SW2ZIM", 3 }, ;
        { "SW1STON6", "SW2STON6", 3 }, ;
        { "SW1TEK", "SW2TEK", 3 }, ;
        { "SW1MARB", "SW2MARB", 3 }, ;
        { "SW1SKULL", "SW2SKULL", 3 }, ;
        { "", "", 0 } }
RETURN
