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

STATIC plyr
STATIC st_firsttime
STATIC lu_palette
STATIC st_clock
STATIC st_msgcounter
STATIC st_chatstate
STATIC st_gamestate
STATIC st_statusbaron
STATIC st_chat
STATIC st_oldchat
STATIC st_cursoron
STATIC st_notdeathmatch
STATIC st_armson
STATIC st_fragson
STATIC sbar
STATIC tallnum
STATIC tallpercent
STATIC shortnum
STATIC keys
STATIC faces
STATIC faceback
STATIC armsbg
STATIC arms
STATIC w_ready
STATIC w_frags
STATIC w_health
STATIC w_armsbg
STATIC w_arms
STATIC w_faces
STATIC w_keyboxes
STATIC w_armor
STATIC w_ammo
STATIC w_maxammo
STATIC st_fragscount
STATIC st_oldhealth
STATIC oldweaponsowned
STATIC st_facecount
STATIC st_faceindex
STATIC keyboxes
STATIC st_randomnumber
STATIC st_palette
STATIC st_stopped
STATIC st_lastcalc
STATIC st_painoldhealth
STATIC st_lastattackdown
STATIC st_facepriority
STATIC cheat_mus
STATIC cheat_god
STATIC cheat_ammo
STATIC cheat_ammonokey
STATIC cheat_noclip
STATIC cheat_commercial_noclip
STATIC cheat_powerup
STATIC cheat_choppers
STATIC cheat_clev
STATIC cheat_mypos

#include "st_stuff.ch"
#include "st_lib.ch"
#include "sounds.ch"
#include "d_event.ch"
#include "doomdef.ch"
#include "doomstat.ch"
#include "d_player.ch"
#include "d_items.ch"
#include "dstrings.ch"
#include "deh_main.ch"
#include "deh_misc.ch"

MEMVAR deh_god_mode_health
MEMVAR deh_idfa_armor
MEMVAR deh_idfa_armor_class
MEMVAR deh_idkfa_armor
MEMVAR deh_idkfa_armor_class
#include "m_cheat.ch"
#include "z_zone.ch"
#include "i_video.ch"
#include "p_local.ch"
#include "p_mobj.ch"
#include "doomkeys.ch"


#define STARTREDPALS       1
#define STARTBONUSPALS     9
#define NUMREDPALS         8
#define NUMBONUSPALS       4
#define RADIATIONPAL       13

#define ST_FACEPROBABILITY 96
#define ST_TOGGLECHAT      KEY_ENTER
#define ST_X               0
#define ST_X2              104
#define ST_FX              143
#define ST_FY              169

#define ST_NUMPAINFACES      5
#define ST_NUMSTRAIGHTFACES  3
#define ST_NUMTURNFACES      2
#define ST_NUMSPECIALFACES   3
#define ST_FACESTRIDE        ( ST_NUMSTRAIGHTFACES + ST_NUMTURNFACES + ST_NUMSPECIALFACES )
#define ST_NUMEXTRAFACES     2
#define ST_NUMFACES          ( ST_FACESTRIDE * ST_NUMPAINFACES + ST_NUMEXTRAFACES )
#define ST_TURNOFFSET        ST_NUMSTRAIGHTFACES
#define ST_OUCHOFFSET        ( ST_TURNOFFSET + ST_NUMTURNFACES )
#define ST_EVILGRINOFFSET    ( ST_OUCHOFFSET + 1 )
#define ST_RAMPAGEOFFSET     ( ST_EVILGRINOFFSET + 1 )
#define ST_GODFACE           ( ST_NUMPAINFACES * ST_FACESTRIDE )
#define ST_DEADFACE          ( ST_GODFACE + 1 )

#define ST_FACESX             143
#define ST_FACESY             168
#define ST_EVILGRINCOUNT      ( 2 * TICRATE )
#define ST_STRAIGHTFACECOUNT  Int( TICRATE / 2 )
#define ST_TURNCOUNT          ( 1 * TICRATE )
#define ST_OUCHCOUNT          ( 1 * TICRATE )
#define ST_RAMPAGEDELAY       ( 2 * TICRATE )
#define ST_MUCHPAIN           20

#define ST_AMMOWIDTH 3
#define ST_AMMOX     44
#define ST_AMMOY     171

#define ST_HEALTHWIDTH 3
#define ST_HEALTHX     90
#define ST_HEALTHY     171

#define ST_ARMSX       111
#define ST_ARMSY       172
#define ST_ARMSBGX     104
#define ST_ARMSBGY     168
#define ST_ARMSXSPACE  12
#define ST_ARMSYSPACE  10

#define ST_FRAGSX      138
#define ST_FRAGSY      171
#define ST_FRAGSWIDTH  2

#define ST_ARMORWIDTH 3
#define ST_ARMORX     221
#define ST_ARMORY     171

#define ST_KEY0X 239
#define ST_KEY0Y 171
#define ST_KEY1X 239
#define ST_KEY1Y 181
#define ST_KEY2X 239
#define ST_KEY2Y 191

#define ST_AMMO0WIDTH 3
#define ST_AMMO0X     288
#define ST_AMMO0Y     173
#define ST_AMMO1WIDTH 3
#define ST_AMMO1X     288
#define ST_AMMO1Y     179
#define ST_AMMO2WIDTH 3
#define ST_AMMO2X     288
#define ST_AMMO2Y     191
#define ST_AMMO3WIDTH 3
#define ST_AMMO3X     288
#define ST_AMMO3Y     185

#define ST_MAXAMMO0WIDTH 3
#define ST_MAXAMMO0X     314
#define ST_MAXAMMO0Y     173
#define ST_MAXAMMO1WIDTH 3
#define ST_MAXAMMO1X     314
#define ST_MAXAMMO1Y     179
#define ST_MAXAMMO2WIDTH 3
#define ST_MAXAMMO2X     314
#define ST_MAXAMMO2Y     191
#define ST_MAXAMMO3WIDTH 3
#define ST_MAXAMMO3X     314
#define ST_MAXAMMO3Y     185

#define ST_MSGWIDTH 52

#ifndef AM_MSGHEADER
#define AM_MSGHEADER   0x616D0000
#define AM_MSGENTERED  0x616D6500
#define AM_MSGEXITED   0x616D7800
#endif



STATIC FUNCTION AsU32( n )
RETURN ( n & 0xFFFFFFFF )

STATIC FUNCTION ST_WeaponOwnedRef( nWeapon )
RETURN {|| iif( plyr:weaponowned[ nWeapon + 1 ], 1, 0 ) }

PROCEDURE ST_refreshBackground()
    MEMVAR dest_screen
    MEMVAR netgame
    MEMVAR st_backing_screen
    IF st_statusbaron
        V_UseBuffer( st_backing_screen )
        V_DrawPatch( ST_X, 0, sbar )
        IF netgame
            V_DrawPatch( ST_FX, 0, faceback )
        ENDIF
        st_backing_screen := dest_screen
        V_RestoreBuffer()
        V_CopyRect( ST_X, 0, st_backing_screen, ST_WIDTH, ST_HEIGHT, ST_X, ST_Y )
    ENDIF
RETURN

FUNCTION ST_Responder( ev )
    LOCAL i
    LOCAL buf
    LOCAL musnum
    LOCAL epsd
    LOCAL map
    LOCAL pl
    MEMVAR consoleplayer
    MEMVAR gamemode
    MEMVAR gameskill
    MEMVAR gameversion
    MEMVAR netgame
    MEMVAR players

    IF ev:type == ev_keyup .AND. ( ev:data1 & 0xFFFF0000 ) == AM_MSGHEADER
        SWITCH ev:data1
        CASE AM_MSGENTERED
            st_gamestate := AutomapState
            st_firsttime := .T.
            EXIT
        CASE AM_MSGEXITED
            st_gamestate := FirstPersonState
            EXIT
        ENDSWITCH
    ELSEIF ev:type == ev_keydown
        IF ! netgame .AND. gameskill != sk_nightmare
            IF cht_CheckCheat( cheat_god, ev:data2 )
                plyr:cheats := ( plyr:cheats ^^ CF_GODMODE )
                IF ( plyr:cheats & CF_GODMODE ) != 0
                    IF !( plyr:mo == NIL )
                        plyr:mo:health := 100
                    ENDIF
                    plyr:health := deh_god_mode_health
                    plyr:message := DEH_String( STSTR_DQDON )
                ELSE
                    plyr:message := DEH_String( STSTR_DQDOFF )
                ENDIF

            ELSEIF cht_CheckCheat( cheat_ammonokey, ev:data2 )
                plyr:armorpoints := deh_idfa_armor
                plyr:armortype := deh_idfa_armor_class
                FOR i := 0 TO NUMWEAPONS - 1
                    plyr:weaponowned[ i + 1 ] := .T.
                NEXT
                FOR i := 0 TO NUMAMMO - 1
                    plyr:ammo[ i + 1 ] := plyr:maxammo[ i + 1 ]
                NEXT
                plyr:message := DEH_String( STSTR_FAADDED )

            ELSEIF cht_CheckCheat( cheat_ammo, ev:data2 )
                plyr:armorpoints := deh_idkfa_armor
                plyr:armortype := deh_idkfa_armor_class
                FOR i := 0 TO NUMWEAPONS - 1
                    plyr:weaponowned[ i + 1 ] := .T.
                NEXT
                FOR i := 0 TO NUMAMMO - 1
                    plyr:ammo[ i + 1 ] := plyr:maxammo[ i + 1 ]
                NEXT
                FOR i := 0 TO NUMCARDS - 1
                    plyr:cards[ i + 1 ] := .T.
                NEXT
                plyr:message := DEH_String( STSTR_KFAADDED )

            ELSEIF cht_CheckCheat( cheat_mus, ev:data2 )
                plyr:message := DEH_String( STSTR_MUS )
                buf := cht_GetParam( cheat_mus, "" )
                IF gamemode == commercial .OR. gameversion < exe_ultimate
                    musnum := mus_runnin + ( Asc( SubStr( buf, 1, 1 ) ) - Asc( "0" ) ) * 10 ;
                              + Asc( SubStr( buf, 2, 1 ) ) - Asc( "0" ) - 1
                    IF ( Asc( SubStr( buf, 1, 1 ) ) - Asc( "0" ) ) * 10 ;
                         + Asc( SubStr( buf, 2, 1 ) ) - Asc( "0" ) > 35
                        plyr:message := DEH_String( STSTR_NOMUS )
                    ELSE
                        S_ChangeMusic( musnum, 1 )
                    ENDIF
                ELSE
                    musnum := mus_e1m1 + ( Asc( SubStr( buf, 1, 1 ) ) - Asc( "1" ) ) * 9 ;
                              + Asc( SubStr( buf, 2, 1 ) ) - Asc( "1" )
                    IF ( Asc( SubStr( buf, 1, 1 ) ) - Asc( "1" ) ) * 9 ;
                         + Asc( SubStr( buf, 2, 1 ) ) - Asc( "1" ) > 31
                        plyr:message := DEH_String( STSTR_NOMUS )
                    ELSE
                        S_ChangeMusic( musnum, 1 )
                    ENDIF
                ENDIF

            ELSEIF ( logical_gamemission == doom .AND. cht_CheckCheat( cheat_noclip, ev:data2 ) ) ;
                .OR. ( logical_gamemission != doom .AND. cht_CheckCheat( cheat_commercial_noclip, ev:data2 ) )
                plyr:cheats := ( plyr:cheats ^^ CF_NOCLIP )
                IF ( plyr:cheats & CF_NOCLIP ) != 0
                    plyr:message := DEH_String( STSTR_NCON )
                ELSE
                    plyr:message := DEH_String( STSTR_NCOFF )
                ENDIF
            ENDIF

            FOR i := 0 TO 5
                IF cht_CheckCheat( cheat_powerup[ i + 1 ], ev:data2 )
                    IF plyr:powers[ i + 1 ] == 0
                        P_GivePower( plyr, i )
                    ELSEIF i != pw_strength
                        plyr:powers[ i + 1 ] := 1
                    ELSE
                        plyr:powers[ i + 1 ] := 0
                    ENDIF
                    plyr:message := DEH_String( STSTR_BEHOLDX )
                ENDIF
            NEXT

            IF cht_CheckCheat( cheat_powerup[ 7 ], ev:data2 )
                plyr:message := DEH_String( STSTR_BEHOLD )
            ELSEIF cht_CheckCheat( cheat_choppers, ev:data2 )
                plyr:weaponowned[ wp_chainsaw + 1 ] := .T.
                plyr:powers[ pw_invulnerability + 1 ] := .T.
                plyr:message := DEH_String( STSTR_CHOPPERS )
            ELSEIF cht_CheckCheat( cheat_mypos, ev:data2 )
                pl := players[ consoleplayer + 1 ]
                plyr:message := "ang=0x" + Lower( hb_NumToHex( pl:mo:angle ) ) ;
                    + ";x,y=(0x" + Lower( hb_NumToHex( pl:mo:x ) ) ;
                    + ",0x" + Lower( hb_NumToHex( pl:mo:y ) ) + ")"
            ENDIF
        ENDIF

        IF ! netgame .AND. cht_CheckCheat( cheat_clev, ev:data2 )
            buf := cht_GetParam( cheat_clev, "" )
            IF gamemode == commercial
                epsd := 1
                map := ( Asc( SubStr( buf, 1, 1 ) ) - Asc( "0" ) ) * 10 ;
                       + Asc( SubStr( buf, 2, 1 ) ) - Asc( "0" )
            ELSE
                epsd := Asc( SubStr( buf, 1, 1 ) ) - Asc( "0" )
                map := Asc( SubStr( buf, 2, 1 ) ) - Asc( "0" )
            ENDIF
            IF gameversion == exe_chex
                epsd := 1
            ENDIF
            IF epsd < 1 .OR. map < 1
                RETURN .F.
            ENDIF
            IF gamemode == retail .AND. ( epsd > 4 .OR. map > 9 )
                RETURN .F.
            ENDIF
            IF gamemode == registered .AND. ( epsd > 3 .OR. map > 9 )
                RETURN .F.
            ENDIF
            IF gamemode == shareware .AND. ( epsd > 1 .OR. map > 9 )
                RETURN .F.
            ENDIF
            IF gamemode == commercial .AND. ( epsd > 1 .OR. map > 40 )
                RETURN .F.
            ENDIF
            plyr:message := DEH_String( STSTR_CLEV )
            G_DeferedInitNew( gameskill, epsd, map )
        ENDIF
    ENDIF
RETURN .F.

FUNCTION ST_calcPainOffset()
    LOCAL health := iif( plyr:health > 100, 100, plyr:health )

    IF health != st_painoldhealth
        st_lastcalc := ST_FACESTRIDE * Int( ( ( 100 - health ) * ST_NUMPAINFACES ) / 101 )
        st_painoldhealth := health
    ENDIF
RETURN st_lastcalc

PROCEDURE ST_updateFaceWidget()
    LOCAL i
    LOCAL badguyangle
    LOCAL diffang
    LOCAL doevilgrin

    IF st_facepriority < 10 .AND. plyr:health == 0
        st_facepriority := 9
        st_faceindex := ST_DEADFACE
        st_facecount := 1
    ENDIF

    IF st_facepriority < 9 .AND. plyr:bonuscount != 0
        doevilgrin := .F.
        FOR i := 0 TO NUMWEAPONS - 1
            IF oldweaponsowned[ i + 1 ] != plyr:weaponowned[ i + 1 ]
                doevilgrin := .T.
                oldweaponsowned[ i + 1 ] := plyr:weaponowned[ i + 1 ]
            ENDIF
        NEXT
        IF doevilgrin
            st_facepriority := 8
            st_facecount := ST_EVILGRINCOUNT
            st_faceindex := ST_calcPainOffset() + ST_EVILGRINOFFSET
        ENDIF
    ENDIF

    IF st_facepriority < 8 .AND. plyr:damagecount != 0 .AND. !( plyr:attacker == NIL ) ;
         .AND. !( plyr:attacker == plyr:mo )
        st_facepriority := 7
        IF plyr:health - st_oldhealth > ST_MUCHPAIN
            st_facecount := ST_TURNCOUNT
            st_faceindex := ST_calcPainOffset() + ST_OUCHOFFSET
        ELSE
            badguyangle := R_PointToAngle2( plyr:mo:x, plyr:mo:y, ;
                                            plyr:attacker:x, plyr:attacker:y )
            IF badguyangle > plyr:mo:angle
                diffang := AsU32( badguyangle - plyr:mo:angle )
                i := diffang > ANG180
            ELSE
                diffang := AsU32( plyr:mo:angle - badguyangle )
                i := diffang <= ANG180
            ENDIF
            st_facecount := ST_TURNCOUNT
            st_faceindex := ST_calcPainOffset()
            IF diffang < ANG45
                st_faceindex += ST_RAMPAGEOFFSET
            ELSEIF i
                st_faceindex += ST_TURNOFFSET
            ELSE
                st_faceindex += ST_TURNOFFSET + 1
            ENDIF
        ENDIF
    ENDIF

    IF st_facepriority < 7 .AND. plyr:damagecount != 0
        IF plyr:health - st_oldhealth > ST_MUCHPAIN
            st_facepriority := 7
            st_facecount := ST_TURNCOUNT
            st_faceindex := ST_calcPainOffset() + ST_OUCHOFFSET
        ELSE
            st_facepriority := 6
            st_facecount := ST_TURNCOUNT
            st_faceindex := ST_calcPainOffset() + ST_RAMPAGEOFFSET
        ENDIF
    ENDIF

    IF st_facepriority < 6
        IF plyr:attackdown
            IF st_lastattackdown == -1
                st_lastattackdown := ST_RAMPAGEDELAY
            ELSE
                st_lastattackdown--
                IF st_lastattackdown == 0
                    st_facepriority := 5
                    st_faceindex := ST_calcPainOffset() + ST_RAMPAGEOFFSET
                    st_facecount := 1
                    st_lastattackdown := 1
                ENDIF
            ENDIF
        ELSE
            st_lastattackdown := -1
        ENDIF
    ENDIF

    IF st_facepriority < 5 .AND. ( ( plyr:cheats & CF_GODMODE ) != 0 ;
         .OR. plyr:powers[ pw_invulnerability + 1 ] != 0 )
        st_facepriority := 4
        st_faceindex := ST_GODFACE
        st_facecount := 1
    ENDIF

    IF st_facecount == 0
        st_faceindex := ST_calcPainOffset() + ( st_randomnumber % 3 )
        st_facecount := ST_STRAIGHTFACECOUNT
        st_facepriority := 0
    ENDIF
    st_facecount--
RETURN

PROCEDURE ST_updateWidgets()
    LOCAL i
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR weaponinfo

    IF weaponinfo[ plyr:readyweapon + 1 ]:ammo == am_noammo
        w_ready:num := {|| 1994 }
    ELSE
        w_ready:num := {|| plyr:ammo[ weaponinfo[ plyr:readyweapon + 1 ]:ammo + 1 ] }
    ENDIF
    w_ready:data := plyr:readyweapon

    FOR i := 0 TO 2
        keyboxes[ i + 1 ] := iif( plyr:cards[ i + 1 ], i, -1 )
        IF plyr:cards[ i + 3 + 1 ]
            keyboxes[ i + 1 ] := i + 3
        ENDIF
    NEXT

    ST_updateFaceWidget()
    st_notdeathmatch := ( deathmatch == 0 )
    st_armson := st_statusbaron .AND. deathmatch == 0
    st_fragson := deathmatch != 0 .AND. st_statusbaron
    st_fragscount := 0

    FOR i := 0 TO MAXPLAYERS - 1
        IF i != consoleplayer
            st_fragscount += plyr:frags[ i + 1 ]
        ELSE
            st_fragscount -= plyr:frags[ i + 1 ]
        ENDIF
    NEXT

    st_msgcounter--
    IF st_msgcounter == 0
        st_chat := st_oldchat
    ENDIF
RETURN

PROCEDURE ST_Ticker()
    st_clock++
    st_randomnumber := M_Random()
    ST_updateWidgets()
    st_oldhealth := plyr:health
RETURN

PROCEDURE ST_doPaletteStuff()
    LOCAL palette
    LOCAL pal
    LOCAL cnt
    LOCAL bzc
    MEMVAR gameversion

    cnt := plyr:damagecount
    IF plyr:powers[ pw_strength + 1 ] != 0
        bzc := 12 - Int( plyr:powers[ pw_strength + 1 ] / 64 )
        IF bzc > cnt
            cnt := bzc
        ENDIF
    ENDIF

    IF cnt != 0
        palette := Int( ( cnt + 7 ) / 8 )
        IF palette >= NUMREDPALS
            palette := NUMREDPALS - 1
        ENDIF
        palette += STARTREDPALS
    ELSEIF plyr:bonuscount != 0
        palette := Int( ( plyr:bonuscount + 7 ) / 8 )
        IF palette >= NUMBONUSPALS
            palette := NUMBONUSPALS - 1
        ENDIF
        palette += STARTBONUSPALS
    ELSEIF plyr:powers[ pw_ironfeet + 1 ] > 4 * 32 ;
         .OR. ( plyr:powers[ pw_ironfeet + 1 ] & 8 ) != 0
        palette := RADIATIONPAL
    ELSE
        palette := 0
    ENDIF

    IF gameversion == exe_chex .AND. palette >= STARTREDPALS ;
         .AND. palette < STARTREDPALS + NUMREDPALS
        palette := RADIATIONPAL
    ENDIF

    IF palette != st_palette
        st_palette := palette
        pal := W_CacheLumpNum( lu_palette, PU_CACHE )
        I_SetPalette( SubStr( pal, palette * 768 + 1, 768 ) )
    ENDIF
RETURN

PROCEDURE ST_drawWidgets( refresh )
    LOCAL i
    MEMVAR deathmatch

    st_armson := st_statusbaron .AND. deathmatch == 0
    st_fragson := deathmatch != 0 .AND. st_statusbaron
    STlib_updateNum( w_ready, refresh )
    FOR i := 0 TO 3
        STlib_updateNum( w_ammo[ i + 1 ], refresh )
        STlib_updateNum( w_maxammo[ i + 1 ], refresh )
    NEXT
    STlib_updatePercent( w_health, refresh )
    STlib_updatePercent( w_armor, refresh )
    STlib_updateBinIcon( w_armsbg, refresh )
    FOR i := 0 TO 5
        STlib_updateMultIcon( w_arms[ i + 1 ], refresh )
    NEXT
    STlib_updateMultIcon( w_faces, refresh )
    FOR i := 0 TO 2
        STlib_updateMultIcon( w_keyboxes[ i + 1 ], refresh )
    NEXT
    STlib_updateNum( w_frags, refresh )
RETURN

PROCEDURE ST_doRefresh()
    st_firsttime := .F.
    ST_refreshBackground()
    ST_drawWidgets( .T. )
RETURN

PROCEDURE ST_diffDraw()
    ST_drawWidgets( .F. )
RETURN

PROCEDURE ST_Drawer( fullscreen, refresh )
    MEMVAR automapactive
    st_statusbaron := ! fullscreen .OR. automapactive
    st_firsttime := st_firsttime .OR. refresh
    ST_doPaletteStuff()
    IF st_firsttime
        ST_doRefresh()
    ELSE
        ST_diffDraw()
    ENDIF
RETURN

STATIC PROCEDURE ST_loadUnloadGraphics( load )
    LOCAL i
    LOCAL j
    LOCAL facenum := 0
    LOCAL namebuf
    MEMVAR consoleplayer

    FOR i := 0 TO 9
        namebuf := "STTNUM" + hb_ntos( i )
        IF load
            tallnum[ i + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
        ELSE
            W_ReleaseLumpName( namebuf )
            tallnum[ i + 1 ] := NIL
        ENDIF
        namebuf := "STYSNUM" + hb_ntos( i )
        IF load
            shortnum[ i + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
        ELSE
            W_ReleaseLumpName( namebuf )
            shortnum[ i + 1 ] := NIL
        ENDIF
    NEXT

    namebuf := DEH_String( "STTPRCNT" )
    IF load
        tallpercent := W_CacheLumpName( namebuf, PU_STATIC )
    ELSE
        W_ReleaseLumpName( namebuf )
        tallpercent := NIL
    ENDIF

    FOR i := 0 TO NUMCARDS - 1
        namebuf := "STKEYS" + hb_ntos( i )
        IF load
            keys[ i + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
        ELSE
            W_ReleaseLumpName( namebuf )
            keys[ i + 1 ] := NIL
        ENDIF
    NEXT

    namebuf := DEH_String( "STARMS" )
    IF load
        armsbg := W_CacheLumpName( namebuf, PU_STATIC )
    ELSE
        W_ReleaseLumpName( namebuf )
        armsbg := NIL
    ENDIF

    FOR i := 0 TO 5
        namebuf := "STGNUM" + hb_ntos( i + 2 )
        IF load
            arms[ i + 1, 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
            arms[ i + 1, 2 ] := shortnum[ i + 2 + 1 ]
        ELSE
            W_ReleaseLumpName( namebuf )
            arms[ i + 1, 1 ] := NIL
            arms[ i + 1, 2 ] := NIL
        ENDIF
    NEXT

    namebuf := "STFB" + hb_ntos( consoleplayer )
    IF load
        faceback := W_CacheLumpName( namebuf, PU_STATIC )
    ELSE
        W_ReleaseLumpName( namebuf )
        faceback := NIL
    ENDIF

    namebuf := DEH_String( "STBAR" )
    IF load
        sbar := W_CacheLumpName( namebuf, PU_STATIC )
    ELSE
        W_ReleaseLumpName( namebuf )
        sbar := NIL
    ENDIF

    FOR i := 0 TO ST_NUMPAINFACES - 1
        FOR j := 0 TO ST_NUMSTRAIGHTFACES - 1
            namebuf := "STFST" + hb_ntos( i ) + hb_ntos( j )
            IF load
                faces[ facenum + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
            ELSE
                W_ReleaseLumpName( namebuf )
                faces[ facenum + 1 ] := NIL
            ENDIF
            facenum++
        NEXT
        namebuf := "STFTR" + hb_ntos( i ) + "0"
        IF load
            faces[ facenum + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
        ELSE
            W_ReleaseLumpName( namebuf )
            faces[ facenum + 1 ] := NIL
        ENDIF
        facenum++
        namebuf := "STFTL" + hb_ntos( i ) + "0"
        IF load
            faces[ facenum + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
        ELSE
            W_ReleaseLumpName( namebuf )
            faces[ facenum + 1 ] := NIL
        ENDIF
        facenum++
        namebuf := "STFOUCH" + hb_ntos( i )
        IF load
            faces[ facenum + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
        ELSE
            W_ReleaseLumpName( namebuf )
            faces[ facenum + 1 ] := NIL
        ENDIF
        facenum++
        namebuf := "STFEVL" + hb_ntos( i )
        IF load
            faces[ facenum + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
        ELSE
            W_ReleaseLumpName( namebuf )
            faces[ facenum + 1 ] := NIL
        ENDIF
        facenum++
        namebuf := "STFKILL" + hb_ntos( i )
        IF load
            faces[ facenum + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
        ELSE
            W_ReleaseLumpName( namebuf )
            faces[ facenum + 1 ] := NIL
        ENDIF
        facenum++
    NEXT

    namebuf := DEH_String( "STFGOD0" )
    IF load
        faces[ facenum + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
    ELSE
        W_ReleaseLumpName( namebuf )
        faces[ facenum + 1 ] := NIL
    ENDIF
    facenum++
    namebuf := DEH_String( "STFDEAD0" )
    IF load
        faces[ facenum + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
    ELSE
        W_ReleaseLumpName( namebuf )
        faces[ facenum + 1 ] := NIL
    ENDIF
RETURN

PROCEDURE ST_loadGraphics()
    ST_loadUnloadGraphics( .T. )
RETURN

PROCEDURE ST_loadData()
    lu_palette := W_GetNumForName( DEH_String( "PLAYPAL" ) )
    ST_loadGraphics()
RETURN

PROCEDURE ST_unloadGraphics()
    ST_loadUnloadGraphics( .F. )
RETURN

PROCEDURE ST_unloadData()
    ST_unloadGraphics()
RETURN

PROCEDURE ST_initData()
    LOCAL i
    MEMVAR consoleplayer
    MEMVAR players

    st_firsttime := .T.
    plyr := players[ consoleplayer + 1 ]
    st_clock := 0
    st_chatstate := StartChatState
    st_gamestate := FirstPersonState
    st_statusbaron := .T.
    st_oldchat := .F.
    st_chat := .F.
    st_cursoron := .F.
    st_faceindex := 0
    st_palette := -1
    st_oldhealth := -1
    FOR i := 0 TO NUMWEAPONS - 1
        oldweaponsowned[ i + 1 ] := plyr:weaponowned[ i + 1 ]
    NEXT
    AFill( keyboxes, -1 )
    STlib_init()
RETURN

PROCEDURE ST_createWidgets()
    LOCAL i
    MEMVAR weaponinfo

    STlib_initNum( w_ready, ST_AMMOX, ST_AMMOY, tallnum, ;
        {|| plyr:ammo[ weaponinfo[ plyr:readyweapon + 1 ]:ammo + 1 ] }, ;
        {|| st_statusbaron }, ST_AMMOWIDTH )
    w_ready:data := plyr:readyweapon

    STlib_initPercent( w_health, ST_HEALTHX, ST_HEALTHY, tallnum, ;
        {|| plyr:health }, {|| st_statusbaron }, tallpercent )
    STlib_initBinIcon( w_armsbg, ST_ARMSBGX, ST_ARMSBGY, armsbg, ;
        {|| st_notdeathmatch }, {|| st_statusbaron } )

    FOR i := 0 TO 5
        STlib_initMultIcon( w_arms[ i + 1 ], ;
            ST_ARMSX + ( i % 3 ) * ST_ARMSXSPACE, ;
            ST_ARMSY + Int( i / 3 ) * ST_ARMSYSPACE, ;
            { arms[ i + 1, 1 ], arms[ i + 1, 2 ] }, ;
            ST_WeaponOwnedRef( i + 1 ), ;
            {|| st_armson } )
    NEXT

    STlib_initNum( w_frags, ST_FRAGSX, ST_FRAGSY, tallnum, ;
        {|| st_fragscount }, {|| st_fragson }, ST_FRAGSWIDTH )
    STlib_initMultIcon( w_faces, ST_FACESX, ST_FACESY, faces, ;
        {|| st_faceindex }, {|| st_statusbaron } )
    STlib_initPercent( w_armor, ST_ARMORX, ST_ARMORY, tallnum, ;
        {|| plyr:armorpoints }, {|| st_statusbaron }, tallpercent )

    STlib_initMultIcon( w_keyboxes[ 1 ], ST_KEY0X, ST_KEY0Y, keys, ;
        {|| keyboxes[ 1 ] }, {|| st_statusbaron } )
    STlib_initMultIcon( w_keyboxes[ 2 ], ST_KEY1X, ST_KEY1Y, keys, ;
        {|| keyboxes[ 2 ] }, {|| st_statusbaron } )
    STlib_initMultIcon( w_keyboxes[ 3 ], ST_KEY2X, ST_KEY2Y, keys, ;
        {|| keyboxes[ 3 ] }, {|| st_statusbaron } )

    STlib_initNum( w_ammo[ 1 ], ST_AMMO0X, ST_AMMO0Y, shortnum, ;
        {|| plyr:ammo[ 1 ] }, {|| st_statusbaron }, ST_AMMO0WIDTH )
    STlib_initNum( w_ammo[ 2 ], ST_AMMO1X, ST_AMMO1Y, shortnum, ;
        {|| plyr:ammo[ 2 ] }, {|| st_statusbaron }, ST_AMMO1WIDTH )
    STlib_initNum( w_ammo[ 3 ], ST_AMMO2X, ST_AMMO2Y, shortnum, ;
        {|| plyr:ammo[ 3 ] }, {|| st_statusbaron }, ST_AMMO2WIDTH )
    STlib_initNum( w_ammo[ 4 ], ST_AMMO3X, ST_AMMO3Y, shortnum, ;
        {|| plyr:ammo[ 4 ] }, {|| st_statusbaron }, ST_AMMO3WIDTH )

    STlib_initNum( w_maxammo[ 1 ], ST_MAXAMMO0X, ST_MAXAMMO0Y, shortnum, ;
        {|| plyr:maxammo[ 1 ] }, {|| st_statusbaron }, ST_MAXAMMO0WIDTH )
    STlib_initNum( w_maxammo[ 2 ], ST_MAXAMMO1X, ST_MAXAMMO1Y, shortnum, ;
        {|| plyr:maxammo[ 2 ] }, {|| st_statusbaron }, ST_MAXAMMO1WIDTH )
    STlib_initNum( w_maxammo[ 3 ], ST_MAXAMMO2X, ST_MAXAMMO2Y, shortnum, ;
        {|| plyr:maxammo[ 3 ] }, {|| st_statusbaron }, ST_MAXAMMO2WIDTH )
    STlib_initNum( w_maxammo[ 4 ], ST_MAXAMMO3X, ST_MAXAMMO3Y, shortnum, ;
        {|| plyr:maxammo[ 4 ] }, {|| st_statusbaron }, ST_MAXAMMO3WIDTH )
RETURN

PROCEDURE ST_Start()
    IF ! st_stopped
        ST_Stop()
    ENDIF
    ST_initData()
    ST_createWidgets()
    st_stopped := .F.
RETURN

PROCEDURE ST_Stop()
    IF st_stopped
        RETURN
    ENDIF
    I_SetPalette( W_CacheLumpNum( lu_palette, PU_CACHE ) )
    st_stopped := .T.
RETURN

PROCEDURE ST_Init()
    MEMVAR st_backing_screen
    ST_loadData()
    st_backing_screen := Replicate( Chr( 0 ), ST_WIDTH * ST_HEIGHT )
RETURN

INIT PROCEDURE init_st_stuff
    LOCAL i

    PUBLIC st_backing_screen

    st_backing_screen := ""
    plyr := NIL
    st_firsttime := .T.
    lu_palette := 0
    st_clock := 0
    st_msgcounter := 0
    st_chatstate := StartChatState
    st_gamestate := FirstPersonState
    st_statusbaron := .T.
    st_chat := .F.
    st_oldchat := .F.
    st_cursoron := .F.
    st_notdeathmatch := .T.
    st_armson := .T.
    st_fragson := .F.
    sbar := NIL
    tallnum := Array( 10 )
    tallpercent := NIL
    shortnum := Array( 10 )
    keys := Array( NUMCARDS )
    faces := Array( ST_NUMFACES )
    faceback := NIL
    armsbg := NIL
    arms := Array( 6, 2 )
    w_ready := st_number_t():New()
    w_frags := st_number_t():New()
    w_health := st_percent_t():New()
    w_armsbg := st_binicon_t():New()
    w_arms := Array( 6 )
    w_faces := st_multicon_t():New()
    w_keyboxes := Array( 3 )
    w_armor := st_percent_t():New()
    w_ammo := Array( 4 )
    w_maxammo := Array( 4 )
    FOR i := 1 TO 6
        w_arms[ i ] := st_multicon_t():New()
    NEXT
    FOR i := 1 TO 3
        w_keyboxes[ i ] := st_multicon_t():New()
    NEXT
    FOR i := 1 TO 4
        w_ammo[ i ] := st_number_t():New()
        w_maxammo[ i ] := st_number_t():New()
    NEXT
    st_fragscount := 0
    st_oldhealth := -1
    oldweaponsowned := Array( NUMWEAPONS )
    AFill( oldweaponsowned, .F. )
    st_facecount := 0
    st_faceindex := 0
    keyboxes := Array( 3 )
    AFill( keyboxes, -1 )
    st_randomnumber := 0
    st_palette := 0
    st_stopped := .T.
    st_lastcalc := 0
    st_painoldhealth := -1
    st_lastattackdown := -1
    st_facepriority := 0

    cheat_mus := CHEAT( "idmus", 2 )
    cheat_god := CHEAT( "iddqd", 0 )
    cheat_ammo := CHEAT( "idkfa", 0 )
    cheat_ammonokey := CHEAT( "idfa", 0 )
    cheat_noclip := CHEAT( "idspispopd", 0 )
    cheat_commercial_noclip := CHEAT( "idclip", 0 )
    cheat_powerup := { CHEAT( "idbeholdv", 0 ), ;
                       CHEAT( "idbeholds", 0 ), ;
                       CHEAT( "idbeholdi", 0 ), ;
                       CHEAT( "idbeholdr", 0 ), ;
                       CHEAT( "idbeholda", 0 ), ;
                       CHEAT( "idbeholdl", 0 ), ;
                       CHEAT( "idbehold", 0 ) }
    cheat_choppers := CHEAT( "idchoppers", 0 )
    cheat_clev := CHEAT( "idclev", 2 )
    cheat_mypos := CHEAT( "idmypos", 0 )
RETURN

FUNCTION DEH_FindCheat( cName )
    cName := Lower( AllTrim( cName ) )
    DO CASE
    CASE cName == "change music"
        RETURN cheat_mus
    CASE cName == "chainsaw"
        RETURN cheat_choppers
    CASE cName == "god mode"
        RETURN cheat_god
    CASE cName == "ammo & keys"
        RETURN cheat_ammo
    CASE cName == "ammo"
        RETURN cheat_ammonokey
    CASE cName == "no clipping 1"
        RETURN cheat_noclip
    CASE cName == "no clipping 2"
        RETURN cheat_commercial_noclip
    CASE cName == "invincibility"
        RETURN cheat_powerup[ 1 ]
    CASE cName == "berserk"
        RETURN cheat_powerup[ 2 ]
    CASE cName == "invisibility"
        RETURN cheat_powerup[ 3 ]
    CASE cName == "radiation suit"
        RETURN cheat_powerup[ 4 ]
    CASE cName == "auto-map"
        RETURN cheat_powerup[ 5 ]
    CASE cName == "lite-amp goggles"
        RETURN cheat_powerup[ 6 ]
    CASE cName == "behold menu"
        RETURN cheat_powerup[ 7 ]
    CASE cName == "level warp"
        RETURN cheat_clev
    CASE cName == "player position"
        RETURN cheat_mypos
    CASE cName == "map cheat"
        RETURN AM_DehCheatAmap()
    ENDCASE
RETURN NIL
