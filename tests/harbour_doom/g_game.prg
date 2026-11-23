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
#include "fileio.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

STATIC oldgamestate
STATIC sendpause
STATIC sendsave
STATIC timingdemo
STATIC starttime
STATIC turbodetected := {}
STATIC demoname
STATIC longtics
STATIC netdemo
STATIC demobuffer := ""
STATIC demo_p := 1
STATIC demoend := 1
STATIC consistancy := {}
STATIC angleturn := {}
STATIC weapon_order_table := {}
STATIC next_weapon := 0
STATIC gamekeydown := {}
STATIC turnheld := 0
STATIC mousearray := {}
STATIC mousex := 0
STATIC mousey := 0
STATIC dclicktime := 0
STATIC dclickstate := .F.
STATIC dclicks := 0
STATIC dclicktime2 := 0
STATIC dclickstate2 := .F.
STATIC dclicks2 := 0
STATIC joyxmove := 0
STATIC joyymove := 0
STATIC joystrafemove := 0
STATIC joyarray := {}
STATIC savegameslot := 0
STATIC savedescription := ""
STATIC bodyque := {}
STATIC angle_carry := 0
STATIC secretexit := .F.
STATIC savename := ""
STATIC d_skill := 0
STATIC d_episode := 0
STATIC d_map := 0
STATIC defdemoname := NIL
STATIC pars := {}
STATIC cpars := {}
STATIC levelstarttic

#include "deh_main.ch"
#include "deh_misc.ch"
#include "d_englsh.ch"

MEMVAR deh_initial_health
MEMVAR deh_initial_bullets
#include "doomstat.ch"
#include "doomkeys.ch"
#include "g_game.ch"

#ifndef BACKUPTICS
#define BACKUPTICS 128
#endif

#ifndef TICRATE
#define TICRATE 35
#endif

#ifndef FRACUNIT
#define FRACUNIT 65536
#define FRACBITS 16
#endif

#ifndef PU_STATIC
#define PU_STATIC 1
#endif

#ifndef SKYFLATNAME
#define SKYFLATNAME "F_SKY1"
#endif

#ifndef MF_SHADOW
#define MF_SHADOW 262144
#endif

#ifndef ANG45
#define ANG45 536870912
#define ANGLETOFINESHIFT 19
#endif

#ifndef MAX_MOUSE_BUTTONS
#define MAX_MOUSE_BUTTONS 8
#endif

#ifndef S_SARG_RUN1
#define S_SARG_RUN1  477
#define S_SARG_PAIN2 489
#endif

#ifndef MT_BRUISERSHOT
#define MT_BRUISERSHOT 16
#define MT_TROOPSHOT   31
#define MT_HEADSHOT    32
#define MT_TFOG        39
#endif

#ifndef sfx_telept
#define sfx_telept 35
#endif

#define SAVEGAMESIZE    180224
#define TURBOTHRESHOLD  50
#define SLOWTURNTICS    6
#define NUMKEYS         256
#define MAX_JOY_BUTTONS 20
#define BODYQUESIZE     32
#define DEMOMARKER      128



INIT PROCEDURE init_g_game
    LOCAL i
    LOCAL j
    LOCAL aRow

    PUBLIC gameaction
    PUBLIC gamestate
    PUBLIC gameskill
    PUBLIC respawnmonsters
    PUBLIC gameepisode
    PUBLIC gamemap
    PUBLIC timelimit
    PUBLIC paused
    PUBLIC usergame
    PUBLIC nodrawers
    PUBLIC viewactive
    PUBLIC deathmatch
    PUBLIC netgame
    PUBLIC playeringame
    PUBLIC players
    PUBLIC consoleplayer
    PUBLIC displayplayer
    PUBLIC totalkills
    PUBLIC totalitems
    PUBLIC totalsecret
    PUBLIC demorecording
    PUBLIC demoplayback
    PUBLIC lowres_turn
    PUBLIC singledemo
    PUBLIC precache
    PUBLIC testcontrols
    PUBLIC testcontrols_mousespeed
    PUBLIC wminfo
    PUBLIC bodyqueslot
    PUBLIC vanilla_savegame_limit
    PUBLIC vanilla_demo_limit
    PUBLIC forwardmove
    PUBLIC sidemove
    PUBLIC save_stream
    PUBLIC savegame_error

    gameaction := ga_nothing
    gamestate := GS_LEVEL
    oldgamestate := GS_LEVEL
    gameskill := sk_medium
    respawnmonsters := .F.
    gameepisode := 1
    gamemap := 1
    timelimit := 0
    paused := .F.
    sendpause := .F.
    sendsave := .F.
    usergame := .F.
    timingdemo := .F.
    nodrawers := .F.
    starttime := 0
    viewactive := .F.
    deathmatch := 0
    netgame := .F.
    consoleplayer := 0
    displayplayer := 0
    levelstarttic := 0
    totalkills := 0
    totalitems := 0
    totalsecret := 0
    demoname := NIL
    demorecording := .F.
    longtics := .F.
    lowres_turn := .F.
    demoplayback := .F.
    netdemo := .F.
    demobuffer := ""
    demo_p := 1
    demoend := 1
    singledemo := .F.
    precache := .T.
    testcontrols := .F.
    testcontrols_mousespeed := 0
    bodyqueslot := 0
    vanilla_savegame_limit := 1
    vanilla_demo_limit := 1
    save_stream := NIL
    savegame_error := .F.
    wminfo := wbstartstruct_t():New()

    playeringame := {}
    players := {}
    turbodetected := {}
    FOR i := 1 TO MAXPLAYERS
        AAdd( playeringame, .F. )
        AAdd( players, player_t():New() )
        AAdd( turbodetected, .F. )
    NEXT

    consistancy := {}
    FOR i := 1 TO MAXPLAYERS
        aRow := {}
        FOR j := 1 TO BACKUPTICS
            AAdd( aRow, 0 )
        NEXT
        AAdd( consistancy, aRow )
    NEXT

    forwardmove := { 0x19, 0x32 }
    sidemove := { 0x18, 0x28 }
    angleturn := { 640, 1280, 320 }

    weapon_order_table := {}
    AAdd( weapon_order_table, { wp_fist, wp_fist } )
    AAdd( weapon_order_table, { wp_chainsaw, wp_fist } )
    AAdd( weapon_order_table, { wp_pistol, wp_pistol } )
    AAdd( weapon_order_table, { wp_shotgun, wp_shotgun } )
    AAdd( weapon_order_table, { wp_supershotgun, wp_shotgun } )
    AAdd( weapon_order_table, { wp_chaingun, wp_chaingun } )
    AAdd( weapon_order_table, { wp_missile, wp_missile } )
    AAdd( weapon_order_table, { wp_plasma, wp_plasma } )
    AAdd( weapon_order_table, { wp_bfg, wp_bfg } )

    gamekeydown := {}
    FOR i := 1 TO NUMKEYS
        AAdd( gamekeydown, .F. )
    NEXT

    mousearray := {}
    FOR i := 1 TO MAX_MOUSE_BUTTONS + 1
        AAdd( mousearray, .F. )
    NEXT

    joyarray := {}
    FOR i := 1 TO MAX_JOY_BUTTONS + 1
        AAdd( joyarray, .F. )
    NEXT

    bodyque := {}
    FOR i := 1 TO BODYQUESIZE
        AAdd( bodyque, NIL )
    NEXT

    pars := {}
    AAdd( pars, { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } )
    AAdd( pars, { 0, 30, 75, 120, 90, 165, 180, 180, 30, 165 } )
    AAdd( pars, { 0, 90, 90, 90, 120, 90, 360, 240, 30, 170 } )
    AAdd( pars, { 0, 90, 45, 90, 150, 90, 90, 165, 30, 135 } )

    cpars := { ;
        30, 90, 120, 120, 90, 150, 120, 120, 270, 90, ;
        210, 150, 150, 150, 210, 150, 420, 150, 210, 150, ;
        240, 150, 180, 150, 150, 300, 330, 420, 300, 180, ;
        120, 30 }
RETURN

STATIC FUNCTION GameKey( nKey )
    IF ValType( nKey ) != "N" .OR. nKey < 0 .OR. nKey >= NUMKEYS
        RETURN .F.
    ENDIF
RETURN gamekeydown[ nKey + 1 ]

STATIC FUNCTION MouseBtn( nBtn )
    IF ValType( nBtn ) != "N" .OR. nBtn < 0
        RETURN mousearray[ 1 ]
    ENDIF
    IF nBtn + 2 > Len( mousearray )
        RETURN .F.
    ENDIF
RETURN mousearray[ nBtn + 2 ]

STATIC PROCEDURE SetMouseBtn( nBtn, lOn )
    IF ValType( nBtn ) == "N" .AND. nBtn >= 0 .AND. nBtn + 2 <= Len( mousearray )
        mousearray[ nBtn + 2 ] := lOn
    ENDIF
RETURN

STATIC FUNCTION JoyBtn( nBtn )
    IF ValType( nBtn ) != "N" .OR. nBtn < 0
        RETURN joyarray[ 1 ]
    ENDIF
    IF nBtn + 2 > Len( joyarray )
        RETURN .F.
    ENDIF
RETURN joyarray[ nBtn + 2 ]

STATIC PROCEDURE SetJoyBtn( nBtn, lOn )
    IF ValType( nBtn ) == "N" .AND. nBtn >= 0 .AND. nBtn + 2 <= Len( joyarray )
        joyarray[ nBtn + 2 ] := lOn
    ENDIF
RETURN

STATIC FUNCTION DemoByte()
    LOCAL n

    IF demo_p > Len( demobuffer )
        RETURN 0
    ENDIF
    n := Asc( SubStr( demobuffer, demo_p, 1 ) )
    demo_p := demo_p + 1
RETURN n

STATIC FUNCTION DemoSByte()
    LOCAL n := DemoByte()
    IF n >= 128
        n := n - 256
    ENDIF
RETURN n

STATIC PROCEDURE DemoPut( nVal )
    nVal := ( nVal & 0xFF )
    IF demo_p > Len( demobuffer )
        demobuffer := demobuffer + Chr( nVal )
    ELSE
        demobuffer := Stuff( demobuffer, demo_p, 1, Chr( nVal ) )
    ENDIF
    demo_p := demo_p + 1
RETURN

FUNCTION G_CmdChecksum( cmd )
RETURN cmd:forwardmove + cmd:sidemove + cmd:angleturn + cmd:chatchar + cmd:buttons

STATIC FUNCTION WeaponSelectable( weapon )
    LOCAL oPl
    MEMVAR consoleplayer
    MEMVAR gamemission
    MEMVAR gamemode
    MEMVAR players

    oPl := players[ consoleplayer + 1 ]
    IF weapon == wp_supershotgun .AND. LogicalGameMission() == doom
        RETURN .F.
    ENDIF
    IF ( weapon == wp_plasma .OR. weapon == wp_bfg ) ;
         .AND. gamemission == doom .AND. gamemode == shareware
        RETURN .F.
    ENDIF
    IF ! oPl:weaponowned[ weapon + 1 ]
        RETURN .F.
    ENDIF
    IF weapon == wp_fist .AND. oPl:weaponowned[ wp_chainsaw + 1 ] ;
         .AND. oPl:powers[ pw_strength + 1 ] == 0
        RETURN .F.
    ENDIF
RETURN .T.

STATIC FUNCTION G_NextWeapon( nDir )
    LOCAL weapon
    LOCAL start_i
    LOCAL i
    LOCAL nLen
    LOCAL oPl
    MEMVAR consoleplayer
    MEMVAR players

    oPl := players[ consoleplayer + 1 ]
    IF oPl:pendingweapon == wp_nochange
        weapon := oPl:readyweapon
    ELSE
        weapon := oPl:pendingweapon
    ENDIF

    nLen := Len( weapon_order_table )
    FOR i := 0 TO nLen - 1
        IF weapon_order_table[ i + 1 ][ 1 ] == weapon
            EXIT
        ENDIF
    NEXT

    start_i := i
    DO WHILE .T.
        i := i + nDir
        i := ( i + nLen ) % nLen
        IF i == start_i .OR. WeaponSelectable( weapon_order_table[ i + 1 ][ 1 ] )
            EXIT
        ENDIF
    ENDDO
RETURN weapon_order_table[ i + 1 ][ 2 ]

FUNCTION G_BuildTiccmd( cmd, maketic )
    LOCAL i
    LOCAL lStrafe
    LOCAL lBstrafe
    LOCAL nSpeed
    LOCAL nTSpeed
    LOCAL nForward
    LOCAL nSide
    LOCAL nKey
    LOCAL aWeapKeys
    LOCAL nDesired
    MEMVAR consoleplayer
    MEMVAR dclick_use
    MEMVAR forwardmove
    MEMVAR gamestate
    MEMVAR joybfire
    MEMVAR joybspeed
    MEMVAR joybstrafe
    MEMVAR joybstrafeleft
    MEMVAR joybstraferight
    MEMVAR joybuse
    MEMVAR key_down
    MEMVAR key_left
    MEMVAR key_right
    MEMVAR key_speed
    MEMVAR key_strafe
    MEMVAR key_strafeleft
    MEMVAR key_straferight
    MEMVAR key_up
    MEMVAR key_weapon1
    MEMVAR key_weapon2
    MEMVAR key_weapon3
    MEMVAR key_weapon4
    MEMVAR key_weapon5
    MEMVAR key_weapon6
    MEMVAR key_weapon7
    MEMVAR key_weapon8
    MEMVAR lowres_turn
    MEMVAR mousebbackward
    MEMVAR mousebfire
    MEMVAR mousebforward
    MEMVAR mousebstrafe
    MEMVAR mousebstrafeleft
    MEMVAR mousebstraferight
    MEMVAR mousebuse
    MEMVAR sidemove
    MEMVAR testcontrols_mousespeed
    MEMVAR ticdup

    cmd:CopyFrom( ticcmd_t():New() )
    cmd:consistancy := consistancy[ consoleplayer + 1 ][ ( maketic % BACKUPTICS ) + 1 ]

    lStrafe := GameKey( key_strafe ) .OR. MouseBtn( mousebstrafe ) .OR. JoyBtn( joybstrafe )
    nSpeed := iif( key_speed >= NUMKEYS .OR. joybspeed >= MAX_JOY_BUTTONS ;
        .OR. GameKey( key_speed ) .OR. JoyBtn( joybspeed ), 1, 0 )

    nForward := 0
    nSide := 0

    IF joyxmove != 0 .OR. GameKey( key_right ) .OR. GameKey( key_left )
        turnheld := turnheld + ticdup
    ELSE
        turnheld := 0
    ENDIF

    nTSpeed := iif( turnheld < SLOWTURNTICS, 2, nSpeed )

    IF lStrafe
        IF GameKey( key_right )
            nSide := nSide + sidemove[ nSpeed + 1 ]
        ENDIF
        IF GameKey( key_left )
            nSide := nSide - sidemove[ nSpeed + 1 ]
        ENDIF
        IF joyxmove > 0
            nSide := nSide + sidemove[ nSpeed + 1 ]
        ENDIF
        IF joyxmove < 0
            nSide := nSide - sidemove[ nSpeed + 1 ]
        ENDIF
    ELSE
        IF GameKey( key_right )
            cmd:angleturn := cmd:angleturn - angleturn[ nTSpeed + 1 ]
        ENDIF
        IF GameKey( key_left )
            cmd:angleturn := cmd:angleturn + angleturn[ nTSpeed + 1 ]
        ENDIF
        IF joyxmove > 0
            cmd:angleturn := cmd:angleturn - angleturn[ nTSpeed + 1 ]
        ENDIF
        IF joyxmove < 0
            cmd:angleturn := cmd:angleturn + angleturn[ nTSpeed + 1 ]
        ENDIF
    ENDIF

    IF GameKey( key_up )
        nForward := nForward + forwardmove[ nSpeed + 1 ]
    ENDIF
    IF GameKey( key_down )
        nForward := nForward - forwardmove[ nSpeed + 1 ]
    ENDIF
    IF joyymove < 0
        nForward := nForward + forwardmove[ nSpeed + 1 ]
    ENDIF
    IF joyymove > 0
        nForward := nForward - forwardmove[ nSpeed + 1 ]
    ENDIF

    IF GameKey( key_strafeleft ) .OR. JoyBtn( joybstrafeleft ) ;
         .OR. MouseBtn( mousebstrafeleft ) .OR. joystrafemove < 0
        nSide := nSide - sidemove[ nSpeed + 1 ]
    ENDIF
    IF GameKey( key_straferight ) .OR. JoyBtn( joybstraferight ) ;
         .OR. MouseBtn( mousebstraferight ) .OR. joystrafemove > 0
        nSide := nSide + sidemove[ nSpeed + 1 ]
    ENDIF

    cmd:chatchar := HU_dequeueChatChar()

    IF GameKey( key_fire ) .OR. MouseBtn( mousebfire ) .OR. JoyBtn( joybfire )
        cmd:buttons := ( cmd:buttons | BT_ATTACK )
    ENDIF

    IF GameKey( key_use ) .OR. JoyBtn( joybuse ) .OR. MouseBtn( mousebuse )
        cmd:buttons := ( cmd:buttons | BT_USE )
        dclicks := 0
    ENDIF

    IF gamestate == GS_LEVEL .AND. next_weapon != 0
        i := G_NextWeapon( next_weapon )
        cmd:buttons := ( cmd:buttons | BT_CHANGE )
        cmd:buttons := ( cmd:buttons | ( i * 8 ) )
    ELSE
        aWeapKeys := { key_weapon1, key_weapon2, key_weapon3, key_weapon4, ;
            key_weapon5, key_weapon6, key_weapon7, key_weapon8 }
        FOR i := 0 TO Len( aWeapKeys ) - 1
            nKey := aWeapKeys[ i + 1 ]
            IF GameKey( nKey )
                cmd:buttons := ( cmd:buttons | BT_CHANGE )
                cmd:buttons := ( cmd:buttons | ( i * 8 ) )
                EXIT
            ENDIF
        NEXT
    ENDIF

    next_weapon := 0

    IF MouseBtn( mousebforward )
        nForward := nForward + forwardmove[ nSpeed + 1 ]
    ENDIF
    IF MouseBtn( mousebbackward )
        nForward := nForward - forwardmove[ nSpeed + 1 ]
    ENDIF

    IF dclick_use != 0
        IF MouseBtn( mousebforward ) != dclickstate .AND. dclicktime > 1
            dclickstate := MouseBtn( mousebforward )
            IF dclickstate
                dclicks := dclicks + 1
            ENDIF
            IF dclicks == 2
                cmd:buttons := ( cmd:buttons | BT_USE )
                dclicks := 0
            ELSE
                dclicktime := 0
            ENDIF
        ELSE
            dclicktime := dclicktime + ticdup
            IF dclicktime > 20
                dclicks := 0
                dclickstate := .F.
            ENDIF
        ENDIF

        lBstrafe := MouseBtn( mousebstrafe ) .OR. JoyBtn( joybstrafe )
        IF lBstrafe != dclickstate2 .AND. dclicktime2 > 1
            dclickstate2 := lBstrafe
            IF dclickstate2
                dclicks2 := dclicks2 + 1
            ENDIF
            IF dclicks2 == 2
                cmd:buttons := ( cmd:buttons | BT_USE )
                dclicks2 := 0
            ELSE
                dclicktime2 := 0
            ENDIF
        ELSE
            dclicktime2 := dclicktime2 + ticdup
            IF dclicktime2 > 20
                dclicks2 := 0
                dclickstate2 := .F.
            ENDIF
        ENDIF
    ENDIF

    nForward := nForward + mousey
    IF lStrafe
        nSide := nSide + mousex * 2
    ELSE
        cmd:angleturn := cmd:angleturn - mousex * 8
    ENDIF

    IF mousex == 0
        testcontrols_mousespeed := 0
    ENDIF

    mousex := 0
    mousey := 0

    IF nForward > forwardmove[ 2 ]
        nForward := forwardmove[ 2 ]
    ELSEIF nForward < -forwardmove[ 2 ]
        nForward := -forwardmove[ 2 ]
    ENDIF
    IF nSide > forwardmove[ 2 ]
        nSide := forwardmove[ 2 ]
    ELSEIF nSide < -forwardmove[ 2 ]
        nSide := -forwardmove[ 2 ]
    ENDIF

    cmd:forwardmove := cmd:forwardmove + nForward
    cmd:sidemove := cmd:sidemove + nSide

    IF sendpause
        sendpause := .F.
        cmd:buttons := ( BT_SPECIAL | BTS_PAUSE )
    ENDIF

    IF sendsave
        sendsave := .F.
        cmd:buttons := ( BT_SPECIAL | BTS_SAVEGAME | ( savegameslot * 4 ) )
    ENDIF

    IF lowres_turn
        nDesired := cmd:angleturn + angle_carry
        cmd:angleturn := ( ( nDesired + 128 ) & 0xFF00 )
        angle_carry := nDesired - cmd:angleturn
    ENDIF
RETURN NIL

FUNCTION G_DoLoadLevel()
    LOCAL i
    LOCAL j
    LOCAL cSky
    MEMVAR consoleplayer
    MEMVAR displayplayer
    MEMVAR gameaction
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gamemode
    MEMVAR gameskill
    MEMVAR gamestate
    MEMVAR gametic
    MEMVAR gameversion
    MEMVAR paused
    MEMVAR playeringame
    MEMVAR players
    MEMVAR skyflatnum
    MEMVAR skytexture
    MEMVAR testcontrols
    MEMVAR wipegamestate

    skyflatnum := R_FlatNumForName( DEH_String( SKYFLATNAME ) )

    IF gamemode == commercial .AND. ( gameversion == exe_final2 .OR. gameversion == exe_chex )
        IF gamemap < 12
            cSky := "SKY1"
        ELSEIF gamemap < 21
            cSky := "SKY2"
        ELSE
            cSky := "SKY3"
        ENDIF
        skytexture := R_TextureNumForName( DEH_String( cSky ) )
    ENDIF

    levelstarttic := gametic
    IF wipegamestate == GS_LEVEL
        wipegamestate := -1
    ENDIF
    gamestate := GS_LEVEL

    FOR i := 0 TO MAXPLAYERS - 1
        turbodetected[ i + 1 ] := .F.
        IF playeringame[ i + 1 ] .AND. players[ i + 1 ]:playerstate == PST_DEAD
            players[ i + 1 ]:playerstate := PST_REBORN
        ENDIF
        FOR j := 1 TO MAXPLAYERS
            players[ i + 1 ]:frags[ j ] := 0
        NEXT
    NEXT

    P_SetupLevel( gameepisode, gamemap, 0, gameskill )
    displayplayer := consoleplayer
    gameaction := ga_nothing
    Z_CheckHeap()

    FOR i := 1 TO NUMKEYS
        gamekeydown[ i ] := .F.
    NEXT
    joyxmove := 0
    joyymove := 0
    joystrafemove := 0
    mousex := 0
    mousey := 0
    sendpause := .F.
    sendsave := .F.
    paused := .F.
    FOR i := 1 TO Len( mousearray )
        mousearray[ i ] := .F.
    NEXT
    FOR i := 1 TO Len( joyarray )
        joyarray[ i ] := .F.
    NEXT

    IF testcontrols
        players[ consoleplayer + 1 ]:message := "Press escape to quit."
    ENDIF
RETURN NIL

STATIC PROCEDURE SetJoyButtons( nMask )
    LOCAL i
    LOCAL lOn
    MEMVAR joybnextweapon
    MEMVAR joybprevweapon

    FOR i := 0 TO MAX_JOY_BUTTONS - 1
        lOn := ( hb_qbitAnd( nMask, Int( 2 ^ i ) ) != 0 )
        IF ! JoyBtn( i ) .AND. lOn
            IF i == joybprevweapon
                next_weapon := -1
            ELSEIF i == joybnextweapon
                next_weapon := 1
            ENDIF
        ENDIF
        SetJoyBtn( i, lOn )
    NEXT
RETURN

STATIC PROCEDURE SetMouseButtons( nMask )
    LOCAL i
    LOCAL lOn
    MEMVAR mousebnextweapon
    MEMVAR mousebprevweapon

    FOR i := 0 TO MAX_MOUSE_BUTTONS - 1
        lOn := ( hb_qbitAnd( nMask, Int( 2 ^ i ) ) != 0 )
        IF ! MouseBtn( i ) .AND. lOn
            IF i == mousebprevweapon
                next_weapon := -1
            ELSEIF i == mousebnextweapon
                next_weapon := 1
            ENDIF
        ENDIF
        SetMouseBtn( i, lOn )
    NEXT
RETURN

FUNCTION G_Responder( ev )
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR demoplayback
    MEMVAR displayplayer
    MEMVAR gameaction
    MEMVAR gamestate
    MEMVAR key_nextweapon
    MEMVAR key_prevweapon
    MEMVAR key_spy
    MEMVAR mouseSensitivity
    MEMVAR playeringame
    MEMVAR singledemo
    MEMVAR testcontrols
    MEMVAR testcontrols_mousespeed
    IF gamestate == GS_LEVEL .AND. ev:type == ev_keydown ;
         .AND. ev:data1 == key_spy .AND. ( singledemo .OR. deathmatch == 0 )
        DO WHILE .T.
            displayplayer := displayplayer + 1
            IF displayplayer == MAXPLAYERS
                displayplayer := 0
            ENDIF
            IF playeringame[ displayplayer + 1 ] .OR. displayplayer == consoleplayer
                EXIT
            ENDIF
        ENDDO
        RETURN .T.
    ENDIF

    IF gameaction == ga_nothing .AND. ! singledemo ;
         .AND. ( demoplayback .OR. gamestate == GS_DEMOSCREEN )
        IF ev:type == ev_keydown .OR. ( ev:type == ev_mouse .AND. ev:data1 != 0 ) ;
             .OR. ( ev:type == ev_joystick .AND. ev:data1 != 0 )
            M_StartControlPanel()
            RETURN .T.
        ENDIF
        RETURN .F.
    ENDIF

    IF gamestate == GS_LEVEL
        IF HU_Responder( ev )
            RETURN .T.
        ENDIF
        IF ST_Responder( ev )
            RETURN .T.
        ENDIF
        IF AM_Responder( ev )
            RETURN .T.
        ENDIF
    ENDIF

    IF gamestate == GS_FINALE
        IF F_Responder( ev )
            RETURN .T.
        ENDIF
    ENDIF

    IF testcontrols .AND. ev:type == ev_mouse
        testcontrols_mousespeed := Abs( ev:data2 )
    ENDIF

    IF ev:type == ev_keydown .AND. ev:data1 == key_prevweapon
        next_weapon := -1
    ELSEIF ev:type == ev_keydown .AND. ev:data1 == key_nextweapon
        next_weapon := 1
    ENDIF

    SWITCH ev:type
    CASE ev_keydown
        IF ev:data1 == key_pause
            sendpause := .T.
        ELSEIF ev:data1 < NUMKEYS
            gamekeydown[ ev:data1 + 1 ] := .T.
        ENDIF
        RETURN .T.
    CASE ev_keyup
        IF ev:data1 < NUMKEYS
            gamekeydown[ ev:data1 + 1 ] := .F.
        ENDIF
        RETURN .F.
    CASE ev_mouse
        SetMouseButtons( ev:data1 )
        mousex := Int( ev:data2 * ( mouseSensitivity + 5 ) / 10 )
        mousey := Int( ev:data3 * ( mouseSensitivity + 5 ) / 10 )
        RETURN .T.
    CASE ev_joystick
        SetJoyButtons( ev:data1 )
        joyxmove := ev:data2
        joyymove := ev:data3
        joystrafemove := ev:data4
        RETURN .T.
    ENDSWITCH
RETURN .F.

FUNCTION G_Ticker()
    LOCAL i
    LOCAL j
    LOCAL nBuf
    LOCAL oCmd
    LOCAL cTurbo
    LOCAL aNames
    MEMVAR consoleplayer
    MEMVAR demoplayback
    MEMVAR demorecording
    MEMVAR gameaction
    MEMVAR gamestate
    MEMVAR gametic
    MEMVAR netcmds
    MEMVAR netgame
    MEMVAR paused
    MEMVAR player_names
    MEMVAR playeringame
    MEMVAR players
    MEMVAR rndindex
    MEMVAR ticdup

    FOR i := 0 TO MAXPLAYERS - 1
        IF playeringame[ i + 1 ] .AND. players[ i + 1 ]:playerstate == PST_REBORN
            G_DoReborn( i )
        ENDIF
    NEXT

    DO WHILE gameaction != ga_nothing
        SWITCH gameaction
        CASE ga_loadlevel
            G_DoLoadLevel()
            EXIT
        CASE ga_newgame
            G_DoNewGame()
            EXIT
        CASE ga_loadgame
            G_DoLoadGame()
            EXIT
        CASE ga_savegame
            G_DoSaveGame()
            EXIT
        CASE ga_playdemo
            G_DoPlayDemo()
            EXIT
        CASE ga_completed
            G_DoCompleted()
            EXIT
        CASE ga_victory
            F_StartFinale()
            EXIT
        CASE ga_worlddone
            G_DoWorldDone()
            EXIT
        CASE ga_screenshot
            V_ScreenShot( "DOOM%02i.%s" )
            players[ consoleplayer + 1 ]:message := DEH_String( "screen shot" )
            gameaction := ga_nothing
            EXIT
        CASE ga_nothing
            EXIT
        ENDSWITCH
    ENDDO

    nBuf := ( Int( gametic / ticdup ) % BACKUPTICS )

    FOR i := 0 TO MAXPLAYERS - 1
        IF playeringame[ i + 1 ]
            oCmd := players[ i + 1 ]:cmd
            IF ValType( netcmds ) == "A" .AND. Len( netcmds ) >= i + 1
                oCmd:CopyFrom( netcmds[ i + 1 ] )
            ENDIF
            IF demoplayback
                G_ReadDemoTiccmd( oCmd )
            ENDIF
            IF demorecording
                G_WriteDemoTiccmd( oCmd )
            ENDIF

            IF oCmd:forwardmove > TURBOTHRESHOLD
                turbodetected[ i + 1 ] := .T.
            ENDIF

            IF ( ( gametic & 31 ) == 0 ) ;
                 .AND. ( ( Int( gametic / 32 ) % MAXPLAYERS ) == i ) ;
                 .AND. turbodetected[ i + 1 ]
                aNames := iif( ValType( player_names ) == "A", player_names, ;
                    { "Player 1", "Player 2", "Player 3", "Player 4" } )
                cTurbo := aNames[ i + 1 ] + " is turbo!"
                players[ consoleplayer + 1 ]:message := cTurbo
                turbodetected[ i + 1 ] := .F.
            ENDIF

            IF netgame .AND. ! netdemo .AND. ( gametic % ticdup ) == 0
                IF gametic > BACKUPTICS ;
                     .AND. consistancy[ i + 1 ][ nBuf + 1 ] != oCmd:consistancy
                    I_Error( "consistency failure (" + hb_ntos( oCmd:consistancy ) + ;
                        " should be " + hb_ntos( consistancy[ i + 1 ][ nBuf + 1 ] ) + ")" )
                ENDIF
                IF players[ i + 1 ]:mo != NIL
                    consistancy[ i + 1 ][ nBuf + 1 ] := players[ i + 1 ]:mo:x
                ELSE
                    consistancy[ i + 1 ][ nBuf + 1 ] := rndindex
                ENDIF
            ENDIF
        ENDIF
    NEXT

    FOR i := 0 TO MAXPLAYERS - 1
        IF playeringame[ i + 1 ]
            IF ( players[ i + 1 ]:cmd:buttons & BT_SPECIAL ) != 0
                SWITCH ( players[ i + 1 ]:cmd:buttons & BT_SPECIALMASK )
                CASE BTS_PAUSE
                    paused := ! paused
                    IF paused
                        S_PauseSound()
                    ELSE
                        S_ResumeSound()
                    ENDIF
                    EXIT
                CASE BTS_SAVEGAME
                    IF Empty( savedescription )
                        savedescription := "NET GAME"
                    ENDIF
                    savegameslot := Int( ( players[ i + 1 ]:cmd:buttons & BTS_SAVEMASK ) / 4 )
                    gameaction := ga_savegame
                    EXIT
                ENDSWITCH
            ENDIF
        ENDIF
    NEXT

    IF oldgamestate == GS_INTERMISSION .AND. gamestate != GS_INTERMISSION
        WI_End()
    ENDIF
    oldgamestate := gamestate

    SWITCH gamestate
    CASE GS_LEVEL
        P_Ticker()
        ST_Ticker()
        AM_Ticker()
        HU_Ticker()
        EXIT
    CASE GS_INTERMISSION
        WI_Ticker()
        EXIT
    CASE GS_FINALE
        F_Ticker()
        EXIT
    CASE GS_DEMOSCREEN
        D_PageTicker()
        EXIT
    ENDSWITCH
RETURN NIL

FUNCTION G_InitPlayer( player )
    G_PlayerReborn( player )
RETURN NIL

FUNCTION G_PlayerFinishLevel( player )
    LOCAL oP
    LOCAL i
    MEMVAR players

    oP := players[ player + 1 ]
    FOR i := 1 TO NUMPOWERS
        oP:powers[ i ] := 0
    NEXT
    FOR i := 1 TO NUMCARDS
        oP:cards[ i ] := .F.
    NEXT
    IF oP:mo != NIL
        oP:mo:flags := ( oP:mo:flags & hb_qbitNot( MF_SHADOW ) )
    ENDIF
    oP:extralight := 0
    oP:fixedcolormap := 0
    oP:damagecount := 0
    oP:bonuscount := 0
RETURN NIL

FUNCTION G_PlayerReborn( player )
    LOCAL oP
    LOCAL i
    LOCAL aFrags
    LOCAL nKill
    LOCAL nItem
    LOCAL nSecret
    MEMVAR maxammo
    MEMVAR players

    oP := players[ player + 1 ]
    aFrags := AClone( oP:frags )
    nKill := oP:killcount
    nItem := oP:itemcount
    nSecret := oP:secretcount

    oP:New()
    oP:frags := aFrags
    oP:killcount := nKill
    oP:itemcount := nItem
    oP:secretcount := nSecret
    oP:usedown := .T.
    oP:attackdown := .T.
    oP:playerstate := PST_LIVE
    oP:health := deh_initial_health
    oP:readyweapon := wp_pistol
    oP:pendingweapon := wp_pistol
    oP:weaponowned[ wp_fist + 1 ] := .T.
    oP:weaponowned[ wp_pistol + 1 ] := .T.
    oP:ammo[ am_clip + 1 ] := deh_initial_bullets

    IF ValType( maxammo ) == "A"
        FOR i := 0 TO NUMAMMO - 1
            oP:maxammo[ i + 1 ] := maxammo[ i + 1 ]
        NEXT
    ENDIF
RETURN NIL

FUNCTION G_CheckSpot( playernum, mthing )
    LOCAL x
    LOCAL y
    LOCAL ss
    LOCAL mo
    LOCAL i
    LOCAL nSlot
    LOCAL xa
    LOCAL ya
    LOCAL an
    MEMVAR bodyqueslot
    MEMVAR consoleplayer
    MEMVAR finecosine
    MEMVAR finesine
    MEMVAR finetangent
    MEMVAR players

    IF players[ playernum + 1 ]:mo == NIL
        FOR i := 0 TO playernum - 1
            IF players[ i + 1 ]:mo != NIL ;
                 .AND. players[ i + 1 ]:mo:x == mthing:x * FRACUNIT ;
                 .AND. players[ i + 1 ]:mo:y == mthing:y * FRACUNIT
                RETURN .F.
            ENDIF
        NEXT
        RETURN .T.
    ENDIF

    x := mthing:x * FRACUNIT
    y := mthing:y * FRACUNIT

    IF ! P_CheckPosition( players[ playernum + 1 ]:mo, x, y )
        RETURN .F.
    ENDIF

    IF bodyqueslot >= BODYQUESIZE
        nSlot := ( bodyqueslot % BODYQUESIZE ) + 1
        IF bodyque[ nSlot ] != NIL
            P_RemoveMobj( bodyque[ nSlot ] )
        ENDIF
    ENDIF
    bodyque[ ( bodyqueslot % BODYQUESIZE ) + 1 ] := players[ playernum + 1 ]:mo
    bodyqueslot := bodyqueslot + 1

    ss := R_PointInSubsector( x, y )
    an := 1024 * Int( mthing:angle / 45 )

    SWITCH an
    CASE 4096
        xa := finetangent[ 2048 + 1 ]
        ya := finetangent[ 0 + 1 ]
        EXIT
    CASE 5120
        xa := finetangent[ 3072 + 1 ]
        ya := finetangent[ 1024 + 1 ]
        EXIT
    CASE 6144
        xa := finesine[ 0 + 1 ]
        ya := finetangent[ 2048 + 1 ]
        EXIT
    CASE 7168
        xa := finesine[ 1024 + 1 ]
        ya := finetangent[ 3072 + 1 ]
        EXIT
    CASE 0
    CASE 1024
    CASE 2048
    CASE 3072
        xa := finecosine[ an + 1 ]
        ya := finesine[ an + 1 ]
        EXIT
    OTHERWISE
        I_Error( "G_CheckSpot: unexpected angle " + hb_ntos( an ) + hb_eol() )
        xa := 0
        ya := 0
    ENDSWITCH

    mo := P_SpawnMobj( x + 20 * xa, y + 20 * ya, ss:sector:floorheight, MT_TFOG )
    IF players[ consoleplayer + 1 ]:viewz != 1
        S_StartSound( mo, sfx_telept )
    ENDIF
RETURN .T.

FUNCTION G_DeathMatchSpawnPlayer( playernum )
    LOCAL i
    LOCAL j
    LOCAL nSel
    MEMVAR deathmatch_p
    MEMVAR deathmatchstarts
    MEMVAR playerstarts

    nSel := iif( ValType( deathmatch_p ) == "N", deathmatch_p, 0 )
    IF nSel < 4
        I_Error( "Only " + hb_ntos( nSel ) + " deathmatch spots, 4 required" )
    ENDIF

    FOR j := 0 TO 19
        i := P_Random() % nSel
        IF G_CheckSpot( playernum, deathmatchstarts[ i + 1 ] )
            deathmatchstarts[ i + 1 ]:type := playernum + 1
            P_SpawnPlayer( deathmatchstarts[ i + 1 ] )
            RETURN NIL
        ENDIF
    NEXT

    P_SpawnPlayer( playerstarts[ playernum + 1 ] )
RETURN NIL

FUNCTION G_DoReborn( playernum )
    LOCAL i
    MEMVAR deathmatch
    MEMVAR gameaction
    MEMVAR netgame
    MEMVAR players
    MEMVAR playerstarts

    IF ! netgame
        gameaction := ga_loadlevel
        RETURN NIL
    ENDIF

    IF players[ playernum + 1 ]:mo != NIL
        players[ playernum + 1 ]:mo:player := NIL
    ENDIF

    IF deathmatch != 0
        G_DeathMatchSpawnPlayer( playernum )
        RETURN NIL
    ENDIF

    IF G_CheckSpot( playernum, playerstarts[ playernum + 1 ] )
        P_SpawnPlayer( playerstarts[ playernum + 1 ] )
        RETURN NIL
    ENDIF

    FOR i := 0 TO MAXPLAYERS - 1
        IF G_CheckSpot( playernum, playerstarts[ i + 1 ] )
            playerstarts[ i + 1 ]:type := playernum + 1
            P_SpawnPlayer( playerstarts[ i + 1 ] )
            playerstarts[ i + 1 ]:type := i + 1
            RETURN NIL
        ENDIF
    NEXT

    P_SpawnPlayer( playerstarts[ playernum + 1 ] )
RETURN NIL

FUNCTION G_ScreenShot()
    MEMVAR gameaction
    gameaction := ga_screenshot
RETURN NIL

FUNCTION G_DrawMouseSpeedBox()
RETURN NIL

FUNCTION G_ExitLevel()
    MEMVAR gameaction
    secretexit := .F.
    gameaction := ga_completed
RETURN NIL

FUNCTION G_SecretExitLevel()
    MEMVAR gameaction
    MEMVAR gamemode
    IF gamemode == commercial .AND. W_CheckNumForName( "map31" ) < 0
        secretexit := .F.
    ELSE
        secretexit := .T.
    ENDIF
    gameaction := ga_completed
RETURN NIL

FUNCTION G_DoCompleted()
    LOCAL i
    LOCAL j
    MEMVAR automapactive
    MEMVAR consoleplayer
    MEMVAR gameaction
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gamemode
    MEMVAR gamestate
    MEMVAR gameversion
    MEMVAR leveltime
    MEMVAR playeringame
    MEMVAR players
    MEMVAR totalitems
    MEMVAR totalkills
    MEMVAR totalsecret
    MEMVAR viewactive
    MEMVAR wminfo

    gameaction := ga_nothing

    FOR i := 0 TO MAXPLAYERS - 1
        IF playeringame[ i + 1 ]
            G_PlayerFinishLevel( i )
        ENDIF
    NEXT

    IF automapactive
        AM_Stop()
    ENDIF

    IF gamemode != commercial
        IF gameversion == exe_chex
            IF gamemap == 5
                gameaction := ga_victory
                RETURN NIL
            ENDIF
        ELSE
            IF gamemap == 8
                gameaction := ga_victory
                RETURN NIL
            ELSEIF gamemap == 9
                FOR i := 0 TO MAXPLAYERS - 1
                    players[ i + 1 ]:didsecret := .T.
                NEXT
            ENDIF
        ENDIF
    ENDIF

    IF gamemap == 8 .AND. gamemode != commercial
        gameaction := ga_victory
        RETURN NIL
    ENDIF

    IF gamemap == 9 .AND. gamemode != commercial
        FOR i := 0 TO MAXPLAYERS - 1
            players[ i + 1 ]:didsecret := .T.
        NEXT
    ENDIF

    wminfo:didsecret := players[ consoleplayer + 1 ]:didsecret
    wminfo:epsd := gameepisode - 1
    wminfo:last := gamemap - 1

    IF gamemode == commercial
        IF secretexit
            SWITCH gamemap
            CASE 15
                wminfo:next := 30
                EXIT
            CASE 31
                wminfo:next := 31
                EXIT
            ENDSWITCH
        ELSE
            SWITCH gamemap
            CASE 31
            CASE 32
                wminfo:next := 15
                EXIT
            OTHERWISE
                wminfo:next := gamemap
            ENDSWITCH
        ENDIF
    ELSE
        IF secretexit
            wminfo:next := 8
        ELSEIF gamemap == 9
            SWITCH gameepisode
            CASE 1
                wminfo:next := 3
                EXIT
            CASE 2
                wminfo:next := 5
                EXIT
            CASE 3
                wminfo:next := 6
                EXIT
            CASE 4
                wminfo:next := 2
                EXIT
            ENDSWITCH
        ELSE
            wminfo:next := gamemap
        ENDIF
    ENDIF

    wminfo:maxkills := totalkills
    wminfo:maxitems := totalitems
    wminfo:maxsecret := totalsecret
    wminfo:maxfrags := 0

    IF gamemode == commercial
        wminfo:partime := TICRATE * cpars[ gamemap ]
    ELSEIF gameepisode < 4
        wminfo:partime := TICRATE * pars[ gameepisode + 1 ][ gamemap + 1 ]
    ELSE
        wminfo:partime := TICRATE * cpars[ gamemap + 1 ]
    ENDIF

    wminfo:pnum := consoleplayer

    FOR i := 0 TO MAXPLAYERS - 1
        wminfo:plyr[ i + 1 ]:in := playeringame[ i + 1 ]
        wminfo:plyr[ i + 1 ]:skills := players[ i + 1 ]:killcount
        wminfo:plyr[ i + 1 ]:sitems := players[ i + 1 ]:itemcount
        wminfo:plyr[ i + 1 ]:ssecret := players[ i + 1 ]:secretcount
        wminfo:plyr[ i + 1 ]:stime := leveltime
        FOR j := 1 TO MAXPLAYERS
            wminfo:plyr[ i + 1 ]:frags[ j ] := players[ i + 1 ]:frags[ j ]
        NEXT
    NEXT

    gamestate := GS_INTERMISSION
    viewactive := .F.
    automapactive := .F.

    StatCopy( wminfo )
    WI_Start( wminfo )
RETURN NIL

FUNCTION G_WorldDone()
    MEMVAR consoleplayer
    MEMVAR gameaction
    MEMVAR gamemap
    MEMVAR gamemode
    MEMVAR players
    gameaction := ga_worlddone

    IF secretexit
        players[ consoleplayer + 1 ]:didsecret := .T.
    ENDIF

    IF gamemode == commercial
        SWITCH gamemap
        CASE 15
        CASE 31
            IF ! secretexit
                EXIT
            ENDIF
            F_StartFinale()
            EXIT
        CASE 6
        CASE 11
        CASE 20
        CASE 30
            F_StartFinale()
            EXIT
        ENDSWITCH
    ENDIF
RETURN NIL

FUNCTION G_DoWorldDone()
    MEMVAR gameaction
    MEMVAR gamemap
    MEMVAR gamestate
    MEMVAR viewactive
    MEMVAR wminfo
    gamestate := GS_LEVEL
    gamemap := wminfo:next + 1
    G_DoLoadLevel()
    gameaction := ga_nothing
    viewactive := .T.
RETURN NIL

FUNCTION G_LoadGame( name )
    MEMVAR gameaction
    savename := name
    gameaction := ga_loadgame
RETURN NIL

FUNCTION G_DoLoadGame()
    LOCAL nSaved
    MEMVAR gameaction
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gameskill
    MEMVAR leveltime
    MEMVAR save_stream
    MEMVAR savegame_error
    MEMVAR setsizeneeded

    gameaction := ga_nothing
    save_stream := FOpen( savename, FO_READ )
    IF save_stream == F_ERROR .OR. save_stream == NIL
        RETURN NIL
    ENDIF

    savegame_error := .F.
    IF ! P_ReadSaveGameHeader()
        FClose( save_stream )
        RETURN NIL
    ENDIF

    nSaved := leveltime
    G_InitNew( gameskill, gameepisode, gamemap )
    leveltime := nSaved

    P_UnArchivePlayers()
    P_UnArchiveWorld()
    P_UnArchiveThinkers()
    P_UnArchiveSpecials()

    IF ! P_ReadSaveGameEOF()
        I_Error( "Bad savegame" )
    ENDIF

    FClose( save_stream )

    IF setsizeneeded
        R_ExecuteSetViewSize()
    ENDIF
    R_FillBackScreen()
RETURN NIL

FUNCTION G_SaveGame( slot, description )
    savegameslot := slot
    savedescription := description
    sendsave := .T.
RETURN NIL

FUNCTION G_DoSaveGame()
    LOCAL cSave
    LOCAL cTemp
    LOCAL cRec
    LOCAL nLen
    MEMVAR consoleplayer
    MEMVAR gameaction
    MEMVAR players
    MEMVAR save_stream
    MEMVAR savegame_error
    MEMVAR vanilla_savegame_limit

    cRec := NIL
    cTemp := P_TempSaveGameFile()
    cSave := P_SaveGameFile( savegameslot )

    save_stream := FCreate( cTemp )
    IF save_stream == F_ERROR .OR. save_stream == NIL
        cRec := M_TempFile( "recovery.dsg" )
        save_stream := FCreate( cRec )
        IF save_stream == F_ERROR .OR. save_stream == NIL
            I_Error( "Failed to open either '" + cTemp + "' or '" + cRec + "' to write savegame." )
        ENDIF
    ENDIF

    savegame_error := .F.
    P_WriteSaveGameHeader( savedescription )
    P_ArchivePlayers()
    P_ArchiveWorld()
    P_ArchiveThinkers()
    P_ArchiveSpecials()
    P_WriteSaveGameEOF()

    nLen := FSeek( save_stream, 0, FS_END )
    IF vanilla_savegame_limit != 0 .AND. nLen > SAVEGAMESIZE
        I_Error( "Savegame buffer overrun" )
    ENDIF

    FClose( save_stream )

    IF cRec != NIL
        I_Error( "Failed to open savegame file '" + cTemp + "' for writing." + hb_eol() + ;
            "But your game has been saved to '" + cRec + "' for recovery." )
    ENDIF

    FErase( cSave )
    FRename( cTemp, cSave )

    gameaction := ga_nothing
    savedescription := ""
    players[ consoleplayer + 1 ]:message := DEH_String( GGSAVED )
    R_FillBackScreen()
RETURN NIL

FUNCTION G_DeferedInitNew( skill, episode, map )
    MEMVAR gameaction
    d_skill := skill
    d_episode := episode
    d_map := map
    gameaction := ga_newgame
RETURN NIL

FUNCTION G_DoNewGame()
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR demoplayback
    MEMVAR fastparm
    MEMVAR gameaction
    MEMVAR netgame
    MEMVAR nomonsters
    MEMVAR playeringame
    MEMVAR respawnparm
    demoplayback := .F.
    netdemo := .F.
    netgame := .F.
    deathmatch := 0
    playeringame[ 2 ] := .F.
    playeringame[ 3 ] := .F.
    playeringame[ 4 ] := .F.
    respawnparm := .F.
    fastparm := .F.
    nomonsters := .F.
    consoleplayer := 0
    G_InitNew( d_skill, d_episode, d_map )
    gameaction := ga_nothing
RETURN NIL

FUNCTION G_InitNew( skill, episode, map )
    LOCAL cSky
    LOCAL i
    MEMVAR automapactive
    MEMVAR demoplayback
    MEMVAR fastparm
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gamemode
    MEMVAR gameskill
    MEMVAR gameversion
    MEMVAR mobjinfo
    MEMVAR paused
    MEMVAR players
    MEMVAR respawnmonsters
    MEMVAR respawnparm
    MEMVAR skytexture
    MEMVAR states
    MEMVAR usergame
    MEMVAR viewactive

    IF paused
        paused := .F.
        S_ResumeSound()
    ENDIF

    IF skill > sk_nightmare
        skill := sk_nightmare
    ENDIF

    IF gameversion >= exe_ultimate
        IF episode == 0
            episode := 4
        ENDIF
    ELSE
        IF episode < 1
            episode := 1
        ENDIF
        IF episode > 3
            episode := 3
        ENDIF
    ENDIF

    IF episode > 1 .AND. gamemode == shareware
        episode := 1
    ENDIF

    IF map < 1
        map := 1
    ENDIF
    IF map > 9 .AND. gamemode != commercial
        map := 9
    ENDIF

    M_ClearRandom()

    respawnmonsters := ( skill == sk_nightmare .OR. respawnparm )

    IF ValType( states ) == "A" .AND. ValType( mobjinfo ) == "A"
        IF fastparm .OR. ( skill == sk_nightmare .AND. gameskill != sk_nightmare )
            FOR i := S_SARG_RUN1 TO S_SARG_PAIN2
                states[ i + 1 ]:tics := Int( states[ i + 1 ]:tics / 2 )
            NEXT
            mobjinfo[ MT_BRUISERSHOT + 1 ]:speed := 20 * FRACUNIT
            mobjinfo[ MT_HEADSHOT + 1 ]:speed := 20 * FRACUNIT
            mobjinfo[ MT_TROOPSHOT + 1 ]:speed := 20 * FRACUNIT
        ELSEIF skill != sk_nightmare .AND. gameskill == sk_nightmare
            FOR i := S_SARG_RUN1 TO S_SARG_PAIN2
                states[ i + 1 ]:tics := states[ i + 1 ]:tics * 2
            NEXT
            mobjinfo[ MT_BRUISERSHOT + 1 ]:speed := 15 * FRACUNIT
            mobjinfo[ MT_HEADSHOT + 1 ]:speed := 10 * FRACUNIT
            mobjinfo[ MT_TROOPSHOT + 1 ]:speed := 10 * FRACUNIT
        ENDIF
    ENDIF

    FOR i := 0 TO MAXPLAYERS - 1
        players[ i + 1 ]:playerstate := PST_REBORN
    NEXT

    usergame := .T.
    paused := .F.
    demoplayback := .F.
    automapactive := .F.
    viewactive := .T.
    gameepisode := episode
    gamemap := map
    gameskill := skill
    viewactive := .T.

    IF gamemode == commercial
        IF gamemap < 12
            cSky := "SKY1"
        ELSEIF gamemap < 21
            cSky := "SKY2"
        ELSE
            cSky := "SKY3"
        ENDIF
    ELSE
        SWITCH gameepisode
        CASE 2
            cSky := "SKY2"
            EXIT
        CASE 3
            cSky := "SKY3"
            EXIT
        CASE 4
            cSky := "SKY4"
            EXIT
        OTHERWISE
            cSky := "SKY1"
        ENDSWITCH
    ENDIF

    skytexture := R_TextureNumForName( DEH_String( cSky ) )
    G_DoLoadLevel()
RETURN NIL

FUNCTION G_ReadDemoTiccmd( cmd )
    IF demo_p > Len( demobuffer ) .OR. Asc( SubStr( demobuffer, demo_p, 1 ) ) == DEMOMARKER
        G_CheckDemoStatus()
        RETURN NIL
    ENDIF

    cmd:forwardmove := DemoSByte()
    cmd:sidemove := DemoSByte()
    IF longtics
        cmd:angleturn := DemoByte() + DemoByte() * 256
        IF cmd:angleturn >= 32768
            cmd:angleturn := cmd:angleturn - 65536
        ENDIF
    ELSE
        cmd:angleturn := DemoByte() * 256
        IF cmd:angleturn >= 32768
            cmd:angleturn := cmd:angleturn - 65536
        ENDIF
    ENDIF
    cmd:buttons := DemoByte()
RETURN NIL

STATIC PROCEDURE IncreaseDemoBuffer()
    LOCAL nPos

    nPos := demo_p
    demobuffer := demobuffer + Replicate( Chr( 0 ), Len( demobuffer ) )
    demo_p := nPos
    demoend := Len( demobuffer ) + 1
RETURN

FUNCTION G_WriteDemoTiccmd( cmd )
    LOCAL nStart
    MEMVAR key_demo_quit
    MEMVAR vanilla_demo_limit

    IF GameKey( key_demo_quit )
        G_CheckDemoStatus()
    ENDIF

    nStart := demo_p
    DemoPut( cmd:forwardmove )
    DemoPut( cmd:sidemove )
    IF longtics
        DemoPut( cmd:angleturn )
        DemoPut( Int( cmd:angleturn / 256 ) )
    ELSE
        DemoPut( Int( cmd:angleturn / 256 ) )
    ENDIF
    DemoPut( cmd:buttons )

    demo_p := nStart
    IF demo_p > demoend - 16
        IF vanilla_demo_limit != 0
            G_CheckDemoStatus()
            RETURN NIL
        ENDIF
        IncreaseDemoBuffer()
    ENDIF

    G_ReadDemoTiccmd( cmd )
RETURN NIL

FUNCTION G_RecordDemo( name )
    LOCAL nMax
    LOCAL i
    MEMVAR demorecording
    MEMVAR myargv
    MEMVAR usergame

    usergame := .F.
    demoname := name + ".lmp"
    nMax := 0x20000
    i := M_CheckParmWithArgs( "-maxdemo", 1 )
    IF i != 0
        nMax := Val( myargv[ i + 1 + 1 ] ) * 1024
    ENDIF
    demobuffer := Replicate( Chr( 0 ), nMax )
    demo_p := 1
    demoend := nMax + 1
    demorecording := .T.
RETURN NIL

FUNCTION G_VanillaVersionCode()
    MEMVAR gameversion
    SWITCH gameversion
    CASE exe_doom_1_2
        I_Error( "Doom 1.2 does not have a version code!" )
        EXIT
    CASE exe_doom_1_666
        RETURN 106
    CASE exe_doom_1_7
        RETURN 107
    CASE exe_doom_1_8
        RETURN 108
    CASE exe_doom_1_9
    OTHERWISE
        RETURN 109
    ENDSWITCH
RETURN 109

FUNCTION G_BeginRecording()
    LOCAL i
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR fastparm
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gameskill
    MEMVAR lowres_turn
    MEMVAR nomonsters
    MEMVAR playeringame
    MEMVAR respawnparm

    longtics := ( M_CheckParm( "-longtics" ) != 0 )
    lowres_turn := ! longtics
    demo_p := 1

    IF longtics
        DemoPut( DOOM_191_VERSION )
    ELSE
        DemoPut( G_VanillaVersionCode() )
    ENDIF

    DemoPut( gameskill )
    DemoPut( gameepisode )
    DemoPut( gamemap )
    DemoPut( deathmatch )
    DemoPut( iif( respawnparm, 1, 0 ) )
    DemoPut( iif( fastparm, 1, 0 ) )
    DemoPut( iif( nomonsters, 1, 0 ) )
    DemoPut( consoleplayer )

    FOR i := 0 TO MAXPLAYERS - 1
        DemoPut( iif( playeringame[ i + 1 ], 1, 0 ) )
    NEXT
RETURN NIL

FUNCTION G_DeferedPlayDemo( name )
    MEMVAR gameaction
    defdemoname := name
    gameaction := ga_playdemo
RETURN NIL

FUNCTION G_PlayDemo( name )
    G_DeferedPlayDemo( name )
RETURN NIL

STATIC FUNCTION DemoVersionDescription( nVer )
    SWITCH nVer
    CASE 104
        RETURN "v1.4"
    CASE 105
        RETURN "v1.5"
    CASE 106
        RETURN "v1.6/v1.666"
    CASE 107
        RETURN "v1.7/v1.7a"
    CASE 108
        RETURN "v1.8"
    CASE 109
        RETURN "v1.9"
    ENDSWITCH

    IF nVer >= 0 .AND. nVer <= 4
        RETURN "v1.0/v1.1/v1.2"
    ENDIF
RETURN hb_ntos( Int( nVer / 100 ) ) + "." + hb_ntos( nVer % 100 ) + " (unknown)"

FUNCTION G_DoPlayDemo()
    LOCAL nSkill
    LOCAL nEpisode
    LOCAL nMap
    LOCAL nVer
    LOCAL i
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR demoplayback
    MEMVAR fastparm
    MEMVAR gameaction
    MEMVAR netgame
    MEMVAR nomonsters
    MEMVAR playeringame
    MEMVAR precache
    MEMVAR respawnparm
    MEMVAR usergame

    gameaction := ga_nothing
    demobuffer := W_CacheLumpName( defdemoname, PU_STATIC )
    IF ValType( demobuffer ) != "C"
        demobuffer := ""
    ENDIF
    demo_p := 1
    demoend := Len( demobuffer ) + 1

    nVer := DemoByte()
    IF nVer == G_VanillaVersionCode()
        longtics := .F.
    ELSEIF nVer == DOOM_191_VERSION
        longtics := .T.
    ELSE
        OutStd( "Demo is from a different game version!" + hb_eol() + ;
            "(read " + hb_ntos( nVer ) + ", should be " + hb_ntos( G_VanillaVersionCode() ) + ")" + hb_eol() + ;
            "This appears to be " + DemoVersionDescription( nVer ) + "." + hb_eol() )
    ENDIF

    nSkill := DemoByte()
    nEpisode := DemoByte()
    nMap := DemoByte()
    deathmatch := DemoByte()
    respawnparm := ( DemoByte() != 0 )
    fastparm := ( DemoByte() != 0 )
    nomonsters := ( DemoByte() != 0 )
    consoleplayer := DemoByte()

    FOR i := 0 TO MAXPLAYERS - 1
        playeringame[ i + 1 ] := ( DemoByte() != 0 )
    NEXT

    IF playeringame[ 2 ] .OR. M_CheckParm( "-solo-net" ) > 0 .OR. M_CheckParm( "-netdemo" ) > 0
        netgame := .T.
        netdemo := .T.
    ENDIF

    precache := .F.
    G_InitNew( nSkill, nEpisode, nMap )
    precache := .T.
    starttime := I_GetTime()
    usergame := .F.
    demoplayback := .T.
RETURN NIL

FUNCTION G_TimeDemo( name )
    MEMVAR gameaction
    MEMVAR nodrawers
    MEMVAR singletics
    nodrawers := ( M_CheckParm( "-nodraw" ) != 0 )
    timingdemo := .T.
    singletics := .T.
    defdemoname := name
    gameaction := ga_playdemo
RETURN NIL

FUNCTION G_CheckDemoStatus()
    LOCAL nEnd
    LOCAL nReal
    LOCAL nFps
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR demoplayback
    MEMVAR demorecording
    MEMVAR fastparm
    MEMVAR gametic
    MEMVAR netgame
    MEMVAR nomonsters
    MEMVAR playeringame
    MEMVAR respawnparm
    MEMVAR singledemo

    IF timingdemo
        nEnd := I_GetTime()
        nReal := nEnd - starttime
        nFps := iif( nReal != 0, ( gametic * TICRATE ) / nReal, 0 )
        timingdemo := .F.
        demoplayback := .F.
        SyncFlush()
        I_Error( "timed " + hb_ntos( gametic ) + " gametics in " + hb_ntos( nReal ) + ;
            " realtics (" + hb_ntos( nFps ) + " fps)" )
    ENDIF

    IF demoplayback
        SyncFlush()
        W_ReleaseLumpName( defdemoname )
        demoplayback := .F.
        netdemo := .F.
        netgame := .F.
        deathmatch := 0
        playeringame[ 2 ] := .F.
        playeringame[ 3 ] := .F.
        playeringame[ 4 ] := .F.
        respawnparm := .F.
        fastparm := .F.
        nomonsters := .F.
        consoleplayer := 0
        IF singledemo
            I_Quit()
        ELSE
            D_AdvanceDemo()
        ENDIF
        RETURN .T.
    ENDIF

    IF demorecording
        DemoPut( DEMOMARKER )
        M_WriteFile( demoname, demobuffer, demo_p - 1 )
        demobuffer := ""
        demorecording := .F.
        I_Error( "Demo " + demoname + " recorded" )
    ENDIF
RETURN .F.
