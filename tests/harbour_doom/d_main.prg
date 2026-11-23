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

STATIC banners := {}
STATIC copyright_banners := {}
STATIC gameversions := {}
STATIC packs := {}
STATIC viewactivestate
STATIC menuactivestate
STATIC inhelpscreensstate
STATIC fullscreen
STATIC oldgamestate
STATIC borderdrawcount
STATIC storedemo
STATIC bfgedition
STATIC main_loop_started
STATIC wadfile
STATIC mapdir
STATIC show_endoom
STATIC demosequence
STATIC pagetic

/* Original: config.h, doomdef.h, doomstat.h, dstrings.h, sounds.h, ... */
#include "d_event.ch"
#include "d_englsh.ch"
#include "d_iwad.ch"
#include "d_loop.ch"
#include "d_main.ch"

#include "config.ch"
#include "doomfeatures.ch"
#include "deh_main.ch"

#ifndef PACKAGE_STRING
#define PACKAGE_STRING "doom_hb 0.1"
#endif

#ifndef PROGRAM_PREFIX
#define PROGRAM_PREFIX "doom_hb"
#endif

#ifndef SCREENWIDTH
#define SCREENWIDTH  320
#define SCREENHEIGHT 200
#endif

#ifndef PU_STATIC
#define PU_STATIC 1
#define PU_CACHE  7
#endif

#ifndef wipe_Melt
#define wipe_ColorXForm 0
#define wipe_Melt       1
#endif

#ifndef PST_LIVE
#define PST_LIVE   0
#define PST_DEAD   1
#define PST_REBORN 2
#endif

#ifndef exe_doom_1_2
#define exe_doom_1_2    0
#define exe_doom_1_666  1
#define exe_doom_1_7    2
#define exe_doom_1_8    3
#define exe_doom_1_9    4
#define exe_hacx        5
#define exe_ultimate    6
#define exe_final       7
#define exe_final2      8
#define exe_chex        9
#endif

#ifndef sk_medium
#define sk_noitems    -1
#define sk_baby        0
#define sk_easy        1
#define sk_medium      2
#define sk_hard        3
#define sk_nightmare   4
#endif

#ifndef mus_intro
#define mus_intro  29
#define mus_dm2ttl 66
#endif

#ifndef TICRATE
#define TICRATE 35
#endif




INIT PROCEDURE init_d_main

    PUBLIC savegamedir
    PUBLIC iwadfile
    PUBLIC devparm
    PUBLIC nomonsters
    PUBLIC respawnparm
    PUBLIC fastparm
    PUBLIC startskill
    PUBLIC startepisode
    PUBLIC startmap
    PUBLIC autostart
    PUBLIC startloadgame
    PUBLIC advancedemo
    PUBLIC wipegamestate
    PUBLIC pagename
    PUBLIC title

    savegamedir := NIL
    iwadfile := NIL
    devparm := .F.
    nomonsters := .F.
    respawnparm := .F.
    fastparm := .F.
    startskill := 0
    startepisode := 0
    startmap := 0
    autostart := .F.
    startloadgame := 0
    advancedemo := .F.
    storedemo := .F.
    bfgedition := .F.
    main_loop_started := .F.
    wadfile := ""
    mapdir := ""
    show_endoom := 1
    wipegamestate := GS_DEMOSCREEN
    demosequence := 0
    pagetic := 0
    pagename := NIL
    title := ""

    viewactivestate := .F.
    menuactivestate := .F.
    inhelpscreensstate := .F.
    fullscreen := .F.
    oldgamestate := -1
    borderdrawcount := 0

    banners := {}
    AAdd( banners, "                         " + "DOOM 2: Hell on Earth v%i.%i" + "                           " )
    AAdd( banners, "                            " + "DOOM Shareware Startup v%i.%i" + "                           " )
    AAdd( banners, "                            " + "DOOM Registered Startup v%i.%i" + "                           " )
    AAdd( banners, "                          " + "DOOM System Startup v%i.%i" + "                          " )
    AAdd( banners, "                         " + "The Ultimate DOOM Startup v%i.%i" + "                        " )
    AAdd( banners, "                     " + "DOOM 2: TNT - Evilution v%i.%i" + "                           " )
    AAdd( banners, "                   " + "DOOM 2: Plutonia Experiment v%i.%i" + "                           " )

    copyright_banners := {}
    AAdd( copyright_banners, ;
        e"===========================================================================\n" + ;
        e"ATTENTION:  This version of DOOM has been modified.  If you would like to\n" + ;
        e"get a copy of the original game, call 1-800-IDGAMES or see the readme file.\n" + ;
        e"        You will not receive technical support for modified games.\n" + ;
        e"                      press enter to continue\n" + ;
        e"===========================================================================\n" )
    AAdd( copyright_banners, ;
        e"===========================================================================\n" + ;
        e"                 Commercial product - do not distribute!\n" + ;
        e"         Please report software piracy to the SPA: 1-800-388-PIR8\n" + ;
        e"===========================================================================\n" )
    AAdd( copyright_banners, ;
        e"===========================================================================\n" + ;
        e"                                Shareware!\n" + ;
        e"===========================================================================\n" )

    gameversions := {}
    AAdd( gameversions, { "Doom 1.666",       "1.666",    exe_doom_1_666 } )
    AAdd( gameversions, { "Doom 1.7/1.7a",    "1.7",      exe_doom_1_7 } )
    AAdd( gameversions, { "Doom 1.8",         "1.8",      exe_doom_1_8 } )
    AAdd( gameversions, { "Doom 1.9",         "1.9",      exe_doom_1_9 } )
    AAdd( gameversions, { "Hacx",             "hacx",     exe_hacx } )
    AAdd( gameversions, { "Ultimate Doom",    "ultimate", exe_ultimate } )
    AAdd( gameversions, { "Final Doom",       "final",    exe_final } )
    AAdd( gameversions, { "Final Doom (alt)", "final2",   exe_final2 } )
    AAdd( gameversions, { "Chex Quest",       "chex",     exe_chex } )
    AAdd( gameversions, { NIL,                NIL,        0 } )

    packs := {}
    AAdd( packs, { "doom2",    doom2 } )
    AAdd( packs, { "tnt",      pack_tnt } )
    AAdd( packs, { "plutonia", pack_plut } )
RETURN

STATIC FUNCTION LogicalGameMission()
    MEMVAR gamemission
    IF gamemission == pack_chex
        RETURN doom
    ENDIF
    IF gamemission == pack_hacx
        RETURN doom2
    ENDIF
RETURN gamemission

STATIC FUNCTION DehPrint( cText )
    OutStd( cText )
RETURN NIL

STATIC FUNCTION DehAddStringReplacement( x, y )
    DEH_AddStringReplacement( x, y )
RETURN NIL

FUNCTION D_ProcessEvents()
    LOCAL ev

    IF storedemo
        RETURN NIL
    ENDIF

    DO WHILE ( ev := D_PopEvent() ) != NIL
        IF M_Responder( ev )
            LOOP
        ENDIF
        G_Responder( ev )
    ENDDO
RETURN NIL

FUNCTION D_Display()
    LOCAL nowtime
    LOCAL tics
    LOCAL wipestart
    LOCAL y
    LOCAL done
    LOCAL wipe
    LOCAL redrawsbar
    MEMVAR automapactive
    MEMVAR displayplayer
    MEMVAR gamestate
    MEMVAR gametic
    MEMVAR inhelpscreens
    MEMVAR menuactive
    MEMVAR nodrawers
    MEMVAR paused
    MEMVAR players
    MEMVAR scaledviewwidth
    MEMVAR setsizeneeded
    MEMVAR testcontrols
    MEMVAR testcontrols_mousespeed
    MEMVAR viewactive
    MEMVAR viewwindowx
    MEMVAR viewwindowy
    MEMVAR wipegamestate

    IF nodrawers
        RETURN NIL
    ENDIF

    redrawsbar := .F.

    IF setsizeneeded
        R_ExecuteSetViewSize()
        oldgamestate := -1
        borderdrawcount := 3
    ENDIF

    IF gamestate != wipegamestate
        IF gamestate == GS_LEVEL .AND. gametic == 0
            wipe := .F.
            wipegamestate := gamestate
        ELSE
            wipe := .T.
            wipe_StartScreen( 0, 0, SCREENWIDTH, SCREENHEIGHT )
        ENDIF
    ELSE
        wipe := .F.
    ENDIF

    IF gamestate == GS_LEVEL .AND. gametic != 0
        HU_Erase()
    ENDIF

    SWITCH gamestate
    CASE GS_LEVEL
        IF gametic == 0
            EXIT
        ENDIF
        IF automapactive
            AM_Drawer()
        ENDIF
        IF wipe .OR. ( viewheight != 200 .AND. fullscreen )
            redrawsbar := .T.
        ENDIF
        IF inhelpscreensstate .AND. ! inhelpscreens
            redrawsbar := .T.
        ENDIF
        ST_Drawer( viewheight == 200, redrawsbar )
        fullscreen := ( viewheight == 200 )
        EXIT

    CASE GS_INTERMISSION
        WI_Drawer()
        EXIT

    CASE GS_FINALE
        F_Drawer()
        EXIT

    CASE GS_DEMOSCREEN
        D_PageDrawer()
        EXIT
    ENDSWITCH

    I_UpdateNoBlit()

    IF gamestate == GS_LEVEL .AND. ! automapactive .AND. gametic != 0
        R_RenderPlayerView( players[ displayplayer + 1 ] )
    ENDIF

    IF gamestate == GS_LEVEL .AND. gametic != 0
        HU_Drawer()
    ENDIF

    IF gamestate != oldgamestate .AND. gamestate != GS_LEVEL
        I_SetPalette( W_CacheLumpName( DEH_String( "PLAYPAL" ), PU_CACHE ) )
    ENDIF

    IF gamestate == GS_LEVEL .AND. oldgamestate != GS_LEVEL
        viewactivestate := .F.
        R_FillBackScreen()
    ENDIF

    IF gamestate == GS_LEVEL .AND. ! automapactive .AND. scaledviewwidth != 320
        IF menuactive != 0 .OR. menuactivestate .OR. ! viewactivestate
            borderdrawcount := 3
        ENDIF
        IF borderdrawcount != 0
            R_DrawViewBorder()
            borderdrawcount := borderdrawcount - 1
        ENDIF
    ENDIF

    IF testcontrols
        V_DrawMouseSpeedBox( testcontrols_mousespeed )
    ENDIF

    menuactivestate := ( menuactive != 0 )
    viewactivestate := viewactive
    inhelpscreensstate := inhelpscreens
    oldgamestate := gamestate
    wipegamestate := gamestate

    IF paused
        IF automapactive
            y := 4
        ELSE
            y := viewwindowy + 4
        ENDIF
        V_DrawPatchDirect( viewwindowx + Int( ( scaledviewwidth - 68 ) / 2 ), y, ;
            W_CacheLumpName( DEH_String( "M_PAUSE" ), PU_CACHE ) )
    ENDIF

    M_Drawer()
    NetUpdate()

    IF ! wipe
        I_FinishUpdate()
        RETURN NIL
    ENDIF

    wipe_EndScreen( 0, 0, SCREENWIDTH, SCREENHEIGHT )

    wipestart := I_GetTime() - 1

    DO WHILE .T.
        DO WHILE .T.
            nowtime := I_GetTime()
            tics := nowtime - wipestart
            I_Sleep( 1 )
            IF tics > 0
                EXIT
            ENDIF
        ENDDO

        wipestart := nowtime
        done := wipe_ScreenWipe( wipe_Melt, 0, 0, SCREENWIDTH, SCREENHEIGHT, tics )
        I_UpdateNoBlit()
        M_Drawer()
        I_FinishUpdate()
        IF done != 0
            EXIT
        ENDIF
    ENDDO
RETURN NIL

FUNCTION D_BindVariables()
    LOCAL i
    LOCAL buf
    MEMVAR chat_macros
    MEMVAR detailLevel
    MEMVAR key_multi_msgplayer
    MEMVAR mouseSensitivity
    MEMVAR musicVolume
    MEMVAR screenblocks
    MEMVAR sfxVolume
    MEMVAR showMessages
    MEMVAR snd_channels
    MEMVAR vanilla_demo_limit
    MEMVAR vanilla_savegame_limit

    M_ApplyPlatformDefaults()

    I_BindVideoVariables()
    I_BindJoystickVariables()
    I_BindSoundVariables()

    M_BindBaseControls()
    M_BindWeaponControls()
    M_BindMapControls()
    M_BindMenuControls()
    M_BindChatControls( MAXPLAYERS )

    key_multi_msgplayer[ 1 ] := HUSTR_KEYGREEN
    key_multi_msgplayer[ 2 ] := HUSTR_KEYINDIGO
    key_multi_msgplayer[ 3 ] := HUSTR_KEYBROWN
    key_multi_msgplayer[ 4 ] := HUSTR_KEYRED

#ifdef FEATURE_MULTIPLAYER
    NET_BindVariables()
#endif

    M_BindVariable( "mouse_sensitivity",      @mouseSensitivity )
    M_BindVariable( "sfx_volume",             @sfxVolume )
    M_BindVariable( "music_volume",           @musicVolume )
    M_BindVariable( "show_messages",          @showMessages )
    M_BindVariable( "screenblocks",           @screenblocks )
    M_BindVariable( "detaillevel",            @detailLevel )
    M_BindVariable( "snd_channels",           @snd_channels )
    M_BindVariable( "vanilla_savegame_limit", @vanilla_savegame_limit )
    M_BindVariable( "vanilla_demo_limit",     @vanilla_demo_limit )
    M_BindVariable( "show_endoom",            @show_endoom )

    FOR i := 0 TO 9
        buf := "chatmacro" + hb_ntos( i )
        M_BindVariable( buf, @chat_macros[ i + 1 ] )
    NEXT
RETURN NIL

FUNCTION D_GrabMouseCallback()
    MEMVAR advancedemo
    MEMVAR demoplayback
    MEMVAR drone
    MEMVAR gamestate
    MEMVAR menuactive
    MEMVAR paused
    IF drone
        RETURN .F.
    ENDIF
    IF menuactive != 0 .OR. paused
        RETURN .F.
    ENDIF
RETURN ( gamestate == GS_LEVEL ) .AND. ! demoplayback .AND. ! advancedemo

FUNCTION doomgeneric_Tick()
    MEMVAR consoleplayer
    MEMVAR players
    MEMVAR screenvisible
    I_StartFrame()
    TryRunTics()
    IF players[ consoleplayer + 1 ]:mo != NIL
        S_UpdateSounds( players[ consoleplayer + 1 ]:mo )
    ELSE
        S_UpdateSounds( NIL )
    ENDIF
    IF screenvisible
        D_Display()
    ENDIF
RETURN NIL

FUNCTION D_DoomLoop()
    MEMVAR demorecording
    MEMVAR gameaction
    MEMVAR gamedescription
    MEMVAR gamestate
    MEMVAR netgame
    MEMVAR testcontrols
    MEMVAR wipegamestate
    IF bfgedition .AND. ( demorecording .OR. gameaction == ga_playdemo .OR. netgame )
        OutStd( " WARNING: You are playing using one of the Doom Classic" + hb_eol() + ;
            " IWAD files shipped with the Doom 3: BFG Edition. These are" + hb_eol() + ;
            " known to be incompatible with the regular IWAD files and" + hb_eol() + ;
            " may cause demos and network games to get out of sync." + hb_eol() )
    ENDIF

    IF demorecording
        G_BeginRecording()
    ENDIF

    main_loop_started := .T.

    TryRunTics()

    I_SetWindowTitle( gamedescription )
    I_GraphicsCheckCommandLine()
    I_SetGrabMouseCallback( {|| D_GrabMouseCallback() } )
    I_InitGraphics()
    I_EnableLoadingDisk()

    V_RestoreBuffer()
    R_ExecuteSetViewSize()

    D_StartGameLoop()

    IF testcontrols
        wipegamestate := gamestate
    ENDIF

    doomgeneric_Tick()
RETURN NIL

FUNCTION D_PageTicker()
    pagetic := pagetic - 1
    IF pagetic < 0
        D_AdvanceDemo()
    ENDIF
RETURN NIL

FUNCTION D_PageDrawer()
    MEMVAR pagename
    IF Empty( pagename )
        RETURN NIL
    ENDIF
    V_DrawPatch( 0, 0, W_CacheLumpName( pagename, PU_CACHE ) )
RETURN NIL

FUNCTION D_AdvanceDemo()
    MEMVAR advancedemo
    advancedemo := .T.
RETURN NIL

FUNCTION D_DoAdvanceDemo()
    MEMVAR advancedemo
    MEMVAR consoleplayer
    MEMVAR gameaction
    MEMVAR gamemode
    MEMVAR gamestate
    MEMVAR gameversion
    MEMVAR pagename
    MEMVAR paused
    MEMVAR players
    MEMVAR usergame
    players[ consoleplayer + 1 ]:playerstate := PST_LIVE
    advancedemo := .F.
    usergame := .F.
    paused := .F.
    gameaction := ga_nothing

    // Harbour SWITCH only accepts integers; `%` yields a float and raises BASE/3104.
    demosequence := Int( demosequence ) + 1
    IF gameversion == exe_ultimate .OR. gameversion == exe_final
        IF demosequence >= 7
            demosequence := 0
        ENDIF
    ELSEIF demosequence >= 6
        demosequence := 0
    ENDIF

    SWITCH demosequence
    CASE 0
        IF gamemode == commercial
            pagetic := TICRATE * 11
        ELSE
            pagetic := 170
        ENDIF
        gamestate := GS_DEMOSCREEN
        pagename := DEH_String( "TITLEPIC" )
        IF gamemode == commercial
            S_StartMusic( mus_dm2ttl )
        ELSE
            S_StartMusic( mus_intro )
        ENDIF
        EXIT

    CASE 1
        G_DeferedPlayDemo( DEH_String( "demo1" ) )
        EXIT

    CASE 2
        pagetic := 200
        gamestate := GS_DEMOSCREEN
        pagename := DEH_String( "CREDIT" )
        EXIT

    CASE 3
        G_DeferedPlayDemo( DEH_String( "demo2" ) )
        EXIT

    CASE 4
        gamestate := GS_DEMOSCREEN
        IF gamemode == commercial
            pagetic := TICRATE * 11
            pagename := DEH_String( "TITLEPIC" )
            S_StartMusic( mus_dm2ttl )
        ELSE
            pagetic := 200
            IF gamemode == retail
                pagename := DEH_String( "CREDIT" )
            ELSE
                pagename := DEH_String( "HELP2" )
            ENDIF
        ENDIF
        EXIT

    CASE 5
        G_DeferedPlayDemo( DEH_String( "demo3" ) )
        EXIT

    CASE 6
        G_DeferedPlayDemo( DEH_String( "demo4" ) )
        EXIT
    ENDSWITCH

    IF bfgedition .AND. hb_stricmp( pagename, "TITLEPIC" ) == 0 ;
         .AND. W_CheckNumForName( "titlepic" ) < 0
        pagename := DEH_String( "INTERPIC" )
    ENDIF
RETURN NIL

FUNCTION D_StartTitle()
    MEMVAR gameaction
    MEMVAR gamestate
    MEMVAR wipegamestate
    gameaction := ga_nothing
    demosequence := -1
    D_AdvanceDemo()
    D_DoAdvanceDemo()
    wipegamestate := gamestate
RETURN NIL

STATIC FUNCTION GetGameName( gamename )
    LOCAL i
    LOCAL deh_sub
    LOCAL version

    FOR i := 0 TO Len( banners ) - 1
        deh_sub := DEH_String( banners[ i + 1 ] )
        IF ! ( deh_sub == banners[ i + 1 ] )
            version := G_VanillaVersionCode()
            gamename := hb_StrFormat( deh_sub, Int( version / 100 ), version % 100 )
            DO WHILE ! Empty( gamename ) .AND. Asc( Left( gamename, 1 ) ) <= 32
                gamename := SubStr( gamename, 2 )
            ENDDO
            DO WHILE ! Empty( gamename ) .AND. Asc( Right( gamename, 1 ) ) <= 32
                gamename := Left( gamename, Len( gamename ) - 1 )
            ENDDO
            RETURN gamename
        ENDIF
    NEXT
RETURN gamename

STATIC FUNCTION SetMissionForPackName( pack_name )
    LOCAL i
    MEMVAR gamemission

    FOR i := 0 TO Len( packs ) - 1
        IF hb_stricmp( pack_name, packs[ i + 1 ][ 1 ] ) == 0
            gamemission := packs[ i + 1 ][ 2 ]
            RETURN NIL
        ENDIF
    NEXT

    OutStd( "Valid mission packs are:" + hb_eol() )
    FOR i := 0 TO Len( packs ) - 1
        OutStd( e"\t" + packs[ i + 1 ][ 1 ] + hb_eol() )
    NEXT
    I_Error( "Unknown mission pack name: " + pack_name )
RETURN NIL

FUNCTION D_IdentifyVersion()
    LOCAL i
    LOCAL p
    MEMVAR gamemission
    MEMVAR gamemode
    MEMVAR lumpinfo
    MEMVAR myargv
    MEMVAR numlumps

    IF gamemission == none
        FOR i := 0 TO numlumps - 1
            IF hb_strnicmp( lumpinfo[ i + 1 ]:name, "MAP01", 8 ) == 0
                gamemission := doom2
                EXIT
            ELSEIF hb_strnicmp( lumpinfo[ i + 1 ]:name, "E1M1", 8 ) == 0
                gamemission := doom
                EXIT
            ENDIF
        NEXT

        IF gamemission == none
            I_Error( "Unknown or invalid IWAD file." )
        ENDIF
    ENDIF

    IF LogicalGameMission() == doom
        IF W_CheckNumForName( "E4M1" ) > 0
            gamemode := retail
        ELSEIF W_CheckNumForName( "E3M1" ) > 0
            gamemode := registered
        ELSE
            gamemode := shareware
        ENDIF
    ELSE
        gamemode := commercial
        p := M_CheckParmWithArgs( "-pack", 1 )
        IF p > 0
            SetMissionForPackName( myargv[ p + 1 + 1 ] )
        ENDIF
    ENDIF
RETURN NIL

FUNCTION D_SetGameDescription()
    LOCAL is_freedoom := ( W_CheckNumForName( "FREEDOOM" ) >= 0 )
    LOCAL is_freedm := ( W_CheckNumForName( "FREEDM" ) >= 0 )
    MEMVAR gamedescription
    MEMVAR gamemode

    gamedescription := "Unknown"

    IF LogicalGameMission() == doom
        IF is_freedoom
            gamedescription := GetGameName( "Freedoom: Phase 1" )
        ELSEIF gamemode == retail
            gamedescription := GetGameName( "The Ultimate DOOM" )
        ELSEIF gamemode == registered
            gamedescription := GetGameName( "DOOM Registered" )
        ELSEIF gamemode == shareware
            gamedescription := GetGameName( "DOOM Shareware" )
        ENDIF
    ELSE
        IF is_freedoom
            IF is_freedm
                gamedescription := GetGameName( "FreeDM" )
            ELSE
                gamedescription := GetGameName( "Freedoom: Phase 2" )
            ENDIF
        ELSEIF LogicalGameMission() == doom2
            gamedescription := GetGameName( "DOOM 2: Hell on Earth" )
        ELSEIF LogicalGameMission() == pack_plut
            gamedescription := GetGameName( "DOOM 2: Plutonia Experiment" )
        ELSEIF LogicalGameMission() == pack_tnt
            gamedescription := GetGameName( "DOOM 2: TNT - Evilution" )
        ENDIF
    ENDIF
RETURN NIL

STATIC FUNCTION D_AddFile( filename )
    LOCAL handle

    OutStd( " adding " + filename + hb_eol() )
    handle := W_AddFile( filename )
RETURN handle != NIL

FUNCTION PrintDehackedBanners()
    LOCAL i
    LOCAL deh_s

    FOR i := 0 TO Len( copyright_banners ) - 1
        deh_s := DEH_String( copyright_banners[ i + 1 ] )
        IF ! ( deh_s == copyright_banners[ i + 1 ] )
            OutStd( deh_s )
            IF Right( deh_s, 1 ) != Chr( 10 )
                OutStd( hb_eol() )
            ENDIF
        ENDIF
    NEXT
RETURN NIL

STATIC FUNCTION InitGameVersion()
    LOCAL p
    LOCAL i
    MEMVAR gamemission
    MEMVAR gamemode
    MEMVAR gameversion
    MEMVAR myargv

    p := M_CheckParmWithArgs( "-gameversion", 1 )

    IF p != 0
        i := 0
        DO WHILE gameversions[ i + 1 ][ 1 ] != NIL
            IF myargv[ p + 1 + 1 ] == gameversions[ i + 1 ][ 2 ]
                gameversion := gameversions[ i + 1 ][ 3 ]
                EXIT
            ENDIF
            i := i + 1
        ENDDO

        IF gameversions[ i + 1 ][ 1 ] == NIL
            OutStd( "Supported game versions:" + hb_eol() )
            i := 0
            DO WHILE gameversions[ i + 1 ][ 1 ] != NIL
                OutStd( e"\t" + gameversions[ i + 1 ][ 2 ] + " (" + gameversions[ i + 1 ][ 1 ] + ")" + hb_eol() )
                i := i + 1
            ENDDO
            I_Error( "Unknown game version '" + myargv[ p + 1 + 1 ] + "'" )
        ENDIF
    ELSE
        IF gamemission == pack_chex
            gameversion := exe_chex
        ELSEIF gamemission == pack_hacx
            gameversion := exe_hacx
        ELSEIF gamemode == shareware .OR. gamemode == registered
            gameversion := exe_doom_1_9
        ELSEIF gamemode == retail
            gameversion := exe_ultimate
        ELSEIF gamemode == commercial
            IF gamemission == doom2
                gameversion := exe_doom_1_9
            ELSE
                gameversion := exe_final
            ENDIF
        ENDIF
    ENDIF

    IF gameversion < exe_ultimate .AND. gamemode == retail
        gamemode := registered
    ENDIF

    IF gameversion < exe_final .AND. gamemode == commercial ;
         .AND. ( gamemission == pack_tnt .OR. gamemission == pack_plut )
        gamemission := doom2
    ENDIF
RETURN NIL

FUNCTION PrintGameVersion()
    LOCAL i
    MEMVAR gameversion

    i := 0
    DO WHILE gameversions[ i + 1 ][ 1 ] != NIL
        IF gameversions[ i + 1 ][ 3 ] == gameversion
            OutStd( "Emulating the behavior of the '" + gameversions[ i + 1 ][ 1 ] + "' executable." + hb_eol() )
            EXIT
        ENDIF
        i := i + 1
    ENDDO
RETURN NIL

STATIC FUNCTION D_Endoom()
    // ENDOOM is a DOS text-mode exit screen; this port has no console
    // display for it (I_Endoom is a stub). Skip it so I_Quit does not
    // hit Harbour BASE/1066: show_endoom is an INT (1), and "!" requires
    // a logical.
RETURN NIL

#ifdef ORIGCODE
STATIC FUNCTION LoadIwadDeh()
    LOCAL chex_deh
    LOCAL sep
    MEMVAR gameversion
    MEMVAR iwadfile

    IF W_CheckNumForName( "FREEDOOM" ) >= 0
        DEH_LoadLumpByName( "DEHACKED", .F., .T. )
    ENDIF

    IF gameversion == exe_hacx
        IF ! DEH_LoadLumpByName( "DEHACKED", .T., .F. )
            I_Error( "DEHACKED lump not found.  Please check that this is the " + ;
                "Hacx v1.2 IWAD." )
        ENDIF
    ENDIF

    IF gameversion == exe_chex
        sep := RAt( hb_ps(), iwadfile )
        IF sep > 0
            chex_deh := Left( iwadfile, sep ) + "chex.deh"
        ELSE
            chex_deh := "chex.deh"
        ENDIF

        IF ! M_FileExists( chex_deh )
            chex_deh := D_FindWADByName( "chex.deh" )
        ENDIF

        IF chex_deh == NIL
            I_Error( "Unable to find Chex Quest dehacked file (chex.deh)." + hb_eol() + ;
                "The dehacked file is required in order to emulate" + hb_eol() + ;
                "chex.exe correctly.  It can be found in your nearest" + hb_eol() + ;
                "/idgames repository mirror at:" + hb_eol() + hb_eol() + ;
                "   utils/exe_edit/patches/chexdeh.zip" )
        ENDIF

        IF ! DEH_LoadFile( chex_deh )
            I_Error( "Failed to load chex.deh needed for emulating chex.exe." )
        ENDIF
    ENDIF
RETURN NIL
#endif

FUNCTION D_DoomMain()
    LOCAL p
    LOCAL file
    LOCAL demolumpname
    LOCAL scale
    LOCAL name
    LOCAL i
    MEMVAR autostart
    MEMVAR configdir
    MEMVAR deathmatch
    MEMVAR devparm
    MEMVAR fastparm
    MEMVAR forwardmove
    MEMVAR gameaction
    MEMVAR gamedescription
    MEMVAR gamemission
    MEMVAR gamemode
    MEMVAR iwadfile
    MEMVAR lumpinfo
    MEMVAR modifiedgame
    MEMVAR musicVolume
    MEMVAR myargc
    MEMVAR myargv
    MEMVAR netgame
    MEMVAR nomonsters
    MEMVAR numlumps
    MEMVAR respawnparm
    MEMVAR savegamedir
    MEMVAR sfxVolume
    MEMVAR sidemove
    MEMVAR singledemo
    MEMVAR startepisode
    MEMVAR startloadgame
    MEMVAR startmap
    MEMVAR startskill
    MEMVAR testcontrols
    MEMVAR timelimit
    LOCAL numiwadlumps
    LOCAL loaded

    I_AtExit( {|| D_Endoom() }, .F. )

    I_PrintBanner( PACKAGE_STRING )

    DehPrint( "Z_Init: Init zone memory allocation daemon. " + hb_eol() )
    Z_Init()

#ifdef FEATURE_MULTIPLAYER
    IF M_CheckParm( "-dedicated" ) > 0
        OutStd( "Dedicated server mode." + hb_eol() )
        NET_DedicatedServer()
    ENDIF

    IF M_CheckParm( "-search" ) != 0
        NET_MasterQuery()
        QUIT
    ENDIF

    p := M_CheckParmWithArgs( "-query", 1 )
    IF p != 0
        NET_QueryAddress( myargv[ p + 1 + 1 ] )
        QUIT
    ENDIF

    IF M_CheckParm( "-localsearch" ) != 0
        NET_LANQuery()
        QUIT
    ENDIF
#endif

    nomonsters := ( M_CheckParm( "-nomonsters" ) != 0 )
    respawnparm := ( M_CheckParm( "-respawn" ) != 0 )
    fastparm := ( M_CheckParm( "-fast" ) != 0 )
    devparm := ( M_CheckParm( "-devparm" ) != 0 )

    I_DisplayFPSDots( devparm )

    IF M_CheckParm( "-deathmatch" ) != 0
        deathmatch := 1
    ENDIF

    IF M_CheckParm( "-altdeath" ) != 0
        deathmatch := 2
    ENDIF

    IF devparm
        DehPrint( D_DEVSTR )
    ENDIF

#ifdef __PLATFORM__WINDOWS
    IF M_ParmExists( "-cdrom" )
        OutStd( D_CDROM )
        M_SetConfigDir( "c:\doomdata\" )
    ELSE
        M_SetConfigDir( NIL )
    ENDIF
#else
    M_SetConfigDir( NIL )
#endif

    p := M_CheckParm( "-turbo" )
    IF p != 0
        scale := 200
        IF p < myargc - 1
            scale := Int( Val( myargv[ p + 1 + 1 ] ) )
        ENDIF
        IF scale < 10
            scale := 10
        ENDIF
        IF scale > 400
            scale := 400
        ENDIF
        DehPrint( "turbo scale: " + hb_ntos( scale ) + "%" + hb_eol() )
        forwardmove[ 1 ] := Int( forwardmove[ 1 ] * scale / 100 )
        forwardmove[ 2 ] := Int( forwardmove[ 2 ] * scale / 100 )
        sidemove[ 1 ] := Int( sidemove[ 1 ] * scale / 100 )
        sidemove[ 2 ] := Int( sidemove[ 2 ] * scale / 100 )
    ENDIF

    DehPrint( "V_Init: allocate screens." + hb_eol() )
    V_Init()

    DehPrint( "M_LoadDefaults: Load system defaults." + hb_eol() )
    M_SetConfigFilenames( "default.cfg", PROGRAM_PREFIX + "doom.cfg" )
    D_BindVariables()
    M_LoadDefaults()

    I_AtExit( {|| M_SaveDefaults() }, .F. )

    iwadfile := D_FindIWAD( IWAD_MASK_DOOM, @gamemission )

    IF iwadfile == NIL
        I_Error( "Game mode indeterminate.  No IWAD file was found.  Try" + hb_eol() + ;
            "specifying one with the '-iwad' command line parameter." + hb_eol() )
    ENDIF

    modifiedgame := .F.

    DehPrint( "W_Init: Init WADfiles." + hb_eol() )
    D_AddFile( iwadfile )
    numiwadlumps := numlumps

    W_CheckCorrectIWAD( doom )

    D_IdentifyVersion()
    InitGameVersion()

#ifdef ORIGCODE
    IF ! M_ParmExists( "-nodeh" )
        LoadIwadDeh()
    ENDIF
#endif

    IF W_CheckNumForName( "dmenupic" ) >= 0
        OutStd( "BFG Edition: Using workarounds as needed." + hb_eol() )
        bfgedition := .T.

        DehAddStringReplacement( HUSTR_31, "level 31: idkfa" )
        DehAddStringReplacement( HUSTR_32, "level 32: keen" )
        DehAddStringReplacement( PHUSTR_1, "level 33: betray" )
        DehAddStringReplacement( "M_GDHIGH", "M_MSGON" )
        DehAddStringReplacement( "M_GDLOW", "M_MSGOFF" )
    ENDIF

#ifdef FEATURE_DEHACKED
    DEH_ParseCommandLine()
#endif

    modifiedgame := W_ParseCommandLine()

    p := M_CheckParmWithArgs( "-playdemo", 1 )
    IF p == 0
        p := M_CheckParmWithArgs( "-timedemo", 1 )
    ENDIF

    IF p != 0
        IF M_StringEndsWith( myargv[ p + 1 + 1 ], ".lmp" )
            file := myargv[ p + 1 + 1 ]
        ELSE
            file := myargv[ p + 1 + 1 ] + ".lmp"
        ENDIF

        IF D_AddFile( file )
            demolumpname := lumpinfo[ numlumps ]:name
        ELSE
            demolumpname := myargv[ p + 1 + 1 ]
        ENDIF

        OutStd( "Playing demo " + file + "." + hb_eol() )
    ENDIF

    I_AtExit( {|| G_CheckDemoStatus() }, .T. )

    W_GenerateHashTable()

    IF M_ParmExists( "-dehlump" )
        loaded := 0
        FOR i := numiwadlumps TO numlumps - 1
            IF hb_stricmp( AllTrim( lumpinfo[ i + 1 ]:name ), "DEHACKED" ) == 0
                DEH_LoadLump( i, .F., .F. )
                loaded := loaded + 1
            ENDIF
        NEXT
        OutStd( "  loaded " + hb_ntos( loaded ) + " DEHACKED lumps from PWAD files." + hb_eol() )
    ENDIF

    D_SetGameDescription()

#ifdef __PLATFORM__WINDOWS
    IF M_ParmExists( "-cdrom" )
        savegamedir := configdir
    ELSE
        savegamedir := M_GetSaveGameDir( D_SaveGameIWADName( gamemission ) )
    ENDIF
#else
    savegamedir := M_GetSaveGameDir( D_SaveGameIWADName( gamemission ) )
#endif

    IF modifiedgame
        name := {}
        AAdd( name, "e2m1" )
        AAdd( name, "e2m2" )
        AAdd( name, "e2m3" )
        AAdd( name, "e2m4" )
        AAdd( name, "e2m5" )
        AAdd( name, "e2m6" )
        AAdd( name, "e2m7" )
        AAdd( name, "e2m8" )
        AAdd( name, "e2m9" )
        AAdd( name, "e3m1" )
        AAdd( name, "e3m3" )
        AAdd( name, "e3m3" )
        AAdd( name, "e3m4" )
        AAdd( name, "e3m5" )
        AAdd( name, "e3m6" )
        AAdd( name, "e3m7" )
        AAdd( name, "e3m8" )
        AAdd( name, "e3m9" )
        AAdd( name, "dphoof" )
        AAdd( name, "bfgga0" )
        AAdd( name, "heada1" )
        AAdd( name, "cybra1" )
        AAdd( name, "spida1d1" )

        IF gamemode == shareware
            I_Error( DEH_String( e"\nYou cannot -file with the shareware version. Register!" ) )
        ENDIF

        IF gamemode == registered
            FOR i := 0 TO 22
                IF W_CheckNumForName( name[ i + 1 ] ) < 0
                    I_Error( DEH_String( e"\nThis is not the registered version." ) )
                ENDIF
            NEXT
        ENDIF
    ENDIF

    IF W_CheckNumForName( "SS_START" ) >= 0 .OR. W_CheckNumForName( "FF_END" ) >= 0
        I_PrintDivider()
        OutStd( " WARNING: The loaded WAD file contains modified sprites or" + hb_eol() + ;
            " floor textures.  You may want to use the '-merge' command" + hb_eol() + ;
            " line option instead of '-file'." + hb_eol() )
    ENDIF

    I_PrintStartupBanner( gamedescription )
    PrintDehackedBanners()

    IF W_CheckNumForName( "FREEDOOM" ) >= 0 .AND. W_CheckNumForName( "FREEDM" ) < 0
        OutStd( " WARNING: You are playing using one of the Freedoom IWAD" + hb_eol() + ;
            " files, which might not work in this port. See this page" + hb_eol() + ;
            " for more information on how to play using Freedoom:" + hb_eol() + ;
            "   http://www.chocolate-doom.org/wiki/index.php/Freedoom" + hb_eol() )
        I_PrintDivider()
    ENDIF

    DehPrint( "I_Init: Setting up machine state." + hb_eol() )
    I_CheckIsScreensaver()
    I_InitTimer()
    I_InitJoystick()
    I_InitSound( .T. )
    I_InitMusic()

#ifdef FEATURE_MULTIPLAYER
    OutStd( "NET_Init: Init network subsystem." + hb_eol() )
    NET_Init()
#endif

    D_ConnectNetGame()

    startskill := sk_medium
    startepisode := 1
    startmap := 1
    autostart := .F.

    p := M_CheckParmWithArgs( "-skill", 1 )
    IF p != 0
        startskill := Asc( Left( myargv[ p + 1 + 1 ], 1 ) ) - Asc( "1" )
        autostart := .T.
    ENDIF

    p := M_CheckParmWithArgs( "-episode", 1 )
    IF p != 0
        startepisode := Asc( Left( myargv[ p + 1 + 1 ], 1 ) ) - Asc( "0" )
        startmap := 1
        autostart := .T.
    ENDIF

    timelimit := 0

    p := M_CheckParmWithArgs( "-timer", 1 )
    IF p != 0
        timelimit := Int( Val( myargv[ p + 1 + 1 ] ) )
    ENDIF

    p := M_CheckParm( "-avg" )
    IF p != 0
        timelimit := 20
    ENDIF

    p := M_CheckParmWithArgs( "-warp", 1 )
    IF p != 0
        IF gamemode == commercial
            startmap := Int( Val( myargv[ p + 1 + 1 ] ) )
        ELSE
            startepisode := Asc( Left( myargv[ p + 1 + 1 ], 1 ) ) - Asc( "0" )
            IF p + 2 < myargc
                startmap := Asc( Left( myargv[ p + 2 + 1 ], 1 ) ) - Asc( "0" )
            ELSE
                startmap := 1
            ENDIF
        ENDIF
        autostart := .T.
    ENDIF

    p := M_CheckParm( "-testcontrols" )
    IF p > 0
        startepisode := 1
        startmap := 1
        autostart := .T.
        testcontrols := .T.
    ENDIF

    p := M_CheckParmWithArgs( "-loadgame", 1 )
    IF p != 0
        startloadgame := Int( Val( myargv[ p + 1 + 1 ] ) )
    ELSE
        startloadgame := -1
    ENDIF

    DehPrint( "M_Init: Init miscellaneous info." + hb_eol() )
    M_Init()

    DehPrint( "R_Init: Init DOOM refresh daemon - " )
    R_Init()

    DehPrint( hb_eol() + "P_Init: Init Playloop state." + hb_eol() )
    P_Init()

    DehPrint( "S_Init: Setting up sound." + hb_eol() )
    S_Init( sfxVolume * 8, musicVolume * 8 )

    DehPrint( "D_CheckNetGame: Checking network game status." + hb_eol() )
    D_CheckNetGame()

    PrintGameVersion()

    DehPrint( "HU_Init: Setting up heads up display." + hb_eol() )
    HU_Init()

    DehPrint( "ST_Init: Init status bar." + hb_eol() )
    ST_Init()

    IF gamemode == commercial .AND. W_CheckNumForName( "map01" ) < 0
        storedemo := .T.
    ENDIF

    IF M_CheckParmWithArgs( "-statdump", 1 ) != 0
        I_AtExit( {|| StatDump() }, .T. )
        DehPrint( "External statistics registered." + hb_eol() )
    ENDIF

    p := M_CheckParmWithArgs( "-record", 1 )
    IF p != 0
        G_RecordDemo( myargv[ p + 1 + 1 ] )
        autostart := .T.
    ENDIF

    p := M_CheckParmWithArgs( "-playdemo", 1 )
    IF p != 0
        singledemo := .T.
        G_DeferedPlayDemo( demolumpname )
        D_DoomLoop()
        RETURN NIL
    ENDIF

    p := M_CheckParmWithArgs( "-timedemo", 1 )
    IF p != 0
        G_TimeDemo( demolumpname )
        D_DoomLoop()
        RETURN NIL
    ENDIF

    IF startloadgame >= 0
        file := P_SaveGameFile( startloadgame )
        G_LoadGame( file )
    ENDIF

    IF gameaction != ga_loadgame
        IF autostart .OR. netgame
            G_InitNew( startskill, startepisode, startmap )
        ELSE
            D_StartTitle()
        ENDIF
    ENDIF

    D_DoomLoop()
RETURN NIL
