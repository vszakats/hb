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

STATIC exitmsg := ""
STATIC doom_loop_interface

/* Original: doomfeatures.h, d_main.h, m_argv.h, g_game.h, doomstat.h, ... */
#include "d_main.ch"
#include "d_loop.ch"
#include "d_net.ch"



INIT PROCEDURE init_d_net

    PUBLIC netcmds

    netcmds := NIL
    exitmsg := ""

    doom_loop_interface := loop_interface_t():New()
    doom_loop_interface:ProcessEvents := {|| D_ProcessEvents() }
    doom_loop_interface:BuildTiccmd   := {| cmd, maketic | G_BuildTiccmd( cmd, maketic ) }
    doom_loop_interface:RunTic        := {| cmds, ingame | RunTic( cmds, ingame ) }
    doom_loop_interface:RunMenu       := {|| M_Ticker() }
RETURN

/* player_num e o indice C (0-based) do jogador que saiu */
STATIC FUNCTION PlayerQuitGame( player_num )
    LOCAL cDigit
    MEMVAR consoleplayer
    MEMVAR demorecording
    MEMVAR playeringame
    MEMVAR players

    exitmsg := DEH_String( "Player 1 left the game" )
    cDigit := Chr( ( ( Asc( SubStr( exitmsg, 8, 1 ) ) + player_num ) & 0xFF ) )
    exitmsg := Left( exitmsg, 7 ) + cDigit + SubStr( exitmsg, 9 )

    playeringame[ player_num + 1 ] := .F.
    players[ consoleplayer + 1 ]:message := exitmsg

    IF demorecording
        G_CheckDemoStatus()
    ENDIF
RETURN NIL

STATIC FUNCTION RunTic( cmds, ingame )
    LOCAL i
    MEMVAR advancedemo
    MEMVAR demoplayback
    MEMVAR netcmds
    MEMVAR playeringame

    FOR i := 0 TO MAXPLAYERS - 1
        IF ! demoplayback .AND. playeringame[ i + 1 ] .AND. ! ingame[ i + 1 ]
            PlayerQuitGame( i )
        ENDIF
    NEXT

    netcmds := cmds

    IF advancedemo
        D_DoAdvanceDemo()
    ENDIF

    G_Ticker()
RETURN NIL

STATIC FUNCTION LoadGameSettings( settings )
    LOCAL i
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR fastparm
    MEMVAR lowres_turn
    MEMVAR nomonsters
    MEMVAR playeringame
    MEMVAR respawnparm
    MEMVAR startepisode
    MEMVAR startloadgame
    MEMVAR startmap
    MEMVAR startskill
    MEMVAR timelimit

    deathmatch := settings:deathmatch
    startepisode := settings:episode
    startmap := settings:map
    startskill := settings:skill
    startloadgame := settings:loadgame
    lowres_turn := settings:lowres_turn
    nomonsters := settings:nomonsters
    fastparm := settings:fast_monsters
    respawnparm := settings:respawn_monsters
    timelimit := settings:timelimit
    consoleplayer := settings:consoleplayer

    IF lowres_turn
        OutStd( "NOTE: Turning resolution is reduced; this is probably " + ;
            "because there is a client recording a Vanilla demo." + hb_eol() )
    ENDIF

    FOR i := 0 TO MAXPLAYERS - 1
        playeringame[ i + 1 ] := ( i < settings:num_players )
    NEXT
RETURN NIL

STATIC FUNCTION SaveGameSettings( settings )

    MEMVAR deathmatch
    MEMVAR fastparm
    MEMVAR gameversion
    MEMVAR nomonsters
    MEMVAR respawnparm
    MEMVAR startepisode
    MEMVAR startloadgame
    MEMVAR startmap
    MEMVAR startskill
    MEMVAR timelimit
    settings:deathmatch := deathmatch
    settings:episode := startepisode
    settings:map := startmap
    settings:skill := startskill
    settings:loadgame := startloadgame
    settings:gameversion := gameversion
    settings:nomonsters := nomonsters
    settings:fast_monsters := fastparm
    settings:respawn_monsters := respawnparm
    settings:timelimit := timelimit

    settings:lowres_turn := ( M_CheckParm( "-record" ) > 0 ) .AND. ( M_CheckParm( "-longtics" ) == 0 )
RETURN NIL

STATIC FUNCTION InitConnectData( connect_data )

    MEMVAR gamemission
    MEMVAR gamemode
    MEMVAR viewangleoffset
    connect_data:max_players := MAXPLAYERS
    connect_data:drone := .F.

    IF M_CheckParm( "-left" ) > 0
        viewangleoffset := ANG90
        connect_data:drone := .T.
    ENDIF

    IF M_CheckParm( "-right" ) > 0
        viewangleoffset := ANG270
        connect_data:drone := .T.
    ENDIF

    connect_data:gamemode := gamemode
    connect_data:gamemission := gamemission

    connect_data:lowres_turn := ( M_CheckParm( "-record" ) > 0 ) .AND. ( M_CheckParm( "-longtics" ) == 0 )

    W_Checksum( connect_data:wad_sha1sum )

#ifdef ORIGCODE
    DEH_Checksum( connect_data:deh_sha1sum )
#endif

    connect_data:is_freedoom := iif( W_CheckNumForName( "FREEDOOM" ) >= 0, 1, 0 )
RETURN NIL

FUNCTION D_ConnectNetGame()
    LOCAL connect_data
    MEMVAR netgame

    connect_data := net_connect_data_t():New()
    InitConnectData( connect_data )
    netgame := D_InitNetGame( connect_data )

    IF M_CheckParm( "-solo-net" ) > 0
        netgame := .T.
    ENDIF
RETURN NIL

FUNCTION D_CheckNetGame()
    LOCAL settings
    MEMVAR autostart
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR netgame
    MEMVAR startepisode
    MEMVAR startmap
    MEMVAR startskill
    MEMVAR timelimit

    settings := net_gamesettings_t():New()

    IF netgame
        autostart := .T.
    ENDIF

    D_RegisterLoopCallbacks( doom_loop_interface )

    SaveGameSettings( settings )
    D_StartNetGame( settings, NIL )
    LoadGameSettings( settings )

    OutStd( "startskill " + hb_ntos( startskill ) + ;
        "  deathmatch: " + hb_ntos( deathmatch ) + ;
        "  startmap: " + hb_ntos( startmap ) + ;
        "  startepisode: " + hb_ntos( startepisode ) + hb_eol() )

    OutStd( "player " + hb_ntos( consoleplayer + 1 ) + ;
        " of " + hb_ntos( settings:num_players ) + ;
        " (" + hb_ntos( settings:num_players ) + " nodes)" + hb_eol() )

    IF timelimit > 0 .AND. deathmatch != 0
        IF timelimit == 20 .AND. M_CheckParm( "-avg" ) != 0
            OutStd( "Austin Virtual Gaming: Levels will end after 20 minutes" + hb_eol() )
        ELSE
            OutStd( "Levels will end after " + hb_ntos( timelimit ) + " minute" )
            IF timelimit > 1
                OutStd( "s" )
            ENDIF
            OutStd( "." + hb_eol() )
        ENDIF
    ENDIF
RETURN NIL
