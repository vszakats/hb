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

STATIC ticdata := {}
STATIC maketic
STATIC recvtic
STATIC localplayer
STATIC skiptics
STATIC new_sync
STATIC loop_interface
STATIC local_playeringame := {}
STATIC player_class
STATIC frameon
STATIC frameskip := {}
STATIC oldnettics
STATIC oldentertics
STATIC lasttime
STATIC offsetms

/* Original: doomfeatures.h, d_ticcmd.h, i_system.h, i_timer.h, i_video.h,
   m_argv.h, m_fixed.h, net_*.h — converter para .ch quando existirem */
#include "d_event.ch"
#include "d_ticcmd.ch"
#include "d_loop.ch"

CLASS loop_interface_t
    DATA ProcessEvents
    DATA BuildTiccmd
    DATA RunTic
    DATA RunMenu
    METHOD New()
ENDCLASS

CLASS ticcmd_set_t
    DATA cmds
    DATA ingame
    METHOD New()
ENDCLASS


METHOD New() CLASS loop_interface_t
    ::ProcessEvents := NIL
    ::BuildTiccmd   := NIL
    ::RunTic        := NIL
    ::RunMenu       := NIL
RETURN Self

METHOD New() CLASS ticcmd_set_t
    LOCAL i

    ::cmds := {}
    ::ingame := {}
    FOR i := 1 TO NET_MAXPLAYERS
        AAdd( ::cmds, ticcmd_t():New() )
        AAdd( ::ingame, .F. )
    NEXT
RETURN Self

INIT PROCEDURE init_d_loop
    LOCAL i

    PUBLIC singletics
    PUBLIC gametic
    PUBLIC ticdup
    PUBLIC drone
    PUBLIC net_client_connected

    ticdata := {}
    FOR i := 1 TO BACKUPTICS
        AAdd( ticdata, ticcmd_set_t():New() )
    NEXT

    local_playeringame := {}
    FOR i := 1 TO NET_MAXPLAYERS
        AAdd( local_playeringame, .F. )
    NEXT

    frameskip := {}
    FOR i := 1 TO 4
        AAdd( frameskip, 0 )
    NEXT

    maketic := 0
    recvtic := 0
    gametic := 0
    singletics := .F.
    localplayer := 0
    skiptics := 0
    ticdup := 0
    offsetms := 0
    new_sync := .T.
    loop_interface := NIL
    player_class := 0
    lasttime := 0
    frameon := 0
    oldnettics := 0
    oldentertics := 0
    drone := .F.
    net_client_connected := .F.
RETURN

STATIC FUNCTION IfaceCall( xFun, x1, x2 )
    LOCAL nArgs := PCount() - 1

    IF xFun == NIL
        RETURN NIL
    ENDIF

    IF ValType( xFun ) == "B"
        IF nArgs <= 0
            RETURN Eval( xFun )
        ELSEIF nArgs == 1
            RETURN Eval( xFun, x1 )
        ENDIF
        RETURN Eval( xFun, x1, x2 )
    ENDIF

    IF ValType( xFun ) == "C"
        IF nArgs <= 0
            RETURN &( xFun )()
        ELSEIF nArgs == 1
            RETURN &( xFun )( x1 )
        ENDIF
        RETURN &( xFun )( x1, x2 )
    ENDIF
RETURN NIL

STATIC FUNCTION GetAdjustedTime()
    LOCAL time_ms

    time_ms := I_GetTimeMS()

    IF new_sync
        time_ms := time_ms + Int( offsetms / FRACUNIT )
    ENDIF
RETURN Int( ( time_ms * TICRATE ) / 1000 )

STATIC FUNCTION BuildNewTic()
    LOCAL gameticdiv
    LOCAL cmd
    LOCAL nSlot
    MEMVAR drone
    MEMVAR gametic
    MEMVAR net_client_connected
    MEMVAR ticdup

    gameticdiv := Int( gametic / ticdup )

    I_StartTic()
    IfaceCall( loop_interface:ProcessEvents )
    IfaceCall( loop_interface:RunMenu )

    IF drone
        RETURN .F.
    ENDIF

    IF new_sync
        IF ! net_client_connected .AND. maketic - gameticdiv > 2
            RETURN .F.
        ENDIF
        IF maketic - gameticdiv > 8
            RETURN .F.
        ENDIF
    ELSE
        IF maketic - gameticdiv >= 5
            RETURN .F.
        ENDIF
    ENDIF

    cmd := ticcmd_t():New()
    IfaceCall( loop_interface:BuildTiccmd, cmd, maketic )

#ifdef FEATURE_MULTIPLAYER
    IF net_client_connected
        NET_CL_SendTiccmd( cmd, maketic )
    ENDIF
#endif

    nSlot := ( maketic % BACKUPTICS ) + 1
    ticdata[ nSlot ]:cmds[ localplayer + 1 ]:CopyFrom( cmd )
    ticdata[ nSlot ]:ingame[ localplayer + 1 ] := .T.

    maketic := maketic + 1
RETURN .T.

FUNCTION NetUpdate()
    LOCAL nowtime
    LOCAL newtics
    LOCAL i
    MEMVAR singletics
    MEMVAR ticdup

    IF singletics
        RETURN NIL
    ENDIF

#ifdef FEATURE_MULTIPLAYER
    NET_CL_Run()
    NET_SV_Run()
#endif

    nowtime := Int( GetAdjustedTime() / ticdup )
    newtics := nowtime - lasttime
    lasttime := nowtime

    IF skiptics <= newtics
        newtics := newtics - skiptics
        skiptics := 0
    ELSE
        skiptics := skiptics - newtics
        newtics := 0
    ENDIF

    FOR i := 0 TO newtics - 1
        IF ! BuildNewTic()
            EXIT
        ENDIF
    NEXT
RETURN NIL

STATIC FUNCTION D_Disconnected()
    MEMVAR drone
    IF drone
        I_Error( "Disconnected from server in drone mode." )
    ENDIF
    OutStd( "Disconnected from server." + hb_eol() )
RETURN NIL

FUNCTION D_ReceiveTic( ticcmds, players_mask )
    LOCAL i
    LOCAL nSlot
    MEMVAR drone

    IF ticcmds == NIL .AND. players_mask == NIL
        D_Disconnected()
        RETURN NIL
    ENDIF

    nSlot := ( recvtic % BACKUPTICS ) + 1

    FOR i := 0 TO NET_MAXPLAYERS - 1
        IF ! ( ! drone .AND. i == localplayer )
            ticdata[ nSlot ]:cmds[ i + 1 ]:CopyFrom( ticcmds[ i + 1 ] )
            ticdata[ nSlot ]:ingame[ i + 1 ] := players_mask[ i + 1 ]
        ENDIF
    NEXT

    recvtic := recvtic + 1
RETURN NIL

FUNCTION D_StartGameLoop()
    MEMVAR ticdup
    lasttime := Int( GetAdjustedTime() / ticdup )
RETURN NIL

#ifdef ORIGCODE
STATIC FUNCTION BlockUntilStart( settings, callback )
    MEMVAR net_client_connected
    MEMVAR net_client_wait_data
    WHILE ! NET_CL_GetSettings( settings )
        NET_CL_Run()
        NET_SV_Run()

        IF ! net_client_connected
            I_Error( "Lost connection to server" )
        ENDIF

        IF callback != NIL .AND. ! IfaceCall( callback, net_client_wait_data:ready_players, net_client_wait_data:num_players )
            I_Error( "Netgame startup aborted." )
        ENDIF

        I_Sleep( 100 )
    ENDDO
RETURN NIL
#endif

FUNCTION D_StartNetGame( settings, callback )
    MEMVAR drone
    MEMVAR myargv
    MEMVAR net_client_connected
    MEMVAR ticdup
#ifdef ORIGCODE
    LOCAL i

    offsetms := 0
    recvtic := 0

    settings:consoleplayer := 0
    settings:num_players := 1
    settings:player_classes[ 1 ] := player_class

    IF M_CheckParm( "-newsync" ) > 0
        settings:new_sync := 1
    ELSE
        settings:new_sync := 0
    ENDIF

    i := M_CheckParmWithArgs( "-extratics", 1 )
    IF i > 0
        settings:extratics := Int( Val( myargv[ i + 1 + 1 ] ) )
    ELSE
        settings:extratics := 1
    ENDIF

    i := M_CheckParmWithArgs( "-dup", 1 )
    IF i > 0
        settings:ticdup := Int( Val( myargv[ i + 1 + 1 ] ) )
    ELSE
        settings:ticdup := 1
    ENDIF

    IF net_client_connected
        NET_CL_StartGame( settings )
        BlockUntilStart( settings, callback )
        NET_CL_GetSettings( settings )
    ENDIF

    IF drone
        settings:consoleplayer := 0
    ENDIF

    localplayer := settings:consoleplayer

    FOR i := 0 TO NET_MAXPLAYERS - 1
        local_playeringame[ i + 1 ] := ( i < settings:num_players )
    NEXT

    ticdup := settings:ticdup
    new_sync := ( settings:new_sync != 0 )
#else
    HB_SYMBOL_UNUSED( callback )

    settings:consoleplayer := 0
    settings:num_players := 1
    settings:player_classes[ 1 ] := player_class
    settings:new_sync := 0
    settings:extratics := 1
    settings:ticdup := 1

    ticdup := settings:ticdup
    new_sync := ( settings:new_sync != 0 )
#endif
RETURN NIL

FUNCTION D_InitNetGame( connect_data )
    LOCAL result := .F.
    MEMVAR myargv
    MEMVAR net_loop_client_module
    MEMVAR net_loop_server_module
    MEMVAR net_sdl_module
#ifdef FEATURE_MULTIPLAYER
    LOCAL addr := NIL
    LOCAL i
#endif

    I_AtExit( {|| D_QuitNetGame() }, .T. )

    player_class := connect_data:player_class

#ifdef FEATURE_MULTIPLAYER
    IF M_CheckParm( "-server" ) > 0 .OR. M_CheckParm( "-privateserver" ) > 0
        NET_SV_Init()
        NET_SV_AddModule( net_loop_server_module )
        NET_SV_AddModule( net_sdl_module )
        NET_SV_RegisterWithMaster()

        IfaceCall( net_loop_client_module:InitClient )
        addr := IfaceCall( net_loop_client_module:ResolveAddress, NIL )
    ELSE
        i := M_CheckParm( "-autojoin" )
        IF i > 0
            addr := NET_FindLANServer()
            IF addr == NIL
                I_Error( "No server found on local LAN" )
            ENDIF
        ENDIF

        i := M_CheckParmWithArgs( "-connect", 1 )
        IF i > 0
            IfaceCall( net_sdl_module:InitClient )
            addr := IfaceCall( net_sdl_module:ResolveAddress, myargv[ i + 1 + 1 ] )
            IF addr == NIL
                I_Error( "Unable to resolve '" + myargv[ i + 1 + 1 ] + "'" + hb_eol() )
            ENDIF
        ENDIF
    ENDIF

    IF addr != NIL
        IF M_CheckParm( "-drone" ) > 0
            connect_data:drone := .T.
        ENDIF

        IF ! NET_CL_Connect( addr, connect_data )
            I_Error( "D_InitNetGame: Failed to connect to " + NET_AddrToString( addr ) + hb_eol() )
        ENDIF

        OutStd( "D_InitNetGame: Connected to " + NET_AddrToString( addr ) + hb_eol() )

        NET_WaitForLaunch()
        result := .T.
    ENDIF
#endif
RETURN result

FUNCTION D_QuitNetGame()
#ifdef FEATURE_MULTIPLAYER
    NET_SV_Shutdown()
    NET_CL_Disconnect()
#endif
RETURN NIL

STATIC FUNCTION GetLowTic()
    LOCAL lowtic
    MEMVAR drone
    MEMVAR net_client_connected

    lowtic := maketic

#ifdef FEATURE_MULTIPLAYER
    IF net_client_connected
        IF drone .OR. recvtic < lowtic
            lowtic := recvtic
        ENDIF
    ENDIF
#endif
RETURN lowtic

STATIC FUNCTION OldNetSync()
    LOCAL i
    LOCAL keyplayer := -1

    frameon := frameon + 1

    FOR i := 0 TO NET_MAXPLAYERS - 1
        IF local_playeringame[ i + 1 ]
            keyplayer := i
            EXIT
        ENDIF
    NEXT

    IF keyplayer < 0
        RETURN NIL
    ENDIF

    IF localplayer != keyplayer
        IF maketic <= recvtic
            lasttime := lasttime - 1
        ENDIF

        frameskip[ ( frameon & 3 ) + 1 ] := iif( oldnettics > recvtic, 1, 0 )
        oldnettics := maketic

        IF frameskip[ 1 ] != 0 .AND. frameskip[ 2 ] != 0 .AND. frameskip[ 3 ] != 0 .AND. frameskip[ 4 ] != 0
            skiptics := 1
        ENDIF
    ENDIF
RETURN NIL

STATIC FUNCTION PlayersInGame()
    LOCAL result := .F.
    LOCAL i
    MEMVAR drone
    MEMVAR net_client_connected

    IF net_client_connected
        FOR i := 0 TO NET_MAXPLAYERS - 1
            result := result .OR. local_playeringame[ i + 1 ]
        NEXT
    ENDIF

    IF ! drone
        result := .T.
    ENDIF
RETURN result

STATIC FUNCTION TicdupSquash( set )
    LOCAL cmd
    LOCAL i

    FOR i := 0 TO NET_MAXPLAYERS - 1
        cmd := set:cmds[ i + 1 ]
        cmd:chatchar := 0
        IF ( ( cmd:buttons & BT_SPECIAL ) != 0 )
            cmd:buttons := 0
        ENDIF
    NEXT
RETURN NIL

STATIC FUNCTION SinglePlayerClear( set )
    LOCAL i

    FOR i := 0 TO NET_MAXPLAYERS - 1
        IF i != localplayer
            set:ingame[ i + 1 ] := .F.
        ENDIF
    NEXT
RETURN NIL

FUNCTION TryRunTics()
    LOCAL i
    LOCAL lowtic
    LOCAL entertic
    LOCAL realtics
    LOCAL availabletics
    LOCAL counts
    LOCAL set
    LOCAL nIng
    MEMVAR gametic
    MEMVAR net_client_connected
    MEMVAR singletics
    MEMVAR ticdup

    entertic := Int( I_GetTime() / ticdup )
    realtics := entertic - oldentertics
    oldentertics := entertic

    IF singletics
        BuildNewTic()
    ELSE
        NetUpdate()
    ENDIF

    lowtic := GetLowTic()
    availabletics := lowtic - Int( gametic / ticdup )

    IF new_sync
        counts := availabletics
    ELSE
        IF realtics < availabletics - 1
            counts := realtics + 1
        ELSEIF realtics < availabletics
            counts := realtics
        ELSE
            counts := availabletics
        ENDIF

        IF counts < 1
            counts := 1
        ENDIF

        IF net_client_connected
            OldNetSync()
        ENDIF
    ENDIF

    IF counts < 1
        counts := 1
    ENDIF

    DO WHILE ! PlayersInGame() .OR. lowtic < Int( gametic / ticdup ) + counts
        NetUpdate()
        lowtic := GetLowTic()

        IF lowtic < Int( gametic / ticdup )
            I_Error( "TryRunTics: lowtic < gametic" )
        ENDIF

        IF Int( I_GetTime() / ticdup ) - entertic > 0
            RETURN NIL
        ENDIF

        I_Sleep( 1 )
    ENDDO

    DO WHILE counts > 0
        counts := counts - 1

        IF ! PlayersInGame()
            RETURN NIL
        ENDIF

        set := ticdata[ ( Int( gametic / ticdup ) % BACKUPTICS ) + 1 ]

        IF ! net_client_connected
            SinglePlayerClear( set )
        ENDIF

        FOR i := 0 TO ticdup - 1
            IF Int( gametic / ticdup ) > lowtic
                I_Error( "gametic>lowtic" )
            ENDIF

            FOR nIng := 1 TO NET_MAXPLAYERS
                local_playeringame[ nIng ] := set:ingame[ nIng ]
            NEXT

            IfaceCall( loop_interface:RunTic, set:cmds, set:ingame )
            gametic := gametic + 1

            TicdupSquash( set )
        NEXT

        NetUpdate()
    ENDDO
RETURN NIL

FUNCTION D_RegisterLoopCallbacks( i )
    loop_interface := i
RETURN NIL
