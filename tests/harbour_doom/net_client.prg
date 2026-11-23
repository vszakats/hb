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

STATIC net_client_received_wait_data
STATIC net_waiting_for_launch
STATIC net_player_name
STATIC net_server_wad_sha1sum
STATIC net_server_deh_sha1sum
STATIC net_server_is_freedoom
STATIC net_local_wad_sha1sum
STATIC net_local_deh_sha1sum
STATIC net_local_is_freedoom

#include "net_client.ch"


STATIC FUNCTION Sha1Zeros()
    LOCAL a
    LOCAL i

    a := {}
    FOR i := 1 TO 20
        AAdd( a, 0 )
    NEXT
RETURN a

INIT PROCEDURE init_net_client

    PUBLIC net_client_connected
    PUBLIC net_client_wait_data
    PUBLIC drone

    net_client_connected := .F.
    net_client_received_wait_data := .F.
    net_client_wait_data := net_waitdata_t():New()
    net_waiting_for_launch := .F.
    net_player_name := NIL
    net_server_wad_sha1sum := Sha1Zeros()
    net_server_deh_sha1sum := Sha1Zeros()
    net_server_is_freedoom := 0
    net_local_wad_sha1sum := Sha1Zeros()
    net_local_deh_sha1sum := Sha1Zeros()
    net_local_is_freedoom := 0
    drone := .F.
RETURN

FUNCTION NET_CL_Connect( addr, data )
    HB_SYMBOL_UNUSED( addr )
    HB_SYMBOL_UNUSED( data )
RETURN .F.

FUNCTION NET_CL_Disconnect()
RETURN NIL

FUNCTION NET_CL_Run()
RETURN NIL

FUNCTION NET_CL_Init()
RETURN NIL

FUNCTION NET_CL_LaunchGame()
RETURN NIL

FUNCTION NET_CL_StartGame( settings )
    HB_SYMBOL_UNUSED( settings )
RETURN NIL

FUNCTION NET_CL_SendTiccmd( ticcmd, maketic )
    HB_SYMBOL_UNUSED( ticcmd )
    HB_SYMBOL_UNUSED( maketic )
RETURN NIL

FUNCTION NET_CL_GetSettings( settings )
    HB_SYMBOL_UNUSED( settings )
RETURN .F.

FUNCTION NET_Init()
RETURN NIL

FUNCTION NET_BindVariables()
RETURN NIL
