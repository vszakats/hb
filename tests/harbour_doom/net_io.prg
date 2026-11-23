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

STATIC net_broadcast_addr

#include "net_io.ch"


INIT PROCEDURE init_net_io


    net_broadcast_addr := net_addr_t():New()
RETURN

FUNCTION NET_NewContext()
    LOCAL oCtx

    oCtx := net_context_t():New()
    oCtx:mods := {}
    oCtx:num_mods := 0
RETURN oCtx

FUNCTION NET_AddModule( context, module )
    IF context == NIL .OR. module == NIL
        RETURN NIL
    ENDIF
    AAdd( context:mods, module )
    context:num_mods := Len( context:mods )
RETURN NIL

FUNCTION NET_SendPacket( addr, packet )
    HB_SYMBOL_UNUSED( addr )
    HB_SYMBOL_UNUSED( packet )
RETURN NIL

FUNCTION NET_SendBroadcast( context, packet )
    HB_SYMBOL_UNUSED( context )
    HB_SYMBOL_UNUSED( packet )
RETURN NIL

FUNCTION NET_RecvPacket( context, addr, packet )
    HB_SYMBOL_UNUSED( context )
    addr := NIL
    packet := NIL
RETURN .F.

FUNCTION NET_AddrToString( addr )
    HB_SYMBOL_UNUSED( addr )
RETURN ""

FUNCTION NET_FreeAddress( addr )
    HB_SYMBOL_UNUSED( addr )
RETURN NIL

FUNCTION NET_ResolveAddress( context, address )
    HB_SYMBOL_UNUSED( context )
    HB_SYMBOL_UNUSED( address )
RETURN NIL
