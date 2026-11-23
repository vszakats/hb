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

#include "net_loop.ch"

INIT PROCEDURE init_net_loop

    PUBLIC net_loop_client_module
    PUBLIC net_loop_server_module

    net_loop_client_module := net_module_t():New()
    net_loop_client_module:InitClient     := {|| .F. }
    net_loop_client_module:InitServer     := {|| .F. }
    net_loop_client_module:SendPacket     := {|addr, packet| HB_SYMBOL_UNUSED( addr ), HB_SYMBOL_UNUSED( packet ), NIL }
    net_loop_client_module:RecvPacket     := {|addr, packet| HB_SYMBOL_UNUSED( addr ), HB_SYMBOL_UNUSED( packet ), .F. }
    net_loop_client_module:AddrToString   := {|addr| HB_SYMBOL_UNUSED( addr ), "" }
    net_loop_client_module:FreeAddress    := {|addr| HB_SYMBOL_UNUSED( addr ), NIL }
    net_loop_client_module:ResolveAddress := {|address| HB_SYMBOL_UNUSED( address ), NIL }

    net_loop_server_module := net_module_t():New()
    net_loop_server_module:InitClient     := {|| .F. }
    net_loop_server_module:InitServer     := {|| .F. }
    net_loop_server_module:SendPacket     := {|addr, packet| HB_SYMBOL_UNUSED( addr ), HB_SYMBOL_UNUSED( packet ), NIL }
    net_loop_server_module:RecvPacket     := {|addr, packet| HB_SYMBOL_UNUSED( addr ), HB_SYMBOL_UNUSED( packet ), .F. }
    net_loop_server_module:AddrToString   := {|addr| HB_SYMBOL_UNUSED( addr ), "" }
    net_loop_server_module:FreeAddress    := {|addr| HB_SYMBOL_UNUSED( addr ), NIL }
    net_loop_server_module:ResolveAddress := {|address| HB_SYMBOL_UNUSED( address ), NIL }
RETURN
