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

#include "net_sdl.ch"

INIT PROCEDURE init_net_sdl

    PUBLIC net_sdl_module

    net_sdl_module := net_module_t():New()
    net_sdl_module:InitClient     := {|| .F. }
    net_sdl_module:InitServer     := {|| .F. }
    net_sdl_module:SendPacket     := {|addr, packet| HB_SYMBOL_UNUSED( addr ), HB_SYMBOL_UNUSED( packet ), NIL }
    net_sdl_module:RecvPacket     := {|addr, packet| HB_SYMBOL_UNUSED( addr ), HB_SYMBOL_UNUSED( packet ), .F. }
    net_sdl_module:AddrToString   := {|addr| HB_SYMBOL_UNUSED( addr ), "" }
    net_sdl_module:FreeAddress    := {|addr| HB_SYMBOL_UNUSED( addr ), NIL }
    net_sdl_module:ResolveAddress := {|address| HB_SYMBOL_UNUSED( address ), NIL }
RETURN
