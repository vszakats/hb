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

#include "net_server.ch"

FUNCTION NET_SV_Init()
RETURN NIL

FUNCTION NET_SV_Run()
RETURN NIL

FUNCTION NET_SV_Shutdown()
RETURN NIL

FUNCTION NET_SV_AddModule( module )
    HB_SYMBOL_UNUSED( module )
RETURN NIL

FUNCTION NET_SV_RegisterWithMaster()
RETURN NIL
