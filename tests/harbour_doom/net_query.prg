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

#include "net_query.ch"

FUNCTION NET_StartLANQuery()
RETURN 0

FUNCTION NET_StartMasterQuery()
RETURN 0

FUNCTION NET_LANQuery()
RETURN NIL

FUNCTION NET_MasterQuery()
RETURN NIL

FUNCTION NET_QueryAddress( addr )
    HB_SYMBOL_UNUSED( addr )
RETURN NIL

FUNCTION NET_FindLANServer()
RETURN NIL

FUNCTION NET_Query_Poll( callback, user_data )
    HB_SYMBOL_UNUSED( callback )
    HB_SYMBOL_UNUSED( user_data )
RETURN 0

FUNCTION NET_Query_ResolveMaster( context )
    HB_SYMBOL_UNUSED( context )
RETURN NIL

FUNCTION NET_Query_AddToMaster( master_addr )
    HB_SYMBOL_UNUSED( master_addr )
RETURN NIL

FUNCTION NET_Query_CheckAddedToMaster( result )
    result := .F.
RETURN .F.

FUNCTION NET_Query_MasterResponse( packet )
    HB_SYMBOL_UNUSED( packet )
RETURN NIL
