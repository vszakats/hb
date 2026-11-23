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

#include "d_think.ch"

CLASS actionf_t
    DATA acv
    METHOD New()
    ACCESS acp1
    ASSIGN acp1
    ACCESS acp2
    ASSIGN acp2
ENDCLASS
CLASS thinker_t
    DATA prev
    DATA next
    DATA function
    DATA owner
    DATA thinkfn
    METHOD New()
ENDCLASS

METHOD New() CLASS actionf_t
    ::acv := NIL
RETURN Self

ACCESS acp1 CLASS actionf_t
RETURN ::acv

ASSIGN acp1( xBlock ) CLASS actionf_t
    ::acv := xBlock
RETURN ::acv

ACCESS acp2 CLASS actionf_t
RETURN ::acv

ASSIGN acp2( xBlock ) CLASS actionf_t
    ::acv := xBlock
RETURN ::acv

METHOD New() CLASS thinker_t
    ::prev := NIL
    ::next := NIL
    ::function := actionf_t():New()
    ::owner := NIL
    ::thinkfn := ""
RETURN Self
