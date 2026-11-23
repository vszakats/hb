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

#include "m_bbox.ch"

FUNCTION M_ClearBox( box )
    box[ BOXTOP + 1 ]    := INT_MIN
    box[ BOXRIGHT + 1 ]  := INT_MIN
    box[ BOXBOTTOM + 1 ] := INT_MAX
    box[ BOXLEFT + 1 ]   := INT_MAX
RETURN NIL

FUNCTION M_AddToBox( box, x, y )
    IF x < box[ BOXLEFT + 1 ]
        box[ BOXLEFT + 1 ] := x
    ELSEIF x > box[ BOXRIGHT + 1 ]
        box[ BOXRIGHT + 1 ] := x
    ENDIF
    IF y < box[ BOXBOTTOM + 1 ]
        box[ BOXBOTTOM + 1 ] := y
    ELSEIF y > box[ BOXTOP + 1 ]
        box[ BOXTOP + 1 ] := y
    ENDIF
RETURN NIL
