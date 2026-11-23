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

#include "d_ticcmd.ch"

CLASS ticcmd_t
    DATA forwardmove
    DATA sidemove
    DATA angleturn
    DATA chatchar
    DATA buttons
    DATA consistancy
    DATA buttons2
    DATA inventory
    DATA lookfly
    DATA arti
    METHOD New()
    METHOD CopyFrom()
ENDCLASS

METHOD New() CLASS ticcmd_t
    ::forwardmove := 0
    ::sidemove    := 0
    ::angleturn   := 0
    ::chatchar    := 0
    ::buttons     := 0
    ::consistancy := 0
    ::buttons2    := 0
    ::inventory   := 0
    ::lookfly     := 0
    ::arti        := 0
RETURN Self

METHOD CopyFrom( src ) CLASS ticcmd_t
    ::forwardmove := src:forwardmove
    ::sidemove    := src:sidemove
    ::angleturn   := src:angleturn
    ::chatchar    := src:chatchar
    ::buttons     := src:buttons
    ::consistancy := src:consistancy
    ::buttons2    := src:buttons2
    ::inventory   := src:inventory
    ::lookfly     := src:lookfly
    ::arti        := src:arti
RETURN Self
