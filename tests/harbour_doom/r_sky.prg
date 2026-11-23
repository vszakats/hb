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

#include "r_sky.ch"
#include "m_fixed.ch"


PROCEDURE R_InitSkyMap()
    MEMVAR skytexturemid
    skytexturemid := 100 * FRACUNIT
RETURN

INIT PROCEDURE init_r_sky
    PUBLIC skyflatnum
    PUBLIC skytexture
    PUBLIC skytexturemid

    skyflatnum := 0
    skytexture := 0
    skytexturemid := 0
RETURN
