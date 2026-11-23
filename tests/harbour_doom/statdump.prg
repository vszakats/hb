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

STATIC captured_stats := {}
STATIC num_captured_stats := 0

#include "statdump.ch"
#include "doomfeatures.ch"

#define MAX_CAPTURES 32


PROCEDURE StatCopy( stats )
    IF M_ParmExists( "-statdump" ) .AND. num_captured_stats < MAX_CAPTURES
        AAdd( captured_stats, stats )
        num_captured_stats++
    ENDIF
RETURN

PROCEDURE StatDump()
RETURN

INIT PROCEDURE init_statdump
    captured_stats := {}
    num_captured_stats := 0
RETURN
