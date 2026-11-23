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

#include "doomstat.ch"

INIT PROCEDURE init_doomstat

    PUBLIC gamemode
    PUBLIC gamemission
    PUBLIC gameversion
    PUBLIC gamedescription
    PUBLIC modifiedgame

    gamemode := indetermined
    gamemission := doom
    gameversion := exe_final2
    gamedescription := NIL
    modifiedgame := .F.
RETURN

FUNCTION LogicalGameMission()
    MEMVAR gamemission
    IF gamemission == pack_chex
        RETURN doom
    ENDIF
    IF gamemission == pack_hacx
        RETURN doom2
    ENDIF
RETURN gamemission
