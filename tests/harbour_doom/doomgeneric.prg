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

#include "doomgeneric.ch"

INIT PROCEDURE init_doomgeneric

    LOCAL i

    PUBLIC DG_ScreenBuffer
    PUBLIC myargc
    PUBLIC myargv

    DG_ScreenBuffer := NIL
    myargc := hb_argc() + 1
    myargv := {}
    FOR i := 0 TO hb_argc()
        AAdd( myargv, hb_argv( i ) )
    NEXT
RETURN

FUNCTION doomgeneric_Create()

    MEMVAR DG_ScreenBuffer
    M_FindResponseFile()

    DG_ScreenBuffer := DG_AllocScreen()

    DG_Init()

    D_DoomMain()
RETURN NIL
