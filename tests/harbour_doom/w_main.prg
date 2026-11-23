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

#include "w_main.ch"
#include "doomfeatures.ch"


FUNCTION W_ParseCommandLine()
    LOCAL modifiedgame := .F.
    LOCAL p
    LOCAL filename
    MEMVAR myargc
    MEMVAR myargv

    p := M_CheckParmWithArgs( "-file", 1 )
    IF p != 0
        modifiedgame := .T.
        p++
        DO WHILE p != myargc .AND. Left( myargv[ p + 1 ], 1 ) != "-"
            filename := D_TryFindWADByName( myargv[ p + 1 ] )
            OutStd( " adding " + filename + hb_eol() )
            W_AddFile( filename )
            p++
        ENDDO
    ENDIF
RETURN modifiedgame
