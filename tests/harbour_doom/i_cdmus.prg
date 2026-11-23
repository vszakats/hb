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

STATIC cd_Error

#include "i_cdmus.ch"


INIT PROCEDURE init_i_cdmus


    cd_Error := 0
RETURN

FUNCTION I_CDMusInit()
RETURN 0

FUNCTION I_CDMusPrintStartup()
RETURN NIL

FUNCTION I_CDMusPlay( track )
RETURN 0

FUNCTION I_CDMusStop()
RETURN 0

FUNCTION I_CDMusResume()
RETURN 0

FUNCTION I_CDMusSetVolume( volume )
    cd_Error := 0
RETURN 0

FUNCTION I_CDMusFirstTrack()
RETURN 0

FUNCTION I_CDMusLastTrack()
RETURN 0

FUNCTION I_CDMusTrackLength( track_num )
RETURN 0
