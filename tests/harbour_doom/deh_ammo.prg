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

#include "deh_main.ch"

#ifndef NUMAMMO
#define NUMAMMO 4
#endif

FUNCTION DEH_AmmoStart( oCtx, cLine )
    LOCAL nAmmo

    nAmmo := Int( Val( SubStr( cLine, 5 ) ) )
    IF nAmmo < 0 .OR. nAmmo >= NUMAMMO
        DEH_Warning( oCtx, "Invalid ammo number: " + hb_ntos( nAmmo ) )
        RETURN NIL
    ENDIF
RETURN nAmmo

FUNCTION DEH_AmmoParseLine( oCtx, cLine, nAmmo )
    LOCAL aAsg
    LOCAL nVal
    MEMVAR maxammo
    MEMVAR clipammo

    IF nAmmo == NIL
        RETURN NIL
    ENDIF
    aAsg := DEH_ParseAssignment( cLine )
    IF aAsg == NIL
        DEH_Warning( oCtx, "Failed to parse assignment" )
        RETURN NIL
    ENDIF
    nVal := Int( Val( aAsg[ 2 ] ) )
    IF hb_stricmp( aAsg[ 1 ], "Per ammo" ) == 0
        clipammo[ nAmmo + 1 ] := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Max ammo" ) == 0
        maxammo[ nAmmo + 1 ] := nVal
    ELSE
        DEH_Warning( oCtx, "Field named '" + aAsg[ 1 ] + "' not found" )
    ENDIF
RETURN NIL
