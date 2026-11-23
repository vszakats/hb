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
#include "d_items.ch"

#ifndef NUMWEAPONS
#define NUMWEAPONS 9
#endif

FUNCTION DEH_WeaponStart( oCtx, cLine )
    LOCAL nWep
    MEMVAR weaponinfo

    nWep := Int( Val( SubStr( cLine, 7 ) ) )
    IF nWep < 0 .OR. nWep >= NUMWEAPONS
        DEH_Warning( oCtx, "Invalid weapon number: " + hb_ntos( nWep ) )
        RETURN NIL
    ENDIF
RETURN weaponinfo[ nWep + 1 ]

FUNCTION DEH_WeaponParseLine( oCtx, cLine, oWep )
    LOCAL aAsg
    LOCAL nVal

    IF oWep == NIL
        RETURN NIL
    ENDIF
    aAsg := DEH_ParseAssignment( cLine )
    IF aAsg == NIL
        DEH_Warning( oCtx, "Failed to parse assignment" )
        RETURN NIL
    ENDIF
    nVal := Int( Val( aAsg[ 2 ] ) )
    IF hb_stricmp( aAsg[ 1 ], "Ammo type" ) == 0
        oWep:ammo := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Deselect frame" ) == 0
        oWep:upstate := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Select frame" ) == 0
        oWep:downstate := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Bobbing frame" ) == 0
        oWep:readystate := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Shooting frame" ) == 0
        oWep:atkstate := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Firing frame" ) == 0
        oWep:flashstate := nVal
    ELSE
        DEH_Warning( oCtx, "Field named '" + aAsg[ 1 ] + "' not found" )
    ENDIF
RETURN NIL
