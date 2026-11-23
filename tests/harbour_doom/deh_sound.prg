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
#include "sounds.ch"

FUNCTION DEH_SoundStart( oCtx, cLine )
    LOCAL nSfx
    MEMVAR S_sfx

    nSfx := Int( Val( SubStr( cLine, 6 ) ) )
    IF nSfx < 0 .OR. nSfx >= NUMSFX
        DEH_Warning( oCtx, "Invalid sound number: " + hb_ntos( nSfx ) )
        RETURN NIL
    ENDIF
    IF nSfx >= DEH_VANILLA_NUMSFX
        DEH_Warning( oCtx, "Attempt to modify SFX " + hb_ntos( nSfx ) + ;
            ". This will cause problems in Vanilla dehacked." )
    ENDIF
RETURN S_sfx[ nSfx + 1 ]

FUNCTION DEH_SoundParseLine( oCtx, cLine, oSfx )
    LOCAL aAsg
    LOCAL nVal

    IF oSfx == NIL
        RETURN NIL
    ENDIF
    aAsg := DEH_ParseAssignment( cLine )
    IF aAsg == NIL
        DEH_Warning( oCtx, "Failed to parse assignment" )
        RETURN NIL
    ENDIF
    nVal := Int( Val( aAsg[ 2 ] ) )
    IF hb_stricmp( aAsg[ 1 ], "Value" ) == 0
        oSfx:priority := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Zero 2" ) == 0
        oSfx:pitch := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Zero 3" ) == 0
        oSfx:volume := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Neg. One 1" ) == 0
        oSfx:usefulness := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Neg. One 2" ) == 0
        oSfx:lumpnum := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Offset" ) == 0 .OR. ;
         hb_stricmp( aAsg[ 1 ], "Zero/One" ) == 0 .OR. ;
         hb_stricmp( aAsg[ 1 ], "Zero 1" ) == 0 .OR. ;
         hb_stricmp( aAsg[ 1 ], "Zero 4" ) == 0
        /* Offset/link not applied; vanilla exe-layout fields. */
    ELSE
        DEH_Warning( oCtx, "Field named '" + aAsg[ 1 ] + "' not found" )
    ENDIF
RETURN NIL
