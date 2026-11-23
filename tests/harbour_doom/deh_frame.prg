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
#include "info.ch"

STATIC FUNCTION DehFrameOverflow( oCtx, cName, nVal )
    MEMVAR weaponinfo

    IF hb_stricmp( cName, "Duration" ) == 0
        weaponinfo[ 1 ]:ammo := nVal
    ELSEIF hb_stricmp( cName, "Codep frame" ) == 0
        weaponinfo[ 1 ]:upstate := nVal
    ELSEIF hb_stricmp( cName, "Next frame" ) == 0
        weaponinfo[ 1 ]:downstate := nVal
    ELSEIF hb_stricmp( cName, "Unknown 1" ) == 0
        weaponinfo[ 1 ]:readystate := nVal
    ELSEIF hb_stricmp( cName, "Unknown 2" ) == 0
        weaponinfo[ 1 ]:atkstate := nVal
    ELSE
        DEH_Error( oCtx, "Unable to simulate frame overflow: field '" + cName + "'" )
    ENDIF
RETURN NIL

FUNCTION DEH_FrameStart( oCtx, cLine )
    LOCAL nFrame
    MEMVAR states

    nFrame := Int( Val( SubStr( cLine, 6 ) ) )
    IF nFrame < 0 .OR. nFrame >= NUMSTATES
        DEH_Warning( oCtx, "Invalid frame number: " + hb_ntos( nFrame ) )
        RETURN NIL
    ENDIF
    IF nFrame >= DEH_VANILLA_NUMSTATES
        DEH_Warning( oCtx, "Attempt to modify frame " + hb_ntos( nFrame ) + ;
            ": this will cause problems in Vanilla dehacked." )
    ENDIF
RETURN states[ nFrame + 1 ]

FUNCTION DEH_FrameParseLine( oCtx, cLine, oState )
    LOCAL aAsg
    LOCAL nVal
    MEMVAR states

    IF oState == NIL
        RETURN NIL
    ENDIF
    aAsg := DEH_ParseAssignment( cLine )
    IF aAsg == NIL
        DEH_Warning( oCtx, "Failed to parse assignment" )
        RETURN NIL
    ENDIF
    nVal := Int( Val( aAsg[ 2 ] ) )
    IF oState == states[ NUMSTATES ]
        DehFrameOverflow( oCtx, aAsg[ 1 ], nVal )
        RETURN NIL
    ENDIF
    IF hb_stricmp( aAsg[ 1 ], "Sprite number" ) == 0
        oState:sprite := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Sprite subnumber" ) == 0
        oState:frame := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Duration" ) == 0
        oState:tics := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Next frame" ) == 0
        oState:nextstate := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Unknown 1" ) == 0
        oState:misc1 := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Unknown 2" ) == 0
        oState:misc2 := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Codep frame" ) == 0
        DEH_Warning( oCtx, "Codep frame is not supported in Frame sections (use Pointer)" )
    ELSE
        DEH_Warning( oCtx, "Field named '" + aAsg[ 1 ] + "' not found" )
    ENDIF
RETURN NIL
