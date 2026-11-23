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

STATIC FUNCTION DehMapSet( oObj, aMap, cName, nVal )
    LOCAL i

    FOR i := 1 TO Len( aMap )
        IF hb_stricmp( aMap[ i, 1 ], cName ) == 0
            oObj:&( aMap[ i, 2 ] ) := nVal
            RETURN .T.
        ENDIF
    NEXT
RETURN .F.

FUNCTION DEH_ThingStart( oCtx, cLine )
    LOCAL nThing
    MEMVAR mobjinfo

    nThing := Int( Val( SubStr( cLine, 6 ) ) ) - 1
    IF nThing < 0 .OR. nThing >= NUMMOBJTYPES
        DEH_Warning( oCtx, "Invalid thing number: " + hb_ntos( nThing + 1 ) )
        RETURN NIL
    ENDIF
RETURN mobjinfo[ nThing + 1 ]

FUNCTION DEH_ThingParseLine( oCtx, cLine, oMobj )
    LOCAL aAsg
    LOCAL aMap

    IF oMobj == NIL
        RETURN NIL
    ENDIF
    aAsg := DEH_ParseAssignment( cLine )
    IF aAsg == NIL
        DEH_Warning( oCtx, "Failed to parse assignment" )
        RETURN NIL
    ENDIF
    aMap := { ;
        { "ID #", "doomednum" }, ;
        { "Initial frame", "spawnstate" }, ;
        { "Hit points", "spawnhealth" }, ;
        { "First moving frame", "seestate" }, ;
        { "Alert sound", "seesound" }, ;
        { "Reaction time", "reactiontime" }, ;
        { "Attack sound", "attacksound" }, ;
        { "Injury frame", "painstate" }, ;
        { "Pain chance", "painchance" }, ;
        { "Pain sound", "painsound" }, ;
        { "Close attack frame", "meleestate" }, ;
        { "Far attack frame", "missilestate" }, ;
        { "Death frame", "deathstate" }, ;
        { "Exploding frame", "xdeathstate" }, ;
        { "Death sound", "deathsound" }, ;
        { "Speed", "speed" }, ;
        { "Width", "radius" }, ;
        { "Height", "height" }, ;
        { "Mass", "mass" }, ;
        { "Missile damage", "damage" }, ;
        { "Action sound", "activesound" }, ;
        { "Bits", "flags" }, ;
        { "Respawn frame", "raisestate" } }
    IF ! DehMapSet( oMobj, aMap, aAsg[ 1 ], Int( Val( aAsg[ 2 ] ) ) )
        DEH_Warning( oCtx, "Field named '" + aAsg[ 1 ] + "' not found" )
    ENDIF
RETURN NIL
