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

FUNCTION DEH_GetSection( cName )
    IF hb_stricmp( cName, "Thing" ) == 0
        RETURN { "Thing", {| o, l | DEH_ThingStart( o, l ) }, {| o, l, t | DEH_ThingParseLine( o, l, t ) } }
    ENDIF
    IF hb_stricmp( cName, "Frame" ) == 0
        RETURN { "Frame", {| o, l | DEH_FrameStart( o, l ) }, {| o, l, t | DEH_FrameParseLine( o, l, t ) } }
    ENDIF
    IF hb_stricmp( cName, "Weapon" ) == 0
        RETURN { "Weapon", {| o, l | DEH_WeaponStart( o, l ) }, {| o, l, t | DEH_WeaponParseLine( o, l, t ) } }
    ENDIF
    IF hb_stricmp( cName, "Ammo" ) == 0
        RETURN { "Ammo", {| o, l | DEH_AmmoStart( o, l ) }, {| o, l, t | DEH_AmmoParseLine( o, l, t ) } }
    ENDIF
    IF hb_stricmp( cName, "Misc" ) == 0
        RETURN { "Misc", {| o, l | DEH_MiscStart( o, l ) }, {| o, l, t | DEH_MiscParseLine( o, l, t ) } }
    ENDIF
    IF hb_stricmp( cName, "Pointer" ) == 0
        RETURN { "Pointer", {| o, l | DEH_PointerStart( o, l ) }, {| o, l, t | DEH_PointerParseLine( o, l, t ) } }
    ENDIF
    IF hb_stricmp( cName, "Sound" ) == 0
        RETURN { "Sound", {| o, l | DEH_SoundStart( o, l ) }, {| o, l, t | DEH_SoundParseLine( o, l, t ) } }
    ENDIF
    IF hb_stricmp( cName, "Cheat" ) == 0
        RETURN { "Cheat", {| o, l | DEH_CheatStart( o, l ) }, {| o, l, t | DEH_CheatParseLine( o, l, t ) } }
    ENDIF
    IF hb_stricmp( cName, "Text" ) == 0
        RETURN { "Text", {| o, l | DEH_TextStart( o, l ) }, {| o, l, t | DEH_TextParseLine( o, l, t ) } }
    ENDIF
    IF hb_stricmp( cName, "[STRINGS]" ) == 0
        RETURN { "[STRINGS]", {| o, l | DEH_BEXStrStart( o, l ) }, {| o, l, t | DEH_BEXStrParseLine( o, l, t ) } }
    ENDIF
RETURN NIL
