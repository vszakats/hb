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
#include "fileio.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

STATIC temp_savegame_filename := NIL
STATIC savegamelength

#include "p_saveg.ch"
#include "p_local.ch"
#include "p_mobj.ch"
#include "p_ceilng.ch"
#include "p_doors.ch"
#include "p_floor.ch"
#include "p_plats.ch"
#include "p_lights.ch"
#include "doomstat.ch"
#include "d_player.ch"
#include "dstrings.ch"
#include "deh_str.ch"
#include "m_misc.ch"

/* thinkerclass_t */
#ifndef tc_end
#define tc_end  0
#define tc_mobj 1
#endif

/* specials_e — names prefixed so they do not clash with tc_end/tc_mobj */
#ifndef spec_tc_ceiling
#define spec_tc_ceiling     0
#define spec_tc_door        1
#define spec_tc_floor       2
#define spec_tc_plat        3
#define spec_tc_flash       4
#define spec_tc_strobe      5
#define spec_tc_glow        6
#define spec_tc_endspecials 7
#endif



INIT PROCEDURE init_p_saveg
    PUBLIC save_stream
    PUBLIC savegame_error

    save_stream := NIL
    savegamelength := 0
    savegame_error := .F.
    temp_savegame_filename := NIL
RETURN

FUNCTION P_TempSaveGameFile()

    MEMVAR savegamedir
    IF temp_savegame_filename == NIL
        temp_savegame_filename := M_StringJoin( savegamedir, "temp.dsg", NIL )
    ENDIF

RETURN temp_savegame_filename

FUNCTION P_SaveGameFile( slot )
    LOCAL filename
    LOCAL basename
    MEMVAR savegamedir

    basename := SAVEGAMENAME + hb_ntos( slot ) + ".dsg"
    filename := iif( savegamedir == NIL, "", savegamedir ) + basename

RETURN filename

STATIC FUNCTION saveg_read8()
    LOCAL cBuf
    LOCAL nRead
    MEMVAR save_stream
    MEMVAR savegame_error

    cBuf := Space( 1 )
    nRead := FRead( save_stream, @cBuf, 1 )
    IF nRead < 1
        IF ! savegame_error
            OutErr( "saveg_read8: Unexpected end of file while reading save game" + hb_eol() )
            savegame_error := .T.
        ENDIF
        RETURN 0
    ENDIF

RETURN ( Asc( cBuf ) & 0xFF )

STATIC FUNCTION SavegNum( value )
    SWITCH ValType( value )
    CASE "N"
        RETURN Int( value )
    CASE "L"
        RETURN iif( value, 1, 0 )
    ENDSWITCH
RETURN 0

STATIC PROCEDURE saveg_write8( value )
    LOCAL nWritten
    MEMVAR save_stream
    MEMVAR savegame_error

    value := SavegNum( value )
    nWritten := FWrite( save_stream, Chr( ( value & 0xFF ) ) )
    IF nWritten < 1
        IF ! savegame_error
            OutErr( "saveg_write8: Error while writing save game" + hb_eol() )
            savegame_error := .T.
        ENDIF
    ENDIF

RETURN

STATIC FUNCTION saveg_read16()
    LOCAL result

    result := saveg_read8()
    result := ( result | ( saveg_read8() * 256 ) )
    result := ( result & 0xFFFF )
    IF result >= 32768
        result := result - 65536
    ENDIF

RETURN result

STATIC PROCEDURE saveg_write16( value )
    LOCAL n

    value := SavegNum( value )
    n := ( value & 0xFFFF )
    IF n < 0
        n := n + 65536
    ENDIF
    saveg_write8( n % 256 )
    saveg_write8( Int( n / 256 ) % 256 )

RETURN

STATIC FUNCTION saveg_read32()
    LOCAL result

    result := saveg_read8()
    result := ( result | ( saveg_read8() * 256 ) )
    result := ( result | ( saveg_read8() * 65536 ) )
    result := ( result | ( saveg_read8() * 16777216 ) )
    result := ( result & 0xFFFFFFFF )
    IF result >= 2147483648
        result := result - 4294967296
    ENDIF

RETURN result

STATIC PROCEDURE saveg_write32( value )
    LOCAL n

    value := SavegNum( value )
    n := ( value & 0xFFFFFFFF )
    IF n < 0
        n := n + 4294967296
    ENDIF
    saveg_write8( n % 256 )
    saveg_write8( Int( n / 256 ) % 256 )
    saveg_write8( Int( n / 65536 ) % 256 )
    saveg_write8( Int( n / 16777216 ) % 256 )

RETURN

STATIC PROCEDURE saveg_read_pad()
    LOCAL pos
    LOCAL padding
    LOCAL i
    MEMVAR save_stream

    pos := FSeek( save_stream, 0, FS_RELATIVE )
    padding := ( ( 4 - ( pos & 3 ) ) & 3 )
    FOR i := 1 TO padding
        saveg_read8()
    NEXT

RETURN

STATIC PROCEDURE saveg_write_pad()
    LOCAL pos
    LOCAL padding
    LOCAL i
    MEMVAR save_stream

    pos := FSeek( save_stream, 0, FS_RELATIVE )
    padding := ( ( 4 - ( pos & 3 ) ) & 3 )
    FOR i := 1 TO padding
        saveg_write8( 0 )
    NEXT

RETURN

STATIC FUNCTION saveg_readp()
    LOCAL n

    n := saveg_read32()
    IF n == 0
        RETURN NIL
    ENDIF

RETURN n

STATIC PROCEDURE saveg_writep( p )

    IF ValType( p ) != "N"
        saveg_write32( 0 )
    ELSE
        saveg_write32( p )
    ENDIF

RETURN

STATIC FUNCTION saveg_read_enum()
RETURN saveg_read32()

STATIC PROCEDURE saveg_write_enum( value )
    saveg_write32( value )
RETURN

STATIC FUNCTION PlayerIndex( oPlayer )
    LOCAL i
    MEMVAR players

    IF oPlayer == NIL
        RETURN -1
    ENDIF
    IF __objHasMsg( oPlayer, "PLAYERINDEX" ) .AND. ValType( oPlayer:PlayerIndex ) == "N"
        RETURN oPlayer:PlayerIndex
    ENDIF
    FOR i := 1 TO MAXPLAYERS
        IF players[ i ] == oPlayer
            RETURN i - 1
        ENDIF
    NEXT

RETURN -1

STATIC FUNCTION StateIndex( oState, nHint )
    LOCAL i
    MEMVAR states

    IF oState == NIL
        RETURN 0
    ENDIF
    IF nHint != NIL .AND. ValType( nHint ) == "N" .AND. nHint >= 0 ;
       .AND. ValType( states ) == "A" .AND. nHint + 1 <= Len( states ) ;
       .AND. states[ nHint + 1 ] == oState
        RETURN nHint
    ENDIF
    IF ValType( states ) == "A"
        FOR i := 1 TO Len( states )
            IF states[ i ] == oState
                RETURN i - 1
            ENDIF
        NEXT
    ENDIF
    IF nHint != NIL .AND. ValType( nHint ) == "N"
        RETURN nHint
    ENDIF

RETURN 0

STATIC FUNCTION ThinkName( o )
    LOCAL x

    IF o == NIL
        RETURN ""
    ENDIF
    IF __objHasMsg( o, "THINKFN" ) .AND. ValType( o:thinkfn ) == "C" .AND. ! Empty( o:thinkfn )
        RETURN o:thinkfn
    ENDIF
    IF __objHasMsg( o, "THINKER" ) .AND. o:thinker != NIL
        IF __objHasMsg( o:thinker, "THINKFN" ) .AND. ValType( o:thinker:thinkfn ) == "C" ;
           .AND. ! Empty( o:thinker:thinkfn )
            RETURN o:thinker:thinkfn
        ENDIF
        IF o:thinker:function != NIL
            x := o:thinker:function:acp1
            IF ValType( x ) == "C"
                RETURN x
            ENDIF
        ENDIF
    ENDIF
    IF __objHasMsg( o, "FUNCTION" ) .AND. o:function != NIL .AND. ValType( o:function ) == "O"
        x := o:function:acp1
        IF ValType( x ) == "C"
            RETURN x
        ENDIF
    ENDIF

RETURN ""

STATIC FUNCTION ThinkerOwner( th )

    IF th == NIL
        RETURN NIL
    ENDIF
    IF __objHasMsg( th, "OWNER" ) .AND. th:owner != NIL
        RETURN th:owner
    ENDIF

RETURN th

STATIC FUNCTION SavegBool32( n )
RETURN ( n != 0 )

STATIC FUNCTION SavegIntBool( lVal )
RETURN iif( ValType( lVal ) == "L", iif( lVal, 1, 0 ), iif( Empty( lVal ), 0, 1 ) )

STATIC PROCEDURE BindMobjThinker( mobj )
    LOCAL oMo := mobj

    mobj:thinkfn := "P_MobjThinker"
    mobj:thinker:thinkfn := "P_MobjThinker"
    mobj:thinker:owner := mobj
    mobj:thinker:function:acp1 := {|| P_MobjThinker( oMo ) }

RETURN

STATIC PROCEDURE BindMoveCeiling( ceiling )
    LOCAL oCeil := ceiling

    ceiling:thinkfn := "T_MoveCeiling"
    ceiling:thinker:thinkfn := "T_MoveCeiling"
    ceiling:thinker:owner := ceiling
    ceiling:thinker:function:acp1 := {|| T_MoveCeiling( oCeil ) }

RETURN

STATIC PROCEDURE BindVerticalDoor( door )
    LOCAL oDoor := door

    door:thinkfn := "T_VerticalDoor"
    door:thinker:thinkfn := "T_VerticalDoor"
    door:thinker:owner := door
    door:thinker:function:acp1 := {|| T_VerticalDoor( oDoor ) }

RETURN

STATIC PROCEDURE BindMoveFloor( floor )
    LOCAL oFloor := floor

    floor:thinkfn := "T_MoveFloor"
    floor:thinker:thinkfn := "T_MoveFloor"
    floor:thinker:owner := floor
    floor:thinker:function:acp1 := {|| T_MoveFloor( oFloor ) }

RETURN

STATIC PROCEDURE BindPlatRaise( plat )
    LOCAL oPlat := plat

    plat:thinkfn := "T_PlatRaise"
    plat:thinker:thinkfn := "T_PlatRaise"
    plat:thinker:owner := plat
    plat:thinker:function:acp1 := {|| T_PlatRaise( oPlat ) }

RETURN

STATIC PROCEDURE BindLightFlash( flash )
    LOCAL oFlash := flash

    flash:thinkfn := "T_LightFlash"
    flash:thinker:thinkfn := "T_LightFlash"
    flash:thinker:owner := flash
    flash:thinker:function:acp1 := {|| T_LightFlash( oFlash ) }

RETURN

STATIC PROCEDURE BindStrobeFlash( strobe )
    LOCAL oStrobe := strobe

    strobe:thinkfn := "T_StrobeFlash"
    strobe:thinker:thinkfn := "T_StrobeFlash"
    strobe:thinker:owner := strobe
    strobe:thinker:function:acp1 := {|| T_StrobeFlash( oStrobe ) }

RETURN

STATIC PROCEDURE BindGlow( glow )
    LOCAL oGlow := glow

    glow:thinkfn := "T_Glow"
    glow:thinker:thinkfn := "T_Glow"
    glow:thinker:owner := glow
    glow:thinker:function:acp1 := {|| T_Glow( oGlow ) }

RETURN

STATIC PROCEDURE saveg_read_mapthing_t( str )

    str:x := saveg_read16()
    str:y := saveg_read16()
    str:angle := saveg_read16()
    str:type := saveg_read16()
    str:options := saveg_read16()

RETURN

STATIC PROCEDURE saveg_write_mapthing_t( str )

    saveg_write16( str:x )
    saveg_write16( str:y )
    saveg_write16( str:angle )
    saveg_write16( str:type )
    saveg_write16( str:options )

RETURN

STATIC PROCEDURE saveg_read_actionf_t( str )

    str:acp1 := saveg_readp()

RETURN

STATIC PROCEDURE saveg_write_actionf_t( str )

    saveg_writep( str:acp1 )

RETURN

STATIC PROCEDURE saveg_read_think_t( str )
    saveg_read_actionf_t( str )
RETURN

STATIC PROCEDURE saveg_write_think_t( str )
    saveg_write_actionf_t( str )
RETURN

STATIC PROCEDURE saveg_read_thinker_t( str )

    str:prev := NIL
    saveg_readp()
    str:next := NIL
    saveg_readp()
    saveg_read_think_t( str:function )

RETURN

STATIC PROCEDURE saveg_write_thinker_t( str )

    saveg_write32( 0 )
    saveg_write32( 0 )
    saveg_write_think_t( str:function )

RETURN

STATIC PROCEDURE saveg_read_mobj_t( str )
    LOCAL pl
    LOCAL nState
    MEMVAR players
    MEMVAR states

    saveg_read_thinker_t( str:thinker )

    str:x := saveg_read32()
    str:y := saveg_read32()
    str:z := saveg_read32()

    str:snext := NIL
    saveg_readp()
    str:sprev := NIL
    saveg_readp()

    str:angle := saveg_read32()
    str:sprite := saveg_read_enum()
    str:frame := saveg_read32()

    str:bnext := NIL
    saveg_readp()
    str:bprev := NIL
    saveg_readp()

    str:subsector := NIL
    saveg_readp()

    str:floorz := saveg_read32()
    str:ceilingz := saveg_read32()
    str:radius := saveg_read32()
    str:height := saveg_read32()
    str:momx := saveg_read32()
    str:momy := saveg_read32()
    str:momz := saveg_read32()
    str:validcount := saveg_read32()
    str:type := saveg_read_enum()

    str:info := NIL
    saveg_readp()

    str:tics := saveg_read32()

    nState := saveg_read32()
    str:iState := nState
    IF ValType( states ) == "A" .AND. nState >= 0 .AND. nState + 1 <= Len( states )
        str:state := states[ nState + 1 ]
    ELSE
        str:state := NIL
    ENDIF

    str:flags := saveg_read32()
    str:health := saveg_read32()
    str:movedir := saveg_read32()
    str:movecount := saveg_read32()

    str:target := NIL
    saveg_readp()

    str:reactiontime := saveg_read32()
    str:threshold := saveg_read32()

    pl := saveg_read32()
    IF pl > 0
        str:player := players[ pl ]
        str:player:mo := str
    ELSE
        str:player := NIL
    ENDIF

    str:lastlook := saveg_read32()

    IF str:spawnpoint == NIL
        str:spawnpoint := mapthing_t():New()
    ENDIF
    saveg_read_mapthing_t( str:spawnpoint )

    str:tracer := NIL
    saveg_readp()

RETURN

STATIC PROCEDURE saveg_write_mobj_t( str )

    saveg_write_thinker_t( str:thinker )

    saveg_write32( str:x )
    saveg_write32( str:y )
    saveg_write32( str:z )

    saveg_writep( str:snext )
    saveg_writep( str:sprev )

    saveg_write32( str:angle )
    saveg_write_enum( str:sprite )
    saveg_write32( str:frame )

    saveg_writep( str:bnext )
    saveg_writep( str:bprev )
    saveg_writep( str:subsector )

    saveg_write32( str:floorz )
    saveg_write32( str:ceilingz )
    saveg_write32( str:radius )
    saveg_write32( str:height )
    saveg_write32( str:momx )
    saveg_write32( str:momy )
    saveg_write32( str:momz )
    saveg_write32( str:validcount )
    saveg_write_enum( str:type )
    saveg_writep( str:info )
    saveg_write32( str:tics )

    saveg_write32( StateIndex( str:state, str:iState ) )

    saveg_write32( str:flags )
    saveg_write32( str:health )
    saveg_write32( str:movedir )
    saveg_write32( str:movecount )
    saveg_writep( str:target )
    saveg_write32( str:reactiontime )
    saveg_write32( str:threshold )

    IF str:player != NIL
        saveg_write32( PlayerIndex( str:player ) + 1 )
    ELSE
        saveg_write32( 0 )
    ENDIF

    saveg_write32( str:lastlook )
    saveg_write_mapthing_t( str:spawnpoint )
    saveg_writep( str:tracer )

RETURN

STATIC PROCEDURE saveg_read_ticcmd_t( str )
    LOCAL n

    n := saveg_read8()
    IF n >= 128
        n := n - 256
    ENDIF
    str:forwardmove := n

    n := saveg_read8()
    IF n >= 128
        n := n - 256
    ENDIF
    str:sidemove := n

    str:angleturn := saveg_read16()
    str:consistancy := saveg_read16()
    str:chatchar := saveg_read8()
    str:buttons := saveg_read8()

RETURN

STATIC PROCEDURE saveg_write_ticcmd_t( str )

    saveg_write8( str:forwardmove )
    saveg_write8( str:sidemove )
    saveg_write16( str:angleturn )
    saveg_write16( str:consistancy )
    saveg_write8( str:chatchar )
    saveg_write8( str:buttons )

RETURN

STATIC PROCEDURE saveg_read_pspdef_t( str )
    LOCAL state
    MEMVAR states

    state := saveg_read32()
    IF state > 0 .AND. ValType( states ) == "A" .AND. state + 1 <= Len( states )
        str:state := states[ state + 1 ]
    ELSE
        str:state := NIL
    ENDIF

    str:tics := saveg_read32()
    str:sx := saveg_read32()
    str:sy := saveg_read32()

RETURN

STATIC PROCEDURE saveg_write_pspdef_t( str )

    IF str:state != NIL
        saveg_write32( StateIndex( str:state, NIL ) )
    ELSE
        saveg_write32( 0 )
    ENDIF

    saveg_write32( str:tics )
    saveg_write32( str:sx )
    saveg_write32( str:sy )

RETURN

STATIC PROCEDURE saveg_read_player_t( str )
    LOCAL i

    str:mo := NIL
    saveg_readp()

    str:playerstate := saveg_read_enum()
    saveg_read_ticcmd_t( str:cmd )

    str:viewz := saveg_read32()
    str:viewheight := saveg_read32()
    str:deltaviewheight := saveg_read32()
    str:bob := saveg_read32()
    str:health := saveg_read32()
    str:armorpoints := saveg_read32()
    str:armortype := saveg_read32()

    FOR i := 0 TO NUMPOWERS - 1
        str:powers[ i + 1 ] := saveg_read32()
    NEXT

    FOR i := 0 TO NUMCARDS - 1
        str:cards[ i + 1 ] := SavegBool32( saveg_read32() )
    NEXT

    str:backpack := SavegBool32( saveg_read32() )

    FOR i := 0 TO MAXPLAYERS - 1
        str:frags[ i + 1 ] := saveg_read32()
    NEXT

    str:readyweapon := saveg_read_enum()
    str:pendingweapon := saveg_read_enum()

    FOR i := 0 TO NUMWEAPONS - 1
        str:weaponowned[ i + 1 ] := SavegBool32( saveg_read32() )
    NEXT

    FOR i := 0 TO NUMAMMO - 1
        str:ammo[ i + 1 ] := saveg_read32()
    NEXT

    FOR i := 0 TO NUMAMMO - 1
        str:maxammo[ i + 1 ] := saveg_read32()
    NEXT

    str:attackdown := SavegBool32( saveg_read32() )
    str:usedown := SavegBool32( saveg_read32() )
    str:cheats := saveg_read32()
    str:refire := saveg_read32()
    str:killcount := saveg_read32()
    str:itemcount := saveg_read32()
    str:secretcount := saveg_read32()

    str:message := NIL
    saveg_readp()

    str:damagecount := saveg_read32()
    str:bonuscount := saveg_read32()

    str:attacker := NIL
    saveg_readp()

    str:extralight := saveg_read32()
    str:fixedcolormap := saveg_read32()
    str:colormap := saveg_read32()

    FOR i := 0 TO NUMPSPRITES - 1
        saveg_read_pspdef_t( str:psprites[ i + 1 ] )
    NEXT

    str:didsecret := SavegBool32( saveg_read32() )

RETURN

STATIC PROCEDURE saveg_write_player_t( str )
    LOCAL i

    saveg_writep( str:mo )
    saveg_write_enum( str:playerstate )
    saveg_write_ticcmd_t( str:cmd )

    saveg_write32( str:viewz )
    saveg_write32( str:viewheight )
    saveg_write32( str:deltaviewheight )
    saveg_write32( str:bob )
    saveg_write32( str:health )
    saveg_write32( str:armorpoints )
    saveg_write32( str:armortype )

    FOR i := 0 TO NUMPOWERS - 1
        saveg_write32( str:powers[ i + 1 ] )
    NEXT

    FOR i := 0 TO NUMCARDS - 1
        saveg_write32( SavegIntBool( str:cards[ i + 1 ] ) )
    NEXT

    saveg_write32( SavegIntBool( str:backpack ) )

    FOR i := 0 TO MAXPLAYERS - 1
        saveg_write32( str:frags[ i + 1 ] )
    NEXT

    saveg_write_enum( str:readyweapon )
    saveg_write_enum( str:pendingweapon )

    FOR i := 0 TO NUMWEAPONS - 1
        saveg_write32( SavegIntBool( str:weaponowned[ i + 1 ] ) )
    NEXT

    FOR i := 0 TO NUMAMMO - 1
        saveg_write32( str:ammo[ i + 1 ] )
    NEXT

    FOR i := 0 TO NUMAMMO - 1
        saveg_write32( str:maxammo[ i + 1 ] )
    NEXT

    saveg_write32( str:attackdown )
    saveg_write32( str:usedown )
    saveg_write32( str:cheats )
    saveg_write32( str:refire )
    saveg_write32( str:killcount )
    saveg_write32( str:itemcount )
    saveg_write32( str:secretcount )
    saveg_writep( str:message )
    saveg_write32( str:damagecount )
    saveg_write32( str:bonuscount )
    saveg_writep( str:attacker )
    saveg_write32( str:extralight )
    saveg_write32( str:fixedcolormap )
    saveg_write32( str:colormap )

    FOR i := 0 TO NUMPSPRITES - 1
        saveg_write_pspdef_t( str:psprites[ i + 1 ] )
    NEXT

    saveg_write32( SavegIntBool( str:didsecret ) )

RETURN

STATIC PROCEDURE saveg_read_ceiling_t( str )
    LOCAL sector
    MEMVAR sectors

    saveg_read_thinker_t( str:thinker )
    str:type := saveg_read_enum()

    sector := saveg_read32()
    str:sector := sectors[ sector + 1 ]

    str:bottomheight := saveg_read32()
    str:topheight := saveg_read32()
    str:speed := saveg_read32()
    str:crush := SavegBool32( saveg_read32() )
    str:direction := saveg_read32()
    str:tag := saveg_read32()
    str:olddirection := saveg_read32()

RETURN

STATIC PROCEDURE saveg_write_ceiling_t( str )

    saveg_write_thinker_t( str:thinker )
    saveg_write_enum( str:type )
    saveg_write32( str:sector:iSector )
    saveg_write32( str:bottomheight )
    saveg_write32( str:topheight )
    saveg_write32( str:speed )
    saveg_write32( SavegIntBool( str:crush ) )
    saveg_write32( str:direction )
    saveg_write32( str:tag )
    saveg_write32( str:olddirection )

RETURN

STATIC PROCEDURE saveg_read_vldoor_t( str )
    LOCAL sector
    MEMVAR sectors

    saveg_read_thinker_t( str:thinker )
    str:type := saveg_read_enum()

    sector := saveg_read32()
    str:sector := sectors[ sector + 1 ]

    str:topheight := saveg_read32()
    str:speed := saveg_read32()
    str:direction := saveg_read32()
    str:topwait := saveg_read32()
    str:topcountdown := saveg_read32()

RETURN

STATIC PROCEDURE saveg_write_vldoor_t( str )

    saveg_write_thinker_t( str:thinker )
    saveg_write_enum( str:type )
    saveg_write32( str:sector:iSector )
    saveg_write32( str:topheight )
    saveg_write32( str:speed )
    saveg_write32( str:direction )
    saveg_write32( str:topwait )
    saveg_write32( str:topcountdown )

RETURN

STATIC PROCEDURE saveg_read_floormove_t( str )
    LOCAL sector
    MEMVAR sectors

    saveg_read_thinker_t( str:thinker )
    str:type := saveg_read_enum()
    str:crush := SavegBool32( saveg_read32() )

    sector := saveg_read32()
    str:sector := sectors[ sector + 1 ]

    str:direction := saveg_read32()
    str:newspecial := saveg_read32()
    str:texture := saveg_read16()
    str:floordestheight := saveg_read32()
    str:speed := saveg_read32()

RETURN

STATIC PROCEDURE saveg_write_floormove_t( str )

    saveg_write_thinker_t( str:thinker )
    saveg_write_enum( str:type )
    saveg_write32( SavegIntBool( str:crush ) )
    saveg_write32( str:sector:iSector )
    saveg_write32( str:direction )
    saveg_write32( str:newspecial )
    saveg_write16( str:texture )
    saveg_write32( str:floordestheight )
    saveg_write32( str:speed )

RETURN

STATIC PROCEDURE saveg_read_plat_t( str )
    LOCAL sector
    MEMVAR sectors

    saveg_read_thinker_t( str:thinker )

    sector := saveg_read32()
    str:sector := sectors[ sector + 1 ]

    str:speed := saveg_read32()
    str:low := saveg_read32()
    str:high := saveg_read32()
    str:wait := saveg_read32()
    str:count := saveg_read32()
    str:status := saveg_read_enum()
    str:oldstatus := saveg_read_enum()
    str:crush := SavegBool32( saveg_read32() )
    str:tag := saveg_read32()
    str:type := saveg_read_enum()

RETURN

STATIC PROCEDURE saveg_write_plat_t( str )

    saveg_write_thinker_t( str:thinker )
    saveg_write32( str:sector:iSector )
    saveg_write32( str:speed )
    saveg_write32( str:low )
    saveg_write32( str:high )
    saveg_write32( str:wait )
    saveg_write32( str:count )
    saveg_write_enum( str:status )
    saveg_write_enum( str:oldstatus )
    saveg_write32( SavegIntBool( str:crush ) )
    saveg_write32( str:tag )
    saveg_write_enum( str:type )

RETURN

STATIC PROCEDURE saveg_read_lightflash_t( str )
    LOCAL sector
    MEMVAR sectors

    saveg_read_thinker_t( str:thinker )

    sector := saveg_read32()
    str:sector := sectors[ sector + 1 ]

    str:count := saveg_read32()
    str:maxlight := saveg_read32()
    str:minlight := saveg_read32()
    str:maxtime := saveg_read32()
    str:mintime := saveg_read32()

RETURN

STATIC PROCEDURE saveg_write_lightflash_t( str )

    saveg_write_thinker_t( str:thinker )
    saveg_write32( str:sector:iSector )
    saveg_write32( str:count )
    saveg_write32( str:maxlight )
    saveg_write32( str:minlight )
    saveg_write32( str:maxtime )
    saveg_write32( str:mintime )

RETURN

STATIC PROCEDURE saveg_read_strobe_t( str )
    LOCAL sector
    MEMVAR sectors

    saveg_read_thinker_t( str:thinker )

    sector := saveg_read32()
    str:sector := sectors[ sector + 1 ]

    str:count := saveg_read32()
    str:minlight := saveg_read32()
    str:maxlight := saveg_read32()
    str:darktime := saveg_read32()
    str:brighttime := saveg_read32()

RETURN

STATIC PROCEDURE saveg_write_strobe_t( str )

    saveg_write_thinker_t( str:thinker )
    saveg_write32( str:sector:iSector )
    saveg_write32( str:count )
    saveg_write32( str:minlight )
    saveg_write32( str:maxlight )
    saveg_write32( str:darktime )
    saveg_write32( str:brighttime )

RETURN

STATIC PROCEDURE saveg_read_glow_t( str )
    LOCAL sector
    MEMVAR sectors

    saveg_read_thinker_t( str:thinker )

    sector := saveg_read32()
    str:sector := sectors[ sector + 1 ]

    str:minlight := saveg_read32()
    str:maxlight := saveg_read32()
    str:direction := saveg_read32()

RETURN

STATIC PROCEDURE saveg_write_glow_t( str )

    saveg_write_thinker_t( str:thinker )
    saveg_write32( str:sector:iSector )
    saveg_write32( str:minlight )
    saveg_write32( str:maxlight )
    saveg_write32( str:direction )

RETURN

FUNCTION P_WriteSaveGameHeader( description )
    LOCAL name
    LOCAL i
    LOCAL cDesc
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gameskill
    MEMVAR leveltime
    MEMVAR playeringame

    cDesc := iif( description == NIL, "", description )
    i := 1
    DO WHILE i <= Len( cDesc ) .AND. i <= SAVESTRINGSIZE
        saveg_write8( Asc( SubStr( cDesc, i, 1 ) ) )
        i := i + 1
    ENDDO
    DO WHILE i <= SAVESTRINGSIZE
        saveg_write8( 0 )
        i := i + 1
    ENDDO

    name := ""
    M_snprintf( @name, VERSIONSIZE, "version %i", G_VanillaVersionCode() )
    FOR i := 1 TO VERSIONSIZE
        IF i <= Len( name )
            saveg_write8( Asc( SubStr( name, i, 1 ) ) )
        ELSE
            saveg_write8( 0 )
        ENDIF
    NEXT

    saveg_write8( gameskill )
    saveg_write8( gameepisode )
    saveg_write8( gamemap )

    FOR i := 0 TO MAXPLAYERS - 1
        saveg_write8( iif( playeringame[ i + 1 ], 1, 0 ) )
    NEXT

    saveg_write8( ( Int( leveltime / 65536 ) & 0xFF ) )
    saveg_write8( ( Int( leveltime / 256 ) & 0xFF ) )
    saveg_write8( ( leveltime & 0xFF ) )

RETURN NIL

FUNCTION P_ReadSaveGameHeader()
    LOCAL i
    LOCAL a
    LOCAL b
    LOCAL c
    LOCAL vcheck
    LOCAL read_vcheck
    LOCAL nZ
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gameskill
    MEMVAR leveltime
    MEMVAR playeringame

    FOR i := 1 TO SAVESTRINGSIZE
        saveg_read8()
    NEXT

    read_vcheck := ""
    FOR i := 1 TO VERSIONSIZE
        read_vcheck += Chr( saveg_read8() )
    NEXT

    vcheck := ""
    M_snprintf( @vcheck, VERSIONSIZE, "version %i", G_VanillaVersionCode() )
    nZ := At( Chr( 0 ), read_vcheck )
    IF nZ > 0
        read_vcheck := Left( read_vcheck, nZ - 1 )
    ENDIF
    IF read_vcheck != vcheck
        RETURN .F.
    ENDIF

    gameskill := saveg_read8()
    gameepisode := saveg_read8()
    gamemap := saveg_read8()

    FOR i := 0 TO MAXPLAYERS - 1
        playeringame[ i + 1 ] := ( saveg_read8() != 0 )
    NEXT

    a := saveg_read8()
    b := saveg_read8()
    c := saveg_read8()
    leveltime := ( a * 65536 ) + ( b * 256 ) + c

RETURN .T.

FUNCTION P_ReadSaveGameEOF()
    LOCAL value

    value := saveg_read8()

RETURN value == SAVEGAME_EOF

FUNCTION P_WriteSaveGameEOF()

    saveg_write8( SAVEGAME_EOF )

RETURN NIL

FUNCTION P_ArchivePlayers()
    LOCAL i
    MEMVAR playeringame
    MEMVAR players

    FOR i := 0 TO MAXPLAYERS - 1
        IF ! playeringame[ i + 1 ]
            LOOP
        ENDIF
        saveg_write_pad()
        saveg_write_player_t( players[ i + 1 ] )
    NEXT

RETURN NIL

FUNCTION P_UnArchivePlayers()
    LOCAL i
    MEMVAR playeringame
    MEMVAR players

    FOR i := 0 TO MAXPLAYERS - 1
        IF ! playeringame[ i + 1 ]
            LOOP
        ENDIF
        saveg_read_pad()
        saveg_read_player_t( players[ i + 1 ] )
        players[ i + 1 ]:mo := NIL
        players[ i + 1 ]:message := NIL
        players[ i + 1 ]:attacker := NIL
    NEXT

RETURN NIL

FUNCTION P_ArchiveWorld()
    LOCAL i
    LOCAL j
    LOCAL sec
    LOCAL li
    LOCAL si
    MEMVAR lines
    MEMVAR numlines
    MEMVAR numsectors
    MEMVAR sectors
    MEMVAR sides

    FOR i := 0 TO numsectors - 1
        sec := sectors[ i + 1 ]
        saveg_write16( Int( sec:floorheight / FRACUNIT ) )
        saveg_write16( Int( sec:ceilingheight / FRACUNIT ) )
        saveg_write16( sec:floorpic )
        saveg_write16( sec:ceilingpic )
        saveg_write16( sec:lightlevel )
        saveg_write16( sec:special )
        saveg_write16( sec:tag )
    NEXT

    FOR i := 0 TO numlines - 1
        li := lines[ i + 1 ]
        saveg_write16( li:flags )
        saveg_write16( li:special )
        saveg_write16( li:tag )
        FOR j := 0 TO 1
            IF li:sidenum[ j + 1 ] == -1
                LOOP
            ENDIF
            si := sides[ li:sidenum[ j + 1 ] + 1 ]
            saveg_write16( Int( si:textureoffset / FRACUNIT ) )
            saveg_write16( Int( si:rowoffset / FRACUNIT ) )
            saveg_write16( si:toptexture )
            saveg_write16( si:bottomtexture )
            saveg_write16( si:midtexture )
        NEXT
    NEXT

RETURN NIL

FUNCTION P_UnArchiveWorld()
    LOCAL i
    LOCAL j
    LOCAL sec
    LOCAL li
    LOCAL si
    MEMVAR lines
    MEMVAR numlines
    MEMVAR numsectors
    MEMVAR sectors
    MEMVAR sides

    FOR i := 0 TO numsectors - 1
        sec := sectors[ i + 1 ]
        sec:floorheight := saveg_read16() * FRACUNIT
        sec:ceilingheight := saveg_read16() * FRACUNIT
        sec:floorpic := saveg_read16()
        sec:ceilingpic := saveg_read16()
        sec:lightlevel := saveg_read16()
        sec:special := saveg_read16()
        sec:tag := saveg_read16()
        sec:specialdata := NIL
        sec:soundtarget := NIL
    NEXT

    FOR i := 0 TO numlines - 1
        li := lines[ i + 1 ]
        li:flags := saveg_read16()
        li:special := saveg_read16()
        li:tag := saveg_read16()
        FOR j := 0 TO 1
            IF li:sidenum[ j + 1 ] == -1
                LOOP
            ENDIF
            si := sides[ li:sidenum[ j + 1 ] + 1 ]
            si:textureoffset := saveg_read16() * FRACUNIT
            si:rowoffset := saveg_read16() * FRACUNIT
            si:toptexture := saveg_read16()
            si:bottomtexture := saveg_read16()
            si:midtexture := saveg_read16()
        NEXT
    NEXT

RETURN NIL

FUNCTION P_ArchiveThinkers()
    LOCAL th
    LOCAL oMo
    MEMVAR thinkercap

    IF thinkercap != NIL
        th := thinkercap:next
        DO WHILE th != NIL .AND. !( th == thinkercap )
            IF ThinkName( th ) == "P_MobjThinker"
                oMo := ThinkerOwner( th )
                saveg_write8( tc_mobj )
                saveg_write_pad()
                saveg_write_mobj_t( oMo )
            ENDIF
            th := th:next
        ENDDO
    ENDIF

    saveg_write8( tc_end )

RETURN NIL

FUNCTION P_UnArchiveThinkers()
    LOCAL tclass
    LOCAL currentthinker
    LOCAL nNext
    LOCAL mobj
    LOCAL oMo
    MEMVAR mobjinfo
    MEMVAR thinkercap

    IF thinkercap != NIL
        currentthinker := thinkercap:next
        DO WHILE currentthinker != NIL .AND. !( currentthinker == thinkercap )
            nNext := currentthinker:next
            IF ThinkName( currentthinker ) == "P_MobjThinker"
                oMo := ThinkerOwner( currentthinker )
                IF oMo != NIL
                    P_RemoveMobj( oMo )
                ENDIF
            ENDIF
            currentthinker := nNext
        ENDDO
    ENDIF
    P_InitThinkers()

    DO WHILE .T.
        tclass := saveg_read8()
        SWITCH tclass
        CASE tc_end
            RETURN NIL

        CASE tc_mobj
            saveg_read_pad()
            mobj := mobj_t():New()
            IF mobj:thinker == NIL
                mobj:thinker := thinker_t():New()
            ENDIF
            saveg_read_mobj_t( mobj )
            mobj:target := NIL
            mobj:tracer := NIL
            P_SetThingPosition( mobj )
            mobj:info := mobjinfo[ mobj:type + 1 ]
            mobj:floorz := mobj:subsector:sector:floorheight
            mobj:ceilingz := mobj:subsector:sector:ceilingheight
            BindMobjThinker( mobj )
            P_AddThinker( mobj:thinker )
            EXIT

        OTHERWISE
            I_Error( "Unknown tclass " + hb_ntos( tclass ) + " in savegame" )
        ENDSWITCH
    ENDDO

RETURN NIL

FUNCTION P_ArchiveSpecials()
    LOCAL th
    LOCAL i
    LOCAL oObj
    LOCAL cName
    LOCAL lNullFn
    MEMVAR activeceilings
    MEMVAR thinkercap

    IF thinkercap == NIL
        saveg_write8( spec_tc_endspecials )
        RETURN NIL
    ENDIF

    th := thinkercap:next
    DO WHILE th != NIL .AND. !( th == thinkercap )
        cName := ThinkName( th )
        lNullFn := ( Empty( cName ) .AND. ( th:function == NIL .OR. th:function:acv == NIL ) )

        IF lNullFn
            FOR i := 0 TO MAXCEILINGS - 1
                IF activeceilings[ i + 1 ] != NIL ;
                   .AND. ( activeceilings[ i + 1 ] == th ;
                        .OR. activeceilings[ i + 1 ]:thinker == th )
                    EXIT
                ENDIF
            NEXT
            IF i < MAXCEILINGS
                oObj := activeceilings[ i + 1 ]
                saveg_write8( spec_tc_ceiling )
                saveg_write_pad()
                saveg_write_ceiling_t( oObj )
            ENDIF
            th := th:next
            LOOP
        ENDIF

        IF cName == "T_MoveCeiling"
            saveg_write8( spec_tc_ceiling )
            saveg_write_pad()
            saveg_write_ceiling_t( ThinkerOwner( th ) )
            th := th:next
            LOOP
        ENDIF

        IF cName == "T_VerticalDoor"
            saveg_write8( spec_tc_door )
            saveg_write_pad()
            saveg_write_vldoor_t( ThinkerOwner( th ) )
            th := th:next
            LOOP
        ENDIF

        IF cName == "T_MoveFloor"
            saveg_write8( spec_tc_floor )
            saveg_write_pad()
            saveg_write_floormove_t( ThinkerOwner( th ) )
            th := th:next
            LOOP
        ENDIF

        IF cName == "T_PlatRaise"
            saveg_write8( spec_tc_plat )
            saveg_write_pad()
            saveg_write_plat_t( ThinkerOwner( th ) )
            th := th:next
            LOOP
        ENDIF

        IF cName == "T_LightFlash"
            saveg_write8( spec_tc_flash )
            saveg_write_pad()
            saveg_write_lightflash_t( ThinkerOwner( th ) )
            th := th:next
            LOOP
        ENDIF

        IF cName == "T_StrobeFlash"
            saveg_write8( spec_tc_strobe )
            saveg_write_pad()
            saveg_write_strobe_t( ThinkerOwner( th ) )
            th := th:next
            LOOP
        ENDIF

        IF cName == "T_Glow"
            saveg_write8( spec_tc_glow )
            saveg_write_pad()
            saveg_write_glow_t( ThinkerOwner( th ) )
            th := th:next
            LOOP
        ENDIF

        th := th:next
    ENDDO

    saveg_write8( spec_tc_endspecials )

RETURN NIL

FUNCTION P_UnArchiveSpecials()
    LOCAL tclass
    LOCAL ceiling
    LOCAL door
    LOCAL floor
    LOCAL plat
    LOCAL flash
    LOCAL strobe
    LOCAL glow

    DO WHILE .T.
        tclass := saveg_read8()

        SWITCH tclass
        CASE spec_tc_endspecials
            RETURN NIL

        CASE spec_tc_ceiling
            saveg_read_pad()
            ceiling := ceiling_t():New()
            saveg_read_ceiling_t( ceiling )
            ceiling:sector:specialdata := ceiling
            IF ceiling:thinker:function:acp1 != NIL
                BindMoveCeiling( ceiling )
            ENDIF
            P_AddThinker( ceiling:thinker )
            P_AddActiveCeiling( ceiling )
            EXIT

        CASE spec_tc_door
            saveg_read_pad()
            door := vldoor_t():New()
            saveg_read_vldoor_t( door )
            door:sector:specialdata := door
            BindVerticalDoor( door )
            P_AddThinker( door:thinker )
            EXIT

        CASE spec_tc_floor
            saveg_read_pad()
            floor := floormove_t():New()
            saveg_read_floormove_t( floor )
            floor:sector:specialdata := floor
            BindMoveFloor( floor )
            P_AddThinker( floor:thinker )
            EXIT

        CASE spec_tc_plat
            saveg_read_pad()
            plat := plat_t():New()
            saveg_read_plat_t( plat )
            plat:sector:specialdata := plat
            IF plat:thinker:function:acp1 != NIL
                BindPlatRaise( plat )
            ENDIF
            P_AddThinker( plat:thinker )
            P_AddActivePlat( plat )
            EXIT

        CASE spec_tc_flash
            saveg_read_pad()
            flash := lightflash_t():New()
            saveg_read_lightflash_t( flash )
            BindLightFlash( flash )
            P_AddThinker( flash:thinker )
            EXIT

        CASE spec_tc_strobe
            saveg_read_pad()
            strobe := strobe_t():New()
            saveg_read_strobe_t( strobe )
            BindStrobeFlash( strobe )
            P_AddThinker( strobe:thinker )
            EXIT

        CASE spec_tc_glow
            saveg_read_pad()
            glow := glow_t():New()
            saveg_read_glow_t( glow )
            BindGlow( glow )
            P_AddThinker( glow:thinker )
            EXIT

        OTHERWISE
            I_Error( "P_UnarchiveSpecials:Unknown tclass " + hb_ntos( tclass ) + " in savegame" )
        ENDSWITCH
    ENDDO

RETURN NIL
