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

STATIC totallines := 0
STATIC null_sector
STATIC null_sector_is_initialized := .F.
STATIC numsegs

#include "p_setup.ch"
#include "p_local.ch"
#include "doomstat.ch"
#include "doomdata.ch"
#include "m_bbox.ch"
#include "i_swap.ch"

#ifndef PU_STATIC
#define PU_STATIC 1
#endif
#ifndef PU_LEVEL
#define PU_LEVEL 5
#endif
#ifndef PU_PURGELEVEL
#define PU_PURGELEVEL 100
#endif

#define MAPVERTEX_SIZE    4
#define MAPSEG_SIZE       12
#define MAPSUBSECTOR_SIZE 4
#define MAPSECTOR_SIZE    26
#define MAPNODE_SIZE      28
#define MAPTHING_SIZE     10
#define MAPLINEDEF_SIZE   14
#define MAPSIDEDEF_SIZE   30



INIT PROCEDURE init_p_setup
    LOCAL i

    PUBLIC vertexes
    PUBLIC numvertexes
    PUBLIC segs
    PUBLIC sectors
    PUBLIC numsectors
    PUBLIC subsectors
    PUBLIC numsubsectors
    PUBLIC nodes
    PUBLIC numnodes
    PUBLIC lines
    PUBLIC numlines
    PUBLIC sides
    PUBLIC numsides
    PUBLIC blockmap
    PUBLIC blockmaplump
    PUBLIC blocklinks
    PUBLIC bmapwidth
    PUBLIC bmapheight
    PUBLIC bmaporgx
    PUBLIC bmaporgy
    PUBLIC rejectmatrix
    PUBLIC deathmatchstarts
    PUBLIC deathmatch_p
    PUBLIC playerstarts
    PUBLIC iquehead
    PUBLIC iquetail

    vertexes := {}
    numvertexes := 0
    segs := {}
    numsegs := 0
    sectors := {}
    numsectors := 0
    subsectors := {}
    numsubsectors := 0
    nodes := {}
    numnodes := 0
    lines := {}
    numlines := 0
    sides := {}
    numsides := 0
    blockmap := {}
    blockmaplump := {}
    blocklinks := {}
    bmapwidth := 0
    bmapheight := 0
    bmaporgx := 0
    bmaporgy := 0
    rejectmatrix := ""
    totallines := 0

    deathmatchstarts := {}
    FOR i := 1 TO MAX_DEATHMATCH_STARTS
        AAdd( deathmatchstarts, mapthing_t():New() )
    NEXT
    playerstarts := {}
    FOR i := 1 TO MAXPLAYERS
        AAdd( playerstarts, mapthing_t():New() )
    NEXT
    deathmatch_p := 0
    iquehead := 0
    iquetail := 0
RETURN

STATIC FUNCTION LumpUShort( c, nOff0 )
    LOCAL n

    n := Asc( SubStr( c, nOff0 + 1, 1 ) ) + Asc( SubStr( c, nOff0 + 2, 1 ) ) * 256

RETURN ( n & 0xFFFF )

STATIC FUNCTION LumpShort( c, nOff0 )
    LOCAL n

    n := LumpUShort( c, nOff0 )
    IF n >= 32768
        n := n - 65536
    ENDIF

RETURN n

STATIC FUNCTION LumpName8( c, nOff0 )
    LOCAL cName
    LOCAL nZero

    cName := SubStr( c, nOff0 + 1, 8 )
    nZero := At( Chr( 0 ), cName )
    IF nZero > 0
        cName := Left( cName, nZero - 1 )
    ENDIF
    DO WHILE ! Empty( cName ) .AND. Right( cName, 1 ) == " "
        cName := Left( cName, Len( cName ) - 1 )
    ENDDO
RETURN cName

STATIC FUNCTION MapShiftRight( nVal, nBits )
    LOCAL nDiv

    nDiv := 2 ^ nBits
    IF nVal >= 0
        RETURN Int( nVal / nDiv )
    ENDIF

RETURN Int( nVal / nDiv )

FUNCTION P_LoadVertexes( lump )
    LOCAL data
    LOCAL i
    LOCAL li
    MEMVAR numvertexes
    MEMVAR vertexes

    numvertexes := Int( W_LumpLength( lump ) / MAPVERTEX_SIZE )
    vertexes := {}
    FOR i := 1 TO numvertexes
        AAdd( vertexes, vertex_t():New() )
    NEXT

    data := W_CacheLumpNum( lump, PU_STATIC )

    FOR i := 0 TO numvertexes - 1
        li := vertexes[ i + 1 ]
        li:x := LumpShort( data, i * MAPVERTEX_SIZE ) * FRACUNIT
        li:y := LumpShort( data, i * MAPVERTEX_SIZE + 2 ) * FRACUNIT
    NEXT

    W_ReleaseLumpNum( lump )

RETURN NIL

FUNCTION GetSectorAtNullAddress()
    LOCAL nFloor
    LOCAL nCeil

    IF ! null_sector_is_initialized
        null_sector := sector_t():New()
        nFloor := 0
        nCeil := 0
        IF hb_IsFunction( "I_GetMemoryValue" )
            I_GetMemoryValue( 0, @nFloor, 4 )
            I_GetMemoryValue( 4, @nCeil, 4 )
        ENDIF
        null_sector:floorheight := nFloor
        null_sector:ceilingheight := nCeil
        null_sector_is_initialized := .T.
    ENDIF

RETURN null_sector

FUNCTION P_LoadSegs( lump )
    LOCAL data
    LOCAL i
    LOCAL li
    LOCAL ldef
    LOCAL linedef
    LOCAL side
    LOCAL sidenum
    LOCAL nOff
    LOCAL nSideIdx
    MEMVAR lines
    MEMVAR numsides
    MEMVAR segs
    MEMVAR sides
    MEMVAR vertexes

    numsegs := Int( W_LumpLength( lump ) / MAPSEG_SIZE )
    segs := {}
    FOR i := 1 TO numsegs
        AAdd( segs, seg_t():New() )
    NEXT

    data := W_CacheLumpNum( lump, PU_STATIC )

    FOR i := 0 TO numsegs - 1
        li := segs[ i + 1 ]
        nOff := i * MAPSEG_SIZE
        li:v1 := vertexes[ LumpShort( data, nOff ) + 1 ]
        li:v2 := vertexes[ LumpShort( data, nOff + 2 ) + 1 ]
        li:angle := LumpShort( data, nOff + 4 ) * 65536
        li:offset := LumpShort( data, nOff + 10 ) * 65536
        linedef := LumpShort( data, nOff + 6 )
        ldef := lines[ linedef + 1 ]
        li:linedef := ldef
        side := LumpShort( data, nOff + 8 )
        nSideIdx := ldef:sidenum[ side + 1 ]
        li:sidedef := sides[ nSideIdx + 1 ]
        li:frontsector := sides[ nSideIdx + 1 ]:sector

        IF ( ldef:flags & ML_TWOSIDED ) != 0
            sidenum := ldef:sidenum[ ( side ^^ 1 ) + 1 ]
            IF sidenum < 0 .OR. sidenum >= numsides
                li:backsector := GetSectorAtNullAddress()
            ELSE
                li:backsector := sides[ sidenum + 1 ]:sector
            ENDIF
        ELSE
            li:backsector := NIL
        ENDIF
    NEXT

    W_ReleaseLumpNum( lump )

RETURN NIL

FUNCTION P_LoadSubsectors( lump )
    LOCAL data
    LOCAL i
    LOCAL ss
    LOCAL nOff
    MEMVAR numsubsectors
    MEMVAR subsectors

    numsubsectors := Int( W_LumpLength( lump ) / MAPSUBSECTOR_SIZE )
    subsectors := {}
    FOR i := 1 TO numsubsectors
        AAdd( subsectors, subsector_t():New() )
    NEXT

    data := W_CacheLumpNum( lump, PU_STATIC )

    FOR i := 0 TO numsubsectors - 1
        ss := subsectors[ i + 1 ]
        nOff := i * MAPSUBSECTOR_SIZE
        ss:numlines := LumpShort( data, nOff )
        ss:firstline := LumpShort( data, nOff + 2 )
    NEXT

    W_ReleaseLumpNum( lump )

RETURN NIL

FUNCTION P_LoadSectors( lump )
    LOCAL data
    LOCAL i
    LOCAL ss
    LOCAL nOff
    MEMVAR numsectors
    MEMVAR sectors

    numsectors := Int( W_LumpLength( lump ) / MAPSECTOR_SIZE )
    sectors := {}
    FOR i := 1 TO numsectors
        AAdd( sectors, sector_t():New() )
    NEXT

    data := W_CacheLumpNum( lump, PU_STATIC )

    FOR i := 0 TO numsectors - 1
        ss := sectors[ i + 1 ]
        nOff := i * MAPSECTOR_SIZE
        ss:floorheight := LumpShort( data, nOff ) * FRACUNIT
        ss:ceilingheight := LumpShort( data, nOff + 2 ) * FRACUNIT
        ss:floorpic := R_FlatNumForName( LumpName8( data, nOff + 4 ) )
        ss:ceilingpic := R_FlatNumForName( LumpName8( data, nOff + 12 ) )
        ss:lightlevel := LumpShort( data, nOff + 20 )
        ss:special := LumpShort( data, nOff + 22 )
        ss:tag := LumpShort( data, nOff + 24 )
        ss:thinglist := NIL
        ss:iSector := i
    NEXT

    W_ReleaseLumpNum( lump )

RETURN NIL

FUNCTION P_LoadNodes( lump )
    LOCAL data
    LOCAL i
    LOCAL j
    LOCAL k
    LOCAL no
    LOCAL nOff
    MEMVAR nodes
    MEMVAR numnodes

    numnodes := Int( W_LumpLength( lump ) / MAPNODE_SIZE )
    nodes := {}
    FOR i := 1 TO numnodes
        AAdd( nodes, node_t():New() )
    NEXT

    data := W_CacheLumpNum( lump, PU_STATIC )

    FOR i := 0 TO numnodes - 1
        no := nodes[ i + 1 ]
        nOff := i * MAPNODE_SIZE
        no:x := LumpShort( data, nOff ) * FRACUNIT
        no:y := LumpShort( data, nOff + 2 ) * FRACUNIT
        no:dx := LumpShort( data, nOff + 4 ) * FRACUNIT
        no:dy := LumpShort( data, nOff + 6 ) * FRACUNIT
        FOR j := 0 TO 1
            no:children[ j + 1 ] := LumpUShort( data, nOff + 24 + j * 2 )
            FOR k := 0 TO 3
                no:bbox[ j + 1 ][ k + 1 ] := LumpShort( data, nOff + 8 + ( j * 4 + k ) * 2 ) * FRACUNIT
            NEXT
        NEXT
    NEXT

    W_ReleaseLumpNum( lump )

RETURN NIL

FUNCTION P_LoadThings( lump )
    LOCAL data
    LOCAL i
    LOCAL spawnthing
    LOCAL numthings
    LOCAL spawn
    LOCAL nOff
    LOCAL nType
    MEMVAR gamemode

    data := W_CacheLumpNum( lump, PU_STATIC )
    numthings := Int( W_LumpLength( lump ) / MAPTHING_SIZE )

    FOR i := 0 TO numthings - 1
        nOff := i * MAPTHING_SIZE
        spawn := .T.

        IF gamemode != commercial
            nType := LumpShort( data, nOff + 6 )
            SWITCH nType
            CASE 68
            CASE 64
            CASE 88
            CASE 89
            CASE 69
            CASE 67
            CASE 71
            CASE 65
            CASE 66
            CASE 84
                spawn := .F.
                EXIT
            ENDSWITCH
        ENDIF

        IF spawn == .F.
            EXIT
        ENDIF

        spawnthing := mapthing_t():New()
        spawnthing:x := LumpShort( data, nOff )
        spawnthing:y := LumpShort( data, nOff + 2 )
        spawnthing:angle := LumpShort( data, nOff + 4 )
        spawnthing:type := LumpShort( data, nOff + 6 )
        spawnthing:options := LumpShort( data, nOff + 8 )

        P_SpawnMapThing( spawnthing )
    NEXT

    W_ReleaseLumpNum( lump )

RETURN NIL

FUNCTION P_LoadLineDefs( lump )
    LOCAL data
    LOCAL i
    LOCAL ld
    LOCAL v1
    LOCAL v2
    LOCAL nOff
    MEMVAR lines
    MEMVAR numlines
    MEMVAR sides
    MEMVAR vertexes

    numlines := Int( W_LumpLength( lump ) / MAPLINEDEF_SIZE )
    lines := {}
    FOR i := 1 TO numlines
        AAdd( lines, line_t():New() )
    NEXT

    data := W_CacheLumpNum( lump, PU_STATIC )

    FOR i := 0 TO numlines - 1
        ld := lines[ i + 1 ]
        nOff := i * MAPLINEDEF_SIZE
        ld:iLine := i
        ld:flags := LumpShort( data, nOff + 4 )
        ld:special := LumpShort( data, nOff + 6 )
        ld:tag := LumpShort( data, nOff + 8 )
        v1 := vertexes[ LumpShort( data, nOff ) + 1 ]
        v2 := vertexes[ LumpShort( data, nOff + 2 ) + 1 ]
        ld:v1 := v1
        ld:v2 := v2
        ld:dx := v2:x - v1:x
        ld:dy := v2:y - v1:y

        IF ld:dx == 0
            ld:slopetype := ST_VERTICAL
        ELSEIF ld:dy == 0
            ld:slopetype := ST_HORIZONTAL
        ELSE
            IF FixedDiv( ld:dy, ld:dx ) > 0
                ld:slopetype := ST_POSITIVE
            ELSE
                ld:slopetype := ST_NEGATIVE
            ENDIF
        ENDIF

        IF v1:x < v2:x
            ld:bbox[ BOXLEFT + 1 ] := v1:x
            ld:bbox[ BOXRIGHT + 1 ] := v2:x
        ELSE
            ld:bbox[ BOXLEFT + 1 ] := v2:x
            ld:bbox[ BOXRIGHT + 1 ] := v1:x
        ENDIF

        IF v1:y < v2:y
            ld:bbox[ BOXBOTTOM + 1 ] := v1:y
            ld:bbox[ BOXTOP + 1 ] := v2:y
        ELSE
            ld:bbox[ BOXBOTTOM + 1 ] := v2:y
            ld:bbox[ BOXTOP + 1 ] := v1:y
        ENDIF

        ld:sidenum[ 1 ] := LumpShort( data, nOff + 10 )
        ld:sidenum[ 2 ] := LumpShort( data, nOff + 12 )

        IF ld:sidenum[ 1 ] != -1
            ld:frontsector := sides[ ld:sidenum[ 1 ] + 1 ]:sector
        ELSE
            ld:frontsector := NIL
        ENDIF

        IF ld:sidenum[ 2 ] != -1
            ld:backsector := sides[ ld:sidenum[ 2 ] + 1 ]:sector
        ELSE
            ld:backsector := NIL
        ENDIF
    NEXT

    W_ReleaseLumpNum( lump )

RETURN NIL

FUNCTION P_LoadSideDefs( lump )
    LOCAL data
    LOCAL i
    LOCAL sd
    LOCAL nOff
    MEMVAR numsides
    MEMVAR sectors
    MEMVAR sides

    numsides := Int( W_LumpLength( lump ) / MAPSIDEDEF_SIZE )
    sides := {}
    FOR i := 1 TO numsides
        AAdd( sides, side_t():New() )
    NEXT

    data := W_CacheLumpNum( lump, PU_STATIC )

    FOR i := 0 TO numsides - 1
        sd := sides[ i + 1 ]
        nOff := i * MAPSIDEDEF_SIZE
        sd:textureoffset := LumpShort( data, nOff ) * FRACUNIT
        sd:rowoffset := LumpShort( data, nOff + 2 ) * FRACUNIT
        sd:toptexture := R_TextureNumForName( LumpName8( data, nOff + 4 ) )
        sd:bottomtexture := R_TextureNumForName( LumpName8( data, nOff + 12 ) )
        sd:midtexture := R_TextureNumForName( LumpName8( data, nOff + 20 ) )
        sd:sector := sectors[ LumpShort( data, nOff + 28 ) + 1 ]
    NEXT

    W_ReleaseLumpNum( lump )

RETURN NIL

FUNCTION P_LoadBlockMap( lump )
    LOCAL i
    LOCAL count
    LOCAL lumplen
    LOCAL data
    LOCAL nBlocks
    MEMVAR blocklinks
    MEMVAR blockmap
    MEMVAR blockmaplump
    MEMVAR bmapheight
    MEMVAR bmaporgx
    MEMVAR bmaporgy
    MEMVAR bmapwidth

    lumplen := W_LumpLength( lump )
    count := Int( lumplen / 2 )

    data := W_CacheLumpNum( lump, PU_LEVEL )
    blockmaplump := {}
    FOR i := 0 TO count - 1
        AAdd( blockmaplump, LumpShort( data, i * 2 ) )
    NEXT
    W_ReleaseLumpNum( lump )

    bmaporgx := blockmaplump[ 1 ] * FRACUNIT
    bmaporgy := blockmaplump[ 2 ] * FRACUNIT
    bmapwidth := blockmaplump[ 3 ]
    bmapheight := blockmaplump[ 4 ]

    nBlocks := bmapwidth * bmapheight
    blockmap := {}
    FOR i := 0 TO nBlocks - 1
        AAdd( blockmap, blockmaplump[ 4 + i + 1 ] )
    NEXT

    blocklinks := {}
    FOR i := 1 TO nBlocks
        AAdd( blocklinks, NIL )
    NEXT

RETURN NIL

FUNCTION P_GroupLines()
    LOCAL i
    LOCAL j
    LOCAL li
    LOCAL sector
    LOCAL ss
    LOCAL seg
    LOCAL bbox
    LOCAL block
    MEMVAR bmapheight
    MEMVAR bmaporgx
    MEMVAR bmaporgy
    MEMVAR bmapwidth
    MEMVAR lines
    MEMVAR numlines
    MEMVAR numsectors
    MEMVAR numsubsectors
    MEMVAR sectors
    MEMVAR segs
    MEMVAR subsectors

    FOR i := 0 TO numsubsectors - 1
        ss := subsectors[ i + 1 ]
        seg := segs[ ss:firstline + 1 ]
        ss:sector := seg:sidedef:sector
    NEXT

    totallines := 0
    FOR i := 0 TO numlines - 1
        li := lines[ i + 1 ]
        totallines := totallines + 1
        IF li:frontsector != NIL
            li:frontsector:linecount := li:frontsector:linecount + 1
        ENDIF
        IF li:backsector != NIL .AND. !( li:backsector == li:frontsector )
            li:backsector:linecount := li:backsector:linecount + 1
            totallines := totallines + 1
        ENDIF
    NEXT

    FOR i := 0 TO numsectors - 1
        sectors[ i + 1 ]:lines := {}
        sectors[ i + 1 ]:linecount := 0
        sectors[ i + 1 ]:iSector := i
    NEXT

    FOR i := 0 TO numlines - 1
        li := lines[ i + 1 ]
        IF li:frontsector != NIL
            sector := li:frontsector
            AAdd( sector:lines, li )
            sector:linecount := sector:linecount + 1
        ENDIF
        IF li:backsector != NIL .AND. !( li:frontsector == li:backsector )
            sector := li:backsector
            AAdd( sector:lines, li )
            sector:linecount := sector:linecount + 1
        ENDIF
    NEXT

    FOR i := 0 TO numsectors - 1
        sector := sectors[ i + 1 ]
        sector:iSector := i
        bbox := { 0, 0, 0, 0 }
        M_ClearBox( bbox )

        FOR j := 0 TO sector:linecount - 1
            li := sector:lines[ j + 1 ]
            M_AddToBox( bbox, li:v1:x, li:v1:y )
            M_AddToBox( bbox, li:v2:x, li:v2:y )
        NEXT

        sector:soundorg:x := Int( ( bbox[ BOXRIGHT + 1 ] + bbox[ BOXLEFT + 1 ] ) / 2 )
        sector:soundorg:y := Int( ( bbox[ BOXTOP + 1 ] + bbox[ BOXBOTTOM + 1 ] ) / 2 )

        block := MapShiftRight( bbox[ BOXTOP + 1 ] - bmaporgy + MAXRADIUS, MAPBLOCKSHIFT )
        IF block >= bmapheight
            block := bmapheight - 1
        ENDIF
        sector:blockbox[ BOXTOP + 1 ] := block

        block := MapShiftRight( bbox[ BOXBOTTOM + 1 ] - bmaporgy - MAXRADIUS, MAPBLOCKSHIFT )
        IF block < 0
            block := 0
        ENDIF
        sector:blockbox[ BOXBOTTOM + 1 ] := block

        block := MapShiftRight( bbox[ BOXRIGHT + 1 ] - bmaporgx + MAXRADIUS, MAPBLOCKSHIFT )
        IF block >= bmapwidth
            block := bmapwidth - 1
        ENDIF
        sector:blockbox[ BOXRIGHT + 1 ] := block

        block := MapShiftRight( bbox[ BOXLEFT + 1 ] - bmaporgx - MAXRADIUS, MAPBLOCKSHIFT )
        IF block < 0
            block := 0
        ENDIF
        sector:blockbox[ BOXLEFT + 1 ] := block
    NEXT

RETURN NIL

STATIC PROCEDURE PadRejectArray( nLen )
    LOCAL i
    LOCAL byte_num
    LOCAL rejectpad
    LOCAL padvalue
    LOCAL nVal
    LOCAL cPad
    MEMVAR rejectmatrix

    rejectpad := { ;
        ( ( totallines * 4 + 3 ) & hb_bitNot( 3 ) ) + 24, ;
        0, ;
        50, ;
        0x1d4a11 }

    cPad := ""
    FOR i := 0 TO nLen - 1
        IF i < 16
            byte_num := i % 4
            nVal := rejectpad[ Int( i / 4 ) + 1 ]
            cPad += Chr( ( Int( nVal / ( 256 ^ byte_num ) ) & 0xFF ) )
        ENDIF
    NEXT

    IF nLen > 16
        OutErr( "PadRejectArray: REJECT lump too short to pad! (" + ;
            hb_ntos( nLen ) + " > 16)" + hb_eol() )
        IF M_CheckParm( "-reject_pad_with_ff" ) != 0
            padvalue := 0xff
        ELSE
            padvalue := 0xf00
        ENDIF
        cPad += Replicate( Chr( ( padvalue & 0xFF ) ), nLen - 16 )
    ENDIF

    rejectmatrix += cPad

RETURN

STATIC PROCEDURE P_LoadReject( lumpnum )
    LOCAL minlength
    LOCAL lumplen
    LOCAL data
    MEMVAR numsectors
    MEMVAR rejectmatrix

    minlength := Int( ( numsectors * numsectors + 7 ) / 8 )
    lumplen := W_LumpLength( lumpnum )

    IF lumplen >= minlength
        rejectmatrix := W_CacheLumpNum( lumpnum, PU_LEVEL )
    ELSE
        data := W_CacheLumpNum( lumpnum, PU_LEVEL )
        rejectmatrix := data
        W_ReleaseLumpNum( lumpnum )
        PadRejectArray( minlength - lumplen )
    ENDIF

RETURN

STATIC FUNCTION SetupLumpName( episode, map )
    LOCAL lumpname
    MEMVAR gamemode

    lumpname := ""
    IF gamemode == commercial
        IF map < 10
            IF hb_IsFunction( "M_snprintf" )
                M_snprintf( @lumpname, 9, "map0%i", map )
            ELSE
                lumpname := "map0" + hb_ntos( map )
            ENDIF
        ELSE
            IF hb_IsFunction( "M_snprintf" )
                M_snprintf( @lumpname, 9, "map%i", map )
            ELSE
                lumpname := "map" + hb_ntos( map )
            ENDIF
        ENDIF
    ELSE
        lumpname := "E" + hb_ntos( episode ) + "M" + hb_ntos( map )
    ENDIF

RETURN lumpname

FUNCTION P_SetupLevel( episode, map, playermask, skill )
    LOCAL i
    LOCAL lumpname
    LOCAL lumpnum
    MEMVAR bodyqueslot
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR deathmatch_p
    MEMVAR iquehead
    MEMVAR iquetail
    MEMVAR leveltime
    MEMVAR playeringame
    MEMVAR players
    MEMVAR precache
    MEMVAR totalitems
    MEMVAR totalkills
    MEMVAR totalsecret
    MEMVAR wminfo

    totalkills := 0
    totalitems := 0
    totalsecret := 0
    wminfo:maxfrags := 0
    wminfo:partime := 180
    FOR i := 0 TO MAXPLAYERS - 1
        players[ i + 1 ]:killcount := 0
        players[ i + 1 ]:secretcount := 0
        players[ i + 1 ]:itemcount := 0
    NEXT

    players[ consoleplayer + 1 ]:viewz := 1

    S_Start()

    Z_FreeTags( PU_LEVEL, PU_PURGELEVEL - 1 )

    P_InitThinkers()

    lumpname := SetupLumpName( episode, map )
    lumpnum := W_GetNumForName( lumpname )

    leveltime := 0

    P_LoadBlockMap( lumpnum + ML_BLOCKMAP )
    P_LoadVertexes( lumpnum + ML_VERTEXES )
    P_LoadSectors( lumpnum + ML_SECTORS )
    P_LoadSideDefs( lumpnum + ML_SIDEDEFS )

    P_LoadLineDefs( lumpnum + ML_LINEDEFS )
    P_LoadSubsectors( lumpnum + ML_SSECTORS )
    P_LoadNodes( lumpnum + ML_NODES )
    P_LoadSegs( lumpnum + ML_SEGS )

    P_GroupLines()
    P_LoadReject( lumpnum + ML_REJECT )

    bodyqueslot := 0
    deathmatch_p := 0
    P_LoadThings( lumpnum + ML_THINGS )

    IF deathmatch != 0
        FOR i := 0 TO MAXPLAYERS - 1
            IF playeringame[ i + 1 ]
                players[ i + 1 ]:mo := NIL
                G_DeathMatchSpawnPlayer( i )
            ENDIF
        NEXT
    ENDIF

    iquehead := 0
    iquetail := 0

    P_SpawnSpecials()

    IF precache
        R_PrecacheLevel()
    ENDIF

RETURN NIL

FUNCTION P_Init()

    MEMVAR sprnames
    P_InitSwitchList()
    P_InitPicAnims()
    R_InitSprites( sprnames )

RETURN NIL
