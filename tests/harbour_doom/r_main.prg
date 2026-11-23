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

STATIC centerx
STATIC framecount
STATIC linecount
STATIC loopcount
STATIC scalelightfixed
STATIC setblocks
STATIC setdetail

#include "r_main.ch"
#include "r_defs.ch"
#include "r_state.ch"
#include "r_sky.ch"
#include "p_local.ch"
#include "doomdata.ch"
#include "m_bbox.ch"
#include "m_fixed.ch"
#include "doomdef.ch"
#include "d_player.ch"
#include "i_video.ch"
#include "r_plane.ch"
#include "r_things.ch"
#include "m_menu.ch"


#define FIELDOFVIEW 2048
#define DISTMAP     2

#ifndef FINEMASK
#define FINEMASK ( FINEANGLES - 1 )
#endif
#ifndef SLOPERANGE
#define SLOPERANGE 2048
#define SLOPEBITS  11
#define DBITS      ( FRACBITS - SLOPEBITS )
#endif

STATIC FUNCTION ColorMapAt( nLevel )
    MEMVAR colormaps
    LOCAL nOff
    nOff := nLevel * 256 + 1
    IF ValType( colormaps ) != "C"
        RETURN Replicate( Chr( 0 ), 256 )
    ENDIF
    IF nOff < 1
        nOff := 1
    ENDIF
RETURN SubStr( colormaps, nOff, 256 )

STATIC FUNCTION AsU32( n )
RETURN ( n & 0xFFFFFFFF )

STATIC FUNCTION IfaceCall( xFun, x1, x2 )
    LOCAL nArgs := PCount() - 1

    IF xFun == NIL
        RETURN NIL
    ENDIF

    IF ValType( xFun ) == "B"
        IF nArgs <= 0
            RETURN Eval( xFun )
        ELSEIF nArgs == 1
            RETURN Eval( xFun, x1 )
        ENDIF
        RETURN Eval( xFun, x1, x2 )
    ENDIF

    IF ValType( xFun ) == "C"
        IF nArgs <= 0
            RETURN &( xFun )()
        ELSEIF nArgs == 1
            RETURN &( xFun )( x1 )
        ENDIF
        RETURN &( xFun )( x1, x2 )
    ENDIF
RETURN NIL

PROCEDURE R_AddPointToBox( x, y, box )
    IF x < box[ BOXLEFT + 1 ]
        box[ BOXLEFT + 1 ] := x
    ENDIF
    IF x > box[ BOXRIGHT + 1 ]
        box[ BOXRIGHT + 1 ] := x
    ENDIF
    IF y < box[ BOXBOTTOM + 1 ]
        box[ BOXBOTTOM + 1 ] := y
    ENDIF
    IF y > box[ BOXTOP + 1 ]
        box[ BOXTOP + 1 ] := y
    ENDIF
RETURN

FUNCTION R_PointOnSide( x, y, node )
    LOCAL dx
    LOCAL dy
    LOCAL left
    LOCAL right

    IF node:dx == 0
        IF x <= node:x
            RETURN iif( node:dy > 0, 1, 0 )
        ENDIF
        RETURN iif( node:dy < 0, 1, 0 )
    ENDIF

    IF node:dy == 0
        IF y <= node:y
            RETURN iif( node:dx < 0, 1, 0 )
        ENDIF
        RETURN iif( node:dx > 0, 1, 0 )
    ENDIF

    dx := x - node:x
    dy := y - node:y

    IF ( ( node:dy ^^ node:dx ^^ dx ^^ dy ) & 0x80000000 ) != 0
        IF ( ( node:dy ^^ dx ) & 0x80000000 ) != 0
            RETURN 1
        ENDIF
        RETURN 0
    ENDIF

    left := FixedMul( Shar( node:dy, FRACBITS ), dx )
    right := FixedMul( dy, Shar( node:dx, FRACBITS ) )

    IF right < left
        RETURN 0
    ENDIF
RETURN 1

FUNCTION R_PointOnSegSide( x, y, line )
    LOCAL lx
    LOCAL ly
    LOCAL ldx
    LOCAL ldy
    LOCAL dx
    LOCAL dy
    LOCAL left
    LOCAL right

    lx := line:v1:x
    ly := line:v1:y
    ldx := line:v2:x - lx
    ldy := line:v2:y - ly

    IF ldx == 0
        IF x <= lx
            RETURN iif( ldy > 0, 1, 0 )
        ENDIF
        RETURN iif( ldy < 0, 1, 0 )
    ENDIF

    IF ldy == 0
        IF y <= ly
            RETURN iif( ldx < 0, 1, 0 )
        ENDIF
        RETURN iif( ldx > 0, 1, 0 )
    ENDIF

    dx := x - lx
    dy := y - ly

    IF ( ( ldy ^^ ldx ^^ dx ^^ dy ) & 0x80000000 ) != 0
        IF ( ( ldy ^^ dx ) & 0x80000000 ) != 0
            RETURN 1
        ENDIF
        RETURN 0
    ENDIF

    left := FixedMul( Shar( ldy, FRACBITS ), dx )
    right := FixedMul( dy, Shar( ldx, FRACBITS ) )

    IF right < left
        RETURN 0
    ENDIF
RETURN 1

FUNCTION R_PointToAngle( x, y )
    MEMVAR tantoangle
    MEMVAR viewx
    MEMVAR viewy
    x -= viewx
    y -= viewy

    IF x == 0 .AND. y == 0
        RETURN 0
    ENDIF

    IF x >= 0
        IF y >= 0
            IF x > y
                RETURN AsU32( tantoangle[ SlopeDiv( y, x ) + 1 ] )
            ENDIF
            RETURN AsU32( ANG90 - 1 - tantoangle[ SlopeDiv( x, y ) + 1 ] )
        ENDIF

        y := -y
        IF x > y
            RETURN AsU32( 0 - tantoangle[ SlopeDiv( y, x ) + 1 ] )
        ENDIF
        RETURN AsU32( ANG270 + tantoangle[ SlopeDiv( x, y ) + 1 ] )
    ENDIF

    x := -x
    IF y >= 0
        IF x > y
            RETURN AsU32( ANG180 - 1 - tantoangle[ SlopeDiv( y, x ) + 1 ] )
        ENDIF
        RETURN AsU32( ANG90 + tantoangle[ SlopeDiv( x, y ) + 1 ] )
    ENDIF

    y := -y
    IF x > y
        RETURN AsU32( ANG180 + tantoangle[ SlopeDiv( y, x ) + 1 ] )
    ENDIF
RETURN AsU32( ANG270 - 1 - tantoangle[ SlopeDiv( x, y ) + 1 ] )

FUNCTION R_PointToAngle2( x1, y1, x2, y2 )
    MEMVAR viewx
    MEMVAR viewy
    viewx := x1
    viewy := y1
RETURN R_PointToAngle( x2, y2 )

FUNCTION R_PointToDist( x, y )
    LOCAL angle
    LOCAL dx
    LOCAL dy
    LOCAL temp
    LOCAL dist
    LOCAL frac
    MEMVAR finesine
    MEMVAR tantoangle
    MEMVAR viewx
    MEMVAR viewy

    dx := Abs( x - viewx )
    dy := Abs( y - viewy )

    IF dy > dx
        temp := dx
        dx := dy
        dy := temp
    ENDIF

    IF dx != 0
        frac := FixedDiv( dy, dx )
    ELSE
        frac := 0
    ENDIF

    angle := UShr( tantoangle[ Shar( frac, DBITS ) + 1 ] + ANG90, ;
                   ANGLETOFINESHIFT )
    dist := FixedDiv( dx, finesine[ angle + 1 ] )
RETURN dist

PROCEDURE R_InitPointToAngle()
RETURN

FUNCTION R_ScaleFromGlobalAngle( visangle )
    LOCAL scale
    LOCAL anglea
    LOCAL angleb
    LOCAL sinea
    LOCAL sineb
    LOCAL num
    LOCAL den
    MEMVAR detailshift
    MEMVAR finesine
    MEMVAR projection
    MEMVAR rw_distance
    MEMVAR rw_normalangle
    MEMVAR viewangle

    anglea := AsU32( ANG90 + AsU32( visangle - viewangle ) )
    angleb := AsU32( ANG90 + AsU32( visangle - rw_normalangle ) )

    sinea := finesine[ UShr( anglea, ANGLETOFINESHIFT ) + 1 ]
    sineb := finesine[ UShr( angleb, ANGLETOFINESHIFT ) + 1 ]
    num := FixedMul( projection, sineb ) * ( 2 ^ detailshift )
    den := FixedMul( rw_distance, sinea )

    IF den > Shar( num, 16 )
        scale := FixedDiv( num, den )
        IF scale > 64 * FRACUNIT
            scale := 64 * FRACUNIT
        ELSEIF scale < 256
            scale := 256
        ENDIF
    ELSE
        scale := 64 * FRACUNIT
    ENDIF
RETURN scale

PROCEDURE R_InitTables()
RETURN

PROCEDURE R_InitTextureMapping()
    LOCAL i
    LOCAL x
    LOCAL t
    LOCAL focallength
    MEMVAR centerxfrac
    MEMVAR clipangle
    MEMVAR finetangent
    MEMVAR viewangletox
    MEMVAR viewwidth
    MEMVAR xtoviewangle

    focallength := FixedDiv( centerxfrac, ;
        finetangent[ Int( FINEANGLES / 4 ) + Int( FIELDOFVIEW / 2 ) + 1 ] )

    FOR i := 0 TO Int( FINEANGLES / 2 ) - 1
        IF finetangent[ i + 1 ] > FRACUNIT * 2
            t := -1
        ELSEIF finetangent[ i + 1 ] < -FRACUNIT * 2
            t := viewwidth + 1
        ELSE
            t := FixedMul( finetangent[ i + 1 ], focallength )
            t := Shar( centerxfrac - t + FRACUNIT - 1, FRACBITS )
            IF t < -1
                t := -1
            ELSEIF t > viewwidth + 1
                t := viewwidth + 1
            ENDIF
        ENDIF
        viewangletox[ i + 1 ] := t
    NEXT

    FOR x := 0 TO viewwidth
        i := 0
        DO WHILE viewangletox[ i + 1 ] > x
            i++
        ENDDO
        xtoviewangle[ x + 1 ] := AsU32( i * ( 2 ^ ANGLETOFINESHIFT ) - ANG90 )
    NEXT

    FOR i := 0 TO Int( FINEANGLES / 2 ) - 1
        t := FixedMul( finetangent[ i + 1 ], focallength )
        t := centerx - t
        IF viewangletox[ i + 1 ] == -1
            viewangletox[ i + 1 ] := 0
        ELSEIF viewangletox[ i + 1 ] == viewwidth + 1
            viewangletox[ i + 1 ] := viewwidth
        ENDIF
    NEXT

    clipangle := xtoviewangle[ 1 ]
RETURN

PROCEDURE R_InitLightTables()
    LOCAL i
    LOCAL j
    LOCAL level
    LOCAL startmap
    LOCAL scale
    MEMVAR colormaps
    MEMVAR zlight

    FOR i := 0 TO LIGHTLEVELS - 1
        startmap := Int( ( ( LIGHTLEVELS - 1 - i ) * 2 ) * ;
                         NUMCOLORMAPS / LIGHTLEVELS )
        FOR j := 0 TO MAXLIGHTZ - 1
            scale := FixedDiv( Int( SCREENWIDTH / 2 ) * FRACUNIT, ;
                               ( j + 1 ) * ( 2 ^ LIGHTZSHIFT ) )
            scale := Shar( scale, LIGHTSCALESHIFT )
            level := startmap - Int( scale / DISTMAP )

            IF level < 0
                level := 0
            ENDIF
            IF level >= NUMCOLORMAPS
                level := NUMCOLORMAPS - 1
            ENDIF
            zlight[ i + 1 ][ j + 1 ] := ColorMapAt( level )
        NEXT
    NEXT
RETURN

PROCEDURE R_SetViewSize( blocks, detail )
    MEMVAR setsizeneeded
    setsizeneeded := .T.
    setblocks := blocks
    setdetail := detail
RETURN

PROCEDURE R_ExecuteSetViewSize()
    LOCAL cosadj
    LOCAL dy
    LOCAL i
    LOCAL j
    LOCAL level
    LOCAL startmap
    MEMVAR basecolfunc
    MEMVAR centerxfrac
    MEMVAR centeryfrac
    MEMVAR colfunc
    MEMVAR colormaps
    MEMVAR detailshift
    MEMVAR distscale
    MEMVAR finecosine
    MEMVAR fuzzcolfunc
    MEMVAR projection
    MEMVAR pspriteiscale
    MEMVAR pspritescale
    MEMVAR scaledviewwidth
    MEMVAR scalelight
    MEMVAR screenheightarray
    MEMVAR setsizeneeded
    MEMVAR spanfunc
    MEMVAR transcolfunc
    MEMVAR viewwidth
    MEMVAR xtoviewangle
    MEMVAR yslope

    setsizeneeded := .F.

    IF setblocks == 11
        scaledviewwidth := SCREENWIDTH
        viewheight := SCREENHEIGHT
    ELSE
        scaledviewwidth := setblocks * 32
        viewheight := ( Int( setblocks * 168 / 10 ) & ( 7 ^^ 0xFFFFFFFF ) )
    ENDIF

    detailshift := setdetail
    viewwidth := Shar( scaledviewwidth, detailshift )

    centery := Int( viewheight / 2 )
    centerx := Int( viewwidth / 2 )
    centerxfrac := centerx * ( 2 ^ FRACBITS )
    centeryfrac := centery * ( 2 ^ FRACBITS )
    projection := centerxfrac

    IF detailshift == 0
        colfunc := "R_DrawColumn"
        basecolfunc := "R_DrawColumn"
        fuzzcolfunc := "R_DrawFuzzColumn"
        transcolfunc := "R_DrawTranslatedColumn"
        spanfunc := "R_DrawSpan"
    ELSE
        colfunc := "R_DrawColumnLow"
        basecolfunc := "R_DrawColumnLow"
        fuzzcolfunc := "R_DrawFuzzColumnLow"
        transcolfunc := "R_DrawTranslatedColumnLow"
        spanfunc := "R_DrawSpanLow"
    ENDIF

    R_InitBuffer( scaledviewwidth, viewheight )
    R_InitTextureMapping()

    pspritescale := Int( FRACUNIT * viewwidth / SCREENWIDTH )
    pspriteiscale := Int( FRACUNIT * SCREENWIDTH / viewwidth )

    FOR i := 0 TO viewwidth - 1
        screenheightarray[ i + 1 ] := viewheight
    NEXT

    FOR i := 0 TO viewheight - 1
        dy := ( i - Int( viewheight / 2 ) ) * ( 2 ^ FRACBITS ) + ;
              Int( FRACUNIT / 2 )
        dy := Abs( dy )
        yslope[ i + 1 ] := FixedDiv( ;
            Int( ( viewwidth * ( 2 ^ detailshift ) ) / 2 ) * FRACUNIT, dy )
    NEXT

    FOR i := 0 TO viewwidth - 1
        cosadj := Abs( finecosine[ ;
            UShr( xtoviewangle[ i + 1 ], ANGLETOFINESHIFT ) + 1 ] )
        distscale[ i + 1 ] := FixedDiv( FRACUNIT, cosadj )
    NEXT

    FOR i := 0 TO LIGHTLEVELS - 1
        startmap := Int( ( ( LIGHTLEVELS - 1 - i ) * 2 ) * ;
                         NUMCOLORMAPS / LIGHTLEVELS )
        FOR j := 0 TO MAXLIGHTSCALE - 1
            level := startmap - Int( j * SCREENWIDTH / ;
                ( viewwidth * ( 2 ^ detailshift ) ) / DISTMAP )
            IF level < 0
                level := 0
            ENDIF
            IF level >= NUMCOLORMAPS
                level := NUMCOLORMAPS - 1
            ENDIF
            scalelight[ i + 1 ][ j + 1 ] := ColorMapAt( level )
        NEXT
    NEXT
RETURN

PROCEDURE R_Init()
    MEMVAR detailLevel
    MEMVAR screenblocks
    R_InitData()
    OutStd( "." )
    R_InitPointToAngle()
    OutStd( "." )
    R_InitTables()
    OutStd( "." )
    R_SetViewSize( screenblocks, detailLevel )
    R_InitPlanes()
    OutStd( "." )
    R_InitLightTables()
    OutStd( "." )
    R_InitSkyMap()
    R_InitTranslationTables()
    OutStd( "." )
    framecount := 0
RETURN

FUNCTION R_PointInSubsector( x, y )
    LOCAL node
    LOCAL side
    LOCAL bspnum
    MEMVAR nodes
    MEMVAR numnodes
    MEMVAR subsectors

    IF numnodes == 0
        RETURN subsectors[ 1 ]
    ENDIF

    bspnum := numnodes - 1
    IF bspnum == -1
        RETURN subsectors[ 1 ]
    ENDIF

    DO WHILE ( bspnum & NF_SUBSECTOR ) == 0
        node := nodes[ bspnum + 1 ]
        side := R_PointOnSide( x, y, node )
        bspnum := node:children[ side + 1 ]
    ENDDO
RETURN subsectors[ ( bspnum & 0x7FFF ) + 1 ]

PROCEDURE R_SetupFrame( player )
    LOCAL i
    MEMVAR colormaps
    MEMVAR extralight
    MEMVAR finecosine
    MEMVAR finesine
    MEMVAR fixedcolormap
    MEMVAR sscount
    MEMVAR validcount
    MEMVAR viewangle
    MEMVAR viewangleoffset
    MEMVAR viewcos
    MEMVAR viewplayer
    MEMVAR viewsin
    MEMVAR viewx
    MEMVAR viewy
    MEMVAR viewz
    MEMVAR walllights

    viewplayer := player
    viewx := player:mo:x
    viewy := player:mo:y
    viewangle := AsU32( player:mo:angle + viewangleoffset )
    extralight := player:extralight
    viewz := player:viewz

    viewsin := finesine[ UShr( viewangle, ANGLETOFINESHIFT ) + 1 ]
    viewcos := finecosine[ UShr( viewangle, ANGLETOFINESHIFT ) + 1 ]
    sscount := 0

    IF player:fixedcolormap != 0
        fixedcolormap := ColorMapAt( player:fixedcolormap )
        walllights := scalelightfixed
        FOR i := 0 TO MAXLIGHTSCALE - 1
            scalelightfixed[ i + 1 ] := fixedcolormap
        NEXT
    ELSE
        fixedcolormap := NIL
    ENDIF

    framecount++
    validcount++
RETURN

PROCEDURE R_RenderPlayerView( player )
    MEMVAR numnodes
    R_SetupFrame( player )
    R_ClearClipSegs()
    R_ClearDrawSegs()
    R_ClearPlanes()
    R_ClearSprites()
    NetUpdate()
    R_RenderBSPNode( numnodes - 1 )
    NetUpdate()
    R_DrawPlanes()
    NetUpdate()
    R_DrawMasked()
    NetUpdate()
RETURN

INIT PROCEDURE init_r_main
    LOCAL i
    LOCAL j

    PUBLIC viewangleoffset
    PUBLIC validcount
    PUBLIC fixedcolormap
    PUBLIC walllights
    PUBLIC centery
    PUBLIC centerxfrac
    PUBLIC centeryfrac
    PUBLIC projection
    PUBLIC sscount
    PUBLIC viewx
    PUBLIC viewy
    PUBLIC viewz
    PUBLIC viewangle
    PUBLIC viewcos
    PUBLIC viewsin
    PUBLIC viewplayer
    PUBLIC detailshift
    PUBLIC clipangle
    PUBLIC viewangletox
    PUBLIC xtoviewangle
    PUBLIC scalelight
    PUBLIC zlight
    PUBLIC extralight
    PUBLIC colfunc
    PUBLIC basecolfunc
    PUBLIC fuzzcolfunc
    PUBLIC transcolfunc
    PUBLIC spanfunc
    PUBLIC setsizeneeded

    viewangleoffset := 0
    validcount := 1
    fixedcolormap := NIL
    walllights := NIL
    centerx := 0
    centery := 0
    centerxfrac := 0
    centeryfrac := 0
    projection := 0
    framecount := 0
    sscount := 0
    linecount := 0
    loopcount := 0
    viewx := 0
    viewy := 0
    viewz := 0
    viewangle := 0
    viewcos := 0
    viewsin := 0
    viewplayer := NIL
    detailshift := 0
    clipangle := 0
    extralight := 0
    colfunc := NIL
    basecolfunc := NIL
    fuzzcolfunc := NIL
    transcolfunc := NIL
    spanfunc := NIL
    setsizeneeded := .F.
    setblocks := 0
    setdetail := 0

    viewangletox := {}
    FOR i := 1 TO Int( FINEANGLES / 2 )
        AAdd( viewangletox, 0 )
    NEXT

    xtoviewangle := {}
    FOR i := 1 TO SCREENWIDTH + 1
        AAdd( xtoviewangle, 0 )
    NEXT

    scalelight := {}
    FOR i := 1 TO LIGHTLEVELS
        AAdd( scalelight, {} )
        FOR j := 1 TO MAXLIGHTSCALE
            AAdd( scalelight[ i ], NIL )
        NEXT
    NEXT

    scalelightfixed := {}
    FOR i := 1 TO MAXLIGHTSCALE
        AAdd( scalelightfixed, NIL )
    NEXT

    zlight := {}
    FOR i := 1 TO LIGHTLEVELS
        AAdd( zlight, {} )
        FOR j := 1 TO MAXLIGHTZ
            AAdd( zlight[ i ], NIL )
        NEXT
    NEXT
RETURN
