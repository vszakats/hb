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

STATIC solidsegs
STATIC newend
STATIC checkcoord

#include "r_bsp.ch"
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
#include "r_plane.ch"


#define MAXSEGS 32


STATIC FUNCTION AsU32( n )
RETURN ( n & 0xFFFFFFFF )

PROCEDURE R_ClearDrawSegs()
    MEMVAR ds_p
    ds_p := 0
RETURN

PROCEDURE R_ClipSolidWallSegment( first, last )
    LOCAL nNext
    LOCAL start

    start := 1
    DO WHILE solidsegs[ start ][ 2 ] < first - 1
        start++
    ENDDO

    IF first < solidsegs[ start ][ 1 ]
        IF last < solidsegs[ start ][ 1 ] - 1
            R_StoreWallRange( first, last )
            nNext := newend
            newend++

            DO WHILE nNext != start
                solidsegs[ nNext ] := { ;
                    solidsegs[ nNext - 1 ][ 1 ], ;
                    solidsegs[ nNext - 1 ][ 2 ] }
                nNext--
            ENDDO
            solidsegs[ nNext ][ 1 ] := first
            solidsegs[ nNext ][ 2 ] := last
            RETURN
        ENDIF

        R_StoreWallRange( first, solidsegs[ start ][ 1 ] - 1 )
        solidsegs[ start ][ 1 ] := first
    ENDIF

    IF last <= solidsegs[ start ][ 2 ]
        RETURN
    ENDIF

    nNext := start
    DO WHILE last >= solidsegs[ nNext + 1 ][ 1 ] - 1
        R_StoreWallRange( solidsegs[ nNext ][ 2 ] + 1, ;
                          solidsegs[ nNext + 1 ][ 1 ] - 1 )
        nNext++

        IF last <= solidsegs[ nNext ][ 2 ]
            solidsegs[ start ][ 2 ] := solidsegs[ nNext ][ 2 ]
            EXIT
        ENDIF
    ENDDO

    IF last > solidsegs[ start ][ 2 ] .AND. ;
       last > solidsegs[ nNext ][ 2 ]
        R_StoreWallRange( solidsegs[ nNext ][ 2 ] + 1, last )
        solidsegs[ start ][ 2 ] := last
    ENDIF

    IF nNext == start
        RETURN
    ENDIF

    DO WHILE nNext != newend
        nNext++
        start++
        solidsegs[ start ] := { solidsegs[ nNext ][ 1 ], ;
                                solidsegs[ nNext ][ 2 ] }
    ENDDO
    newend := start + 1
RETURN

PROCEDURE R_ClipPassWallSegment( first, last )
    LOCAL start

    start := 1
    DO WHILE solidsegs[ start ][ 2 ] < first - 1
        start++
    ENDDO

    IF first < solidsegs[ start ][ 1 ]
        IF last < solidsegs[ start ][ 1 ] - 1
            R_StoreWallRange( first, last )
            RETURN
        ENDIF
        R_StoreWallRange( first, solidsegs[ start ][ 1 ] - 1 )
    ENDIF

    IF last <= solidsegs[ start ][ 2 ]
        RETURN
    ENDIF

    DO WHILE last >= solidsegs[ start + 1 ][ 1 ] - 1
        R_StoreWallRange( solidsegs[ start ][ 2 ] + 1, ;
                          solidsegs[ start + 1 ][ 1 ] - 1 )
        start++
        IF last <= solidsegs[ start ][ 2 ]
            RETURN
        ENDIF
    ENDDO

    R_StoreWallRange( solidsegs[ start ][ 2 ] + 1, last )
RETURN

PROCEDURE R_ClearClipSegs()
    MEMVAR viewwidth
    solidsegs[ 1 ][ 1 ] := -0x7FFFFFFF
    solidsegs[ 1 ][ 2 ] := -1
    solidsegs[ 2 ][ 1 ] := viewwidth
    solidsegs[ 2 ][ 2 ] := 0x7FFFFFFF
    newend := 3
RETURN

PROCEDURE R_AddLine( line )
    LOCAL x1
    LOCAL x2
    LOCAL angle1
    LOCAL angle2
    LOCAL span
    LOCAL tspan
    LOCAL clipsolid := .F.
    MEMVAR backsector
    MEMVAR clipangle
    MEMVAR curline
    MEMVAR frontsector
    MEMVAR rw_angle1
    MEMVAR viewangle
    MEMVAR viewangletox

    curline := line

    angle1 := R_PointToAngle( line:v1:x, line:v1:y )
    angle2 := R_PointToAngle( line:v2:x, line:v2:y )
    span := AsU32( angle1 - angle2 )

    IF span >= ANG180
        RETURN
    ENDIF

    rw_angle1 := angle1
    angle1 := AsU32( angle1 - viewangle )
    angle2 := AsU32( angle2 - viewangle )

    tspan := AsU32( angle1 + clipangle )
    IF tspan > 2 * clipangle
        tspan := AsU32( tspan - 2 * clipangle )
        IF tspan >= span
            RETURN
        ENDIF
        angle1 := clipangle
    ENDIF

    tspan := AsU32( clipangle - angle2 )
    IF tspan > 2 * clipangle
        tspan := AsU32( tspan - 2 * clipangle )
        IF tspan >= span
            RETURN
        ENDIF
        angle2 := AsU32( 0 - clipangle )
    ENDIF

    angle1 := UShr( angle1 + ANG90, ANGLETOFINESHIFT )
    angle2 := UShr( angle2 + ANG90, ANGLETOFINESHIFT )
    x1 := viewangletox[ angle1 + 1 ]
    x2 := viewangletox[ angle2 + 1 ]

    IF x1 == x2
        RETURN
    ENDIF

    backsector := line:backsector
    IF backsector == NIL
        clipsolid := .T.
    ELSEIF backsector:ceilingheight <= frontsector:floorheight .OR. ;
           backsector:floorheight >= frontsector:ceilingheight
        clipsolid := .T.
    ELSEIF backsector:ceilingheight != frontsector:ceilingheight .OR. ;
           backsector:floorheight != frontsector:floorheight
        clipsolid := .F.
    ELSEIF backsector:ceilingpic == frontsector:ceilingpic .AND. ;
           backsector:floorpic == frontsector:floorpic .AND. ;
           backsector:lightlevel == frontsector:lightlevel .AND. ;
           curline:sidedef:midtexture == 0
        RETURN
    ENDIF

    IF clipsolid
        R_ClipSolidWallSegment( x1, x2 - 1 )
    ELSE
        R_ClipPassWallSegment( x1, x2 - 1 )
    ENDIF
RETURN

FUNCTION R_CheckBBox( bspcoord )
    LOCAL boxx
    LOCAL boxy
    LOCAL boxpos
    LOCAL x1
    LOCAL y1
    LOCAL x2
    LOCAL y2
    LOCAL angle1
    LOCAL angle2
    LOCAL span
    LOCAL tspan
    LOCAL start
    LOCAL sx1
    LOCAL sx2
    MEMVAR clipangle
    MEMVAR viewangle
    MEMVAR viewangletox
    MEMVAR viewx
    MEMVAR viewy

    IF viewx <= bspcoord[ BOXLEFT + 1 ]
        boxx := 0
    ELSEIF viewx < bspcoord[ BOXRIGHT + 1 ]
        boxx := 1
    ELSE
        boxx := 2
    ENDIF

    IF viewy >= bspcoord[ BOXTOP + 1 ]
        boxy := 0
    ELSEIF viewy > bspcoord[ BOXBOTTOM + 1 ]
        boxy := 1
    ELSE
        boxy := 2
    ENDIF

    boxpos := boxy * 4 + boxx
    IF boxpos == 5
        RETURN .T.
    ENDIF

    x1 := bspcoord[ checkcoord[ boxpos + 1 ][ 1 ] + 1 ]
    y1 := bspcoord[ checkcoord[ boxpos + 1 ][ 2 ] + 1 ]
    x2 := bspcoord[ checkcoord[ boxpos + 1 ][ 3 ] + 1 ]
    y2 := bspcoord[ checkcoord[ boxpos + 1 ][ 4 ] + 1 ]

    angle1 := AsU32( R_PointToAngle( x1, y1 ) - viewangle )
    angle2 := AsU32( R_PointToAngle( x2, y2 ) - viewangle )
    span := AsU32( angle1 - angle2 )

    IF span >= ANG180
        RETURN .T.
    ENDIF

    tspan := AsU32( angle1 + clipangle )
    IF tspan > 2 * clipangle
        tspan := AsU32( tspan - 2 * clipangle )
        IF tspan >= span
            RETURN .F.
        ENDIF
        angle1 := clipangle
    ENDIF

    tspan := AsU32( clipangle - angle2 )
    IF tspan > 2 * clipangle
        tspan := AsU32( tspan - 2 * clipangle )
        IF tspan >= span
            RETURN .F.
        ENDIF
        angle2 := AsU32( 0 - clipangle )
    ENDIF

    angle1 := UShr( angle1 + ANG90, ANGLETOFINESHIFT )
    angle2 := UShr( angle2 + ANG90, ANGLETOFINESHIFT )
    sx1 := viewangletox[ angle1 + 1 ]
    sx2 := viewangletox[ angle2 + 1 ]

    IF sx1 == sx2
        RETURN .F.
    ENDIF
    sx2--

    start := 1
    DO WHILE solidsegs[ start ][ 2 ] < sx2
        start++
    ENDDO

    IF sx1 >= solidsegs[ start ][ 1 ] .AND. ;
       sx2 <= solidsegs[ start ][ 2 ]
        RETURN .F.
    ENDIF
RETURN .T.

PROCEDURE R_Subsector( num )
    LOCAL count
    LOCAL line
    LOCAL sub
    LOCAL i
    MEMVAR ceilingplane
    MEMVAR floorplane
    MEMVAR frontsector
    MEMVAR segs
    MEMVAR skyflatnum
    MEMVAR sscount
    MEMVAR subsectors
    MEMVAR viewz

    sscount++
    sub := subsectors[ num + 1 ]
    frontsector := sub:sector
    count := sub:numlines

    IF frontsector:floorheight < viewz
        floorplane := R_FindPlane( frontsector:floorheight, ;
                                   frontsector:floorpic, ;
                                   frontsector:lightlevel )
    ELSE
        floorplane := NIL
    ENDIF

    IF frontsector:ceilingheight > viewz .OR. ;
       frontsector:ceilingpic == skyflatnum
        ceilingplane := R_FindPlane( frontsector:ceilingheight, ;
                                     frontsector:ceilingpic, ;
                                     frontsector:lightlevel )
    ELSE
        ceilingplane := NIL
    ENDIF

    R_AddSprites( frontsector )

    FOR i := 0 TO count - 1
        line := segs[ sub:firstline + i + 1 ]
        R_AddLine( line )
    NEXT
RETURN

PROCEDURE R_RenderBSPNode( bspnum )
    LOCAL bsp
    LOCAL side
    MEMVAR nodes
    MEMVAR viewx
    MEMVAR viewy

    IF ( bspnum & NF_SUBSECTOR ) != 0
        IF bspnum == -1
            R_Subsector( 0 )
        ELSE
            R_Subsector( ( bspnum & 0x7FFF ) )
        ENDIF
        RETURN
    ENDIF

    bsp := nodes[ bspnum + 1 ]
    side := R_PointOnSide( viewx, viewy, bsp )

    R_RenderBSPNode( bsp:children[ side + 1 ] )

    IF R_CheckBBox( bsp:bbox[ ( side ^^ 1 ) + 1 ] )
        R_RenderBSPNode( bsp:children[ ( side ^^ 1 ) + 1 ] )
    ENDIF
RETURN

INIT PROCEDURE init_r_bsp
    LOCAL i

    PUBLIC curline
    PUBLIC sidedef
    PUBLIC linedef
    PUBLIC frontsector
    PUBLIC backsector
    PUBLIC drawsegs
    PUBLIC ds_p

    curline := NIL
    sidedef := NIL
    linedef := NIL
    frontsector := NIL
    backsector := NIL
    ds_p := 0
    newend := 0

    drawsegs := {}
    FOR i := 1 TO MAXDRAWSEGS
        AAdd( drawsegs, drawseg_t():New() )
    NEXT

    solidsegs := {}
    FOR i := 1 TO MAXSEGS
        AAdd( solidsegs, { 0, 0 } )
    NEXT

    checkcoord := { ;
        { 3, 0, 2, 1 }, ;
        { 3, 0, 2, 0 }, ;
        { 3, 1, 2, 0 }, ;
        { 0, 0, 0, 0 }, ;
        { 2, 0, 2, 1 }, ;
        { 0, 0, 0, 0 }, ;
        { 3, 1, 3, 0 }, ;
        { 0, 0, 0, 0 }, ;
        { 2, 0, 3, 1 }, ;
        { 2, 1, 3, 1 }, ;
        { 2, 1, 3, 0 }, ;
        { 0, 0, 0, 0 } }
RETURN
