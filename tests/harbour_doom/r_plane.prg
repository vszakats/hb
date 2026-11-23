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

STATIC planezlight
STATIC planeheight
STATIC spanstart
STATIC spanstop
STATIC basexscale
STATIC baseyscale
STATIC cachedheight
STATIC cacheddistance
STATIC cachedxstep
STATIC cachedystep

#include "m_fixed.ch"
#include "r_local.ch"
#include "r_sky.ch"

#define MAXOPENINGS      ( SCREENWIDTH * 64 )
#define ANGLETOFINESHIFT 19
#define ANG90            0x40000000
#define PU_STATIC        1


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

STATIC FUNCTION PlaneTopAt( pl, x )
    IF x < 0 .OR. x >= SCREENWIDTH
        RETURN 255
    ENDIF
RETURN pl:top[ x + 1 ]

STATIC FUNCTION PlaneBottomAt( pl, x )
    IF x < 0 .OR. x >= SCREENWIDTH
        RETURN 0
    ENDIF
RETURN pl:bottom[ x + 1 ]

PROCEDURE R_InitPlanes()
RETURN

PROCEDURE R_MapPlane( y, x1, x2 )
    MEMVAR yslope, distscale, viewangle, xtoviewangle, viewx, viewy
    MEMVAR finecosine, finesine, fixedcolormap, ds_colormap
    MEMVAR ds_xstep, ds_ystep, ds_xfrac, ds_yfrac, ds_y, ds_x1, ds_x2
    MEMVAR spanfunc
    LOCAL angle
    LOCAL distance
    LOCAL length
    LOCAL index

    IF planeheight != cachedheight[ y + 1 ]
        cachedheight[ y + 1 ] := planeheight
        distance := FixedMul( planeheight, yslope[ y + 1 ] )
        cacheddistance[ y + 1 ] := distance
        ds_xstep := FixedMul( distance, basexscale )
        cachedxstep[ y + 1 ] := ds_xstep
        ds_ystep := FixedMul( distance, baseyscale )
        cachedystep[ y + 1 ] := ds_ystep
    ELSE
        distance := cacheddistance[ y + 1 ]
        ds_xstep := cachedxstep[ y + 1 ]
        ds_ystep := cachedystep[ y + 1 ]
    ENDIF

    length := FixedMul( distance, distscale[ x1 + 1 ] )
    angle := UShr( viewangle + xtoviewangle[ x1 + 1 ], ANGLETOFINESHIFT )
    ds_xfrac := viewx + FixedMul( finecosine[ angle + 1 ], length )
    ds_yfrac := -viewy - FixedMul( finesine[ angle + 1 ], length )

    IF ! Empty( fixedcolormap )
        ds_colormap := fixedcolormap
    ELSE
        index := UShr( distance, LIGHTZSHIFT )
        IF index >= MAXLIGHTZ
            index := MAXLIGHTZ - 1
        ENDIF
        ds_colormap := planezlight[ index + 1 ]
    ENDIF

    ds_y := y
    ds_x1 := x1
    ds_x2 := x2
    IfaceCall( spanfunc )
RETURN

PROCEDURE R_ClearPlanes()
    MEMVAR viewwidth, viewheight, floorclip, ceilingclip
    MEMVAR lastvisplane, lastopening, viewangle, finecosine, finesine
    MEMVAR centerxfrac
    LOCAL i
    LOCAL angle

    FOR i := 0 TO viewwidth - 1
        floorclip[ i + 1 ] := viewheight
        ceilingclip[ i + 1 ] := -1
    NEXT

    lastvisplane := 0
    lastopening := 0
    AFill( cachedheight, 0 )

    angle := UShr( viewangle - ANG90, ANGLETOFINESHIFT )
    basexscale := FixedDiv( finecosine[ angle + 1 ], centerxfrac )
    baseyscale := -FixedDiv( finesine[ angle + 1 ], centerxfrac )
RETURN

FUNCTION R_FindPlane( height, picnum, lightlevel )
    MEMVAR skyflatnum, visplanes, lastvisplane
    LOCAL i
    LOCAL check

    IF picnum == skyflatnum
        height := 0
        lightlevel := 0
    ENDIF

    FOR i := 1 TO lastvisplane
        check := visplanes[ i ]
        IF height == check:height .AND. picnum == check:picnum .AND. ;
                lightlevel == check:lightlevel
            RETURN check
        ENDIF
    NEXT

    IF lastvisplane >= MAXVISPLANES
        I_Error( "R_FindPlane: no more visplanes" )
        RETURN NIL
    ENDIF

    lastvisplane++
    check := visplanes[ lastvisplane ]
    check:height := height
    check:picnum := picnum
    check:lightlevel := lightlevel
    check:minx := SCREENWIDTH
    check:maxx := -1
    AFill( check:top, 255 )
RETURN check

FUNCTION R_CheckPlane( pl, start, stop )
    MEMVAR visplanes, lastvisplane
    LOCAL intrl
    LOCAL intrh
    LOCAL unionl
    LOCAL unionh
    LOCAL x

    IF start < pl:minx
        intrl := pl:minx
        unionl := start
    ELSE
        unionl := pl:minx
        intrl := start
    ENDIF

    IF stop > pl:maxx
        intrh := pl:maxx
        unionh := stop
    ELSE
        unionh := pl:maxx
        intrh := stop
    ENDIF

    x := intrl
    DO WHILE x <= intrh
        IF pl:top[ x + 1 ] != 255
            EXIT
        ENDIF
        x++
    ENDDO

    IF x > intrh
        pl:minx := unionl
        pl:maxx := unionh
        RETURN pl
    ENDIF

    IF lastvisplane >= MAXVISPLANES
        I_Error( "R_CheckPlane: no more visplanes" )
        RETURN NIL
    ENDIF

    lastvisplane++
    visplanes[ lastvisplane ]:height := pl:height
    visplanes[ lastvisplane ]:picnum := pl:picnum
    visplanes[ lastvisplane ]:lightlevel := pl:lightlevel
    pl := visplanes[ lastvisplane ]
    pl:minx := start
    pl:maxx := stop
    AFill( pl:top, 255 )
RETURN pl

PROCEDURE R_MakeSpans( x, t1, b1, t2, b2 )
    DO WHILE t1 < t2 .AND. t1 <= b1
        R_MapPlane( t1, spanstart[ t1 + 1 ], x - 1 )
        t1++
    ENDDO
    DO WHILE b1 > b2 .AND. b1 >= t1
        R_MapPlane( b1, spanstart[ b1 + 1 ], x - 1 )
        b1--
    ENDDO
    DO WHILE t2 < t1 .AND. t2 <= b2
        spanstart[ t2 + 1 ] := x
        t2++
    ENDDO
    DO WHILE b2 > b1 .AND. b2 >= t2
        spanstart[ b2 + 1 ] := x
        b2--
    ENDDO
RETURN

PROCEDURE R_DrawPlanes()
    MEMVAR visplanes, lastvisplane, skyflatnum, pspriteiscale, detailshift
    MEMVAR dc_iscale, dc_colormap, colormaps, dc_texturemid, skytexturemid
    MEMVAR dc_yl, dc_yh, dc_x, dc_source, viewangle, xtoviewangle
    MEMVAR skytexture, colfunc, firstflat, flattranslation, ds_source
    MEMVAR viewz, extralight, zlight
    LOCAL pl
    LOCAL light
    LOCAL x
    LOCAL stop
    LOCAL angle
    LOCAL lumpnum
    LOCAL i
    LOCAL t1
    LOCAL b1
    LOCAL t2
    LOCAL b2

    FOR i := 1 TO lastvisplane
        pl := visplanes[ i ]
        IF pl:minx > pl:maxx
            LOOP
        ENDIF

        IF pl:picnum == skyflatnum
            dc_iscale := Shar( pspriteiscale, detailshift )
            dc_colormap := colormaps
            dc_texturemid := skytexturemid

            FOR x := pl:minx TO pl:maxx
                dc_yl := pl:top[ x + 1 ]
                dc_yh := pl:bottom[ x + 1 ]
                IF dc_yl <= dc_yh
                    angle := UShr( viewangle + xtoviewangle[ x + 1 ], ANGLETOSKYSHIFT )
                    dc_x := x
                    dc_source := R_GetColumn( skytexture, angle )
                    IfaceCall( colfunc )
                ENDIF
            NEXT
            LOOP
        ENDIF

        lumpnum := firstflat + flattranslation[ pl:picnum + 1 ]
        ds_source := W_CacheLumpNum( lumpnum, PU_STATIC )
        planeheight := Abs( pl:height - viewz )
        light := Shar( pl:lightlevel, LIGHTSEGSHIFT ) + extralight
        IF light >= LIGHTLEVELS
            light := LIGHTLEVELS - 1
        ENDIF
        IF light < 0
            light := 0
        ENDIF
        planezlight := zlight[ light + 1 ]

        stop := pl:maxx + 1
        FOR x := pl:minx TO stop
            t1 := PlaneTopAt( pl, x - 1 )
            b1 := PlaneBottomAt( pl, x - 1 )
            t2 := PlaneTopAt( pl, x )
            b2 := PlaneBottomAt( pl, x )
            R_MakeSpans( x, t1, b1, t2, b2 )
        NEXT

        W_ReleaseLumpNum( lumpnum )
    NEXT
RETURN

INIT PROCEDURE init_r_plane
    LOCAL i

    PUBLIC floorfunc, ceilingfunc
    PUBLIC visplanes, lastvisplane, floorplane, ceilingplane
    PUBLIC openings, lastopening, floorclip, ceilingclip
    PUBLIC yslope, distscale

    floorfunc := NIL
    ceilingfunc := NIL
    visplanes := Array( MAXVISPLANES )
    FOR i := 1 TO MAXVISPLANES
        visplanes[ i ] := visplane_t():New()
    NEXT
    lastvisplane := 0
    floorplane := NIL
    ceilingplane := NIL
    openings := Array( MAXOPENINGS )
    AFill( openings, 0 )
    lastopening := 0
    floorclip := Array( SCREENWIDTH )
    AFill( floorclip, 0 )
    ceilingclip := Array( SCREENWIDTH )
    AFill( ceilingclip, 0 )
    spanstart := Array( SCREENHEIGHT )
    AFill( spanstart, 0 )
    spanstop := Array( SCREENHEIGHT )
    AFill( spanstop, 0 )
    yslope := Array( SCREENHEIGHT )
    AFill( yslope, 0 )
    distscale := Array( SCREENWIDTH )
    AFill( distscale, 0 )
    planezlight := {}
    planeheight := 0
    basexscale := 0
    baseyscale := 0
    cachedheight := Array( SCREENHEIGHT )
    AFill( cachedheight, 0 )
    cacheddistance := Array( SCREENHEIGHT )
    AFill( cacheddistance, 0 )
    cachedxstep := Array( SCREENHEIGHT )
    AFill( cachedxstep, 0 )
    cachedystep := Array( SCREENHEIGHT )
    AFill( cachedystep, 0 )
RETURN
