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

STATIC segtextured
STATIC markfloor
STATIC markceiling
STATIC maskedtexture
STATIC toptexture
STATIC bottomtexture
STATIC midtexture
STATIC rw_centerangle
STATIC rw_offset
STATIC rw_scale
STATIC rw_scalestep
STATIC rw_midtexturemid
STATIC rw_toptexturemid
STATIC rw_bottomtexturemid
STATIC worldtop
STATIC worldbottom
STATIC worldhigh
STATIC worldlow
STATIC pixhigh
STATIC pixlow
STATIC pixhighstep
STATIC pixlowstep
STATIC topfrac
STATIC topstep
STATIC bottomfrac
STATIC bottomstep
STATIC maskedtexturecol

#include "r_segs.ch"
#include "r_defs.ch"
#include "r_state.ch"
#include "r_main.ch"
#include "r_bsp.ch"
#include "r_plane.ch"
#include "r_draw.ch"
#include "r_sky.ch"
#include "r_things.ch"
#include "p_local.ch"
#include "doomstat.ch"


#define HEIGHTBITS 12
#define HEIGHTUNIT ( 2 ^ HEIGHTBITS )
#define SHRT_MAX   32767
#define INT_MAX    2147483647
#define INT_MIN    ( -2147483647 - 1 )



STATIC FUNCTION AsU32( n )
RETURN ( n & 0xFFFFFFFF )

/* Vanilla reads past finetangent[4096] into finesine, which follows it in memory. */
STATIC FUNCTION FineTangentAt( nAngle )
    MEMVAR finetangent
    MEMVAR finesine
    IF nAngle < Len( finetangent )
        RETURN finetangent[ nAngle + 1 ]
    ENDIF
    nAngle -= Len( finetangent )
    IF nAngle < Len( finesine )
        RETURN finesine[ nAngle + 1 ]
    ENDIF
RETURN 0

STATIC FUNCTION AsS32( n )
    n := ( n & 0xFFFFFFFF )
    IF n >= 0x80000000
        n -= 0x100000000
    ENDIF
RETURN n

STATIC FUNCTION IfaceCall( xFun )
    IF xFun == NIL
        RETURN NIL
    ENDIF
    IF ValType( xFun ) == "B"
        RETURN Eval( xFun )
    ENDIF
    IF ValType( xFun ) == "C"
        RETURN &( xFun )()
    ENDIF
RETURN NIL

STATIC FUNCTION NewScreenArray( nValue )
    LOCAL aResult := {}
    LOCAL i

    FOR i := 1 TO SCREENWIDTH
        AAdd( aResult, nValue )
    NEXT
RETURN aResult

STATIC FUNCTION ReserveOpening( aSource, nStart, nStop )
    LOCAL aResult := NewScreenArray( 0 )
    LOCAL x
    MEMVAR lastopening
    MEMVAR openings

    IF ValType( openings ) != "A"
        openings := {}
    ENDIF
    FOR x := nStart TO nStop
        aResult[ x + 1 ] := aSource[ x + 1 ]
        IF Len( openings ) < lastopening + 1
            AAdd( openings, aResult[ x + 1 ] )
        ELSE
            openings[ lastopening + 1 ] := aResult[ x + 1 ]
        ENDIF
        lastopening++
    NEXT
RETURN aResult

PROCEDURE R_RenderMaskedSegRange( ds, x1, x2 )
    LOCAL index
    LOCAL col
    LOCAL lightnum
    LOCAL texnum
    MEMVAR backsector
    MEMVAR centeryfrac
    MEMVAR curline
    MEMVAR dc_colormap
    MEMVAR dc_iscale
    MEMVAR dc_texturemid
    MEMVAR dc_x
    MEMVAR extralight
    MEMVAR fixedcolormap
    MEMVAR frontsector
    MEMVAR mceilingclip
    MEMVAR mfloorclip
    MEMVAR scalelight
    MEMVAR sprtopscreen
    MEMVAR spryscale
    MEMVAR textureheight
    MEMVAR texturetranslation
    MEMVAR viewz
    MEMVAR walllights

    curline := ds:curline
    frontsector := curline:frontsector
    backsector := curline:backsector
    texnum := texturetranslation[ curline:sidedef:midtexture + 1 ]

    lightnum := Shar( frontsector:lightlevel, LIGHTSEGSHIFT ) + extralight
    IF curline:v1:y == curline:v2:y
        lightnum--
    ELSEIF curline:v1:x == curline:v2:x
        lightnum++
    ENDIF

    IF lightnum < 0
        walllights := scalelight[ 1 ]
    ELSEIF lightnum >= LIGHTLEVELS
        walllights := scalelight[ LIGHTLEVELS ]
    ELSE
        walllights := scalelight[ lightnum + 1 ]
    ENDIF

    maskedtexturecol := ds:maskedtexturecol
    rw_scalestep := ds:scalestep
    spryscale := ds:scale1 + ( x1 - ds:x1 ) * rw_scalestep
    mfloorclip := ds:sprbottomclip
    mceilingclip := ds:sprtopclip

    IF ( curline:linedef:flags & ML_DONTPEGBOTTOM ) != 0
        dc_texturemid := Max( frontsector:floorheight, backsector:floorheight )
        dc_texturemid += textureheight[ texnum + 1 ] - viewz
    ELSE
        dc_texturemid := Min( frontsector:ceilingheight, backsector:ceilingheight )
        dc_texturemid -= viewz
    ENDIF
    dc_texturemid += curline:sidedef:rowoffset

    IF ! Empty( fixedcolormap )
        dc_colormap := fixedcolormap
    ENDIF

    FOR dc_x := x1 TO x2
        IF maskedtexturecol[ dc_x + 1 ] != SHRT_MAX
            IF Empty( fixedcolormap )
                index := UShr( spryscale, LIGHTSCALESHIFT )
                IF index >= MAXLIGHTSCALE
                    index := MAXLIGHTSCALE - 1
                ENDIF
                dc_colormap := walllights[ index + 1 ]
            ENDIF

            sprtopscreen := centeryfrac - FixedMul( dc_texturemid, spryscale )
            IF ( spryscale & 0xFFFFFFFF ) != 0
                dc_iscale := Int( 0xFFFFFFFF / ( spryscale & 0xFFFFFFFF ) )
            ELSE
                dc_iscale := 0
            ENDIF

            col := R_GetColumnPosts( texnum, maskedtexturecol[ dc_x + 1 ] )
            R_DrawMaskedColumn( col )
            maskedtexturecol[ dc_x + 1 ] := SHRT_MAX
        ENDIF
        spryscale += rw_scalestep
    NEXT
RETURN

PROCEDURE R_RenderSegLoop()
    LOCAL angle
    LOCAL index
    LOCAL yl
    LOCAL yh
    LOCAL mid
    LOCAL texturecolumn
    LOCAL top
    LOCAL bottom
    MEMVAR ceilingclip
    MEMVAR ceilingplane
    MEMVAR colfunc
    MEMVAR dc_colormap
    MEMVAR dc_iscale
    MEMVAR dc_source
    MEMVAR dc_texturemid
    MEMVAR dc_x
    MEMVAR dc_yh
    MEMVAR dc_yl
    MEMVAR finetangent
    MEMVAR floorclip
    MEMVAR floorplane
    MEMVAR rw_distance
    MEMVAR rw_stopx
    MEMVAR rw_x
    MEMVAR walllights
    MEMVAR xtoviewangle

    DO WHILE rw_x < rw_stopx
        yl := Shar( topfrac + HEIGHTUNIT - 1, HEIGHTBITS )
        IF yl < ceilingclip[ rw_x + 1 ] + 1
            yl := ceilingclip[ rw_x + 1 ] + 1
        ENDIF

        IF markceiling
            top := ceilingclip[ rw_x + 1 ] + 1
            bottom := yl - 1
            IF bottom >= floorclip[ rw_x + 1 ]
                bottom := floorclip[ rw_x + 1 ] - 1
            ENDIF
            IF top <= bottom
                ceilingplane:top[ rw_x + 1 ] := top
                ceilingplane:bottom[ rw_x + 1 ] := bottom
            ENDIF
        ENDIF

        yh := Shar( bottomfrac, HEIGHTBITS )
        IF yh >= floorclip[ rw_x + 1 ]
            yh := floorclip[ rw_x + 1 ] - 1
        ENDIF

        IF markfloor
            top := yh + 1
            bottom := floorclip[ rw_x + 1 ] - 1
            IF top <= ceilingclip[ rw_x + 1 ]
                top := ceilingclip[ rw_x + 1 ] + 1
            ENDIF
            IF top <= bottom
                floorplane:top[ rw_x + 1 ] := top
                floorplane:bottom[ rw_x + 1 ] := bottom
            ENDIF
        ENDIF

        IF segtextured
            angle := UShr( rw_centerangle + xtoviewangle[ rw_x + 1 ], ;
                           ANGLETOFINESHIFT )
            texturecolumn := rw_offset - FixedMul( FineTangentAt( angle ), rw_distance )
            texturecolumn := Shar( texturecolumn, FRACBITS )

            index := UShr( rw_scale, LIGHTSCALESHIFT )
            IF index >= MAXLIGHTSCALE
                index := MAXLIGHTSCALE - 1
            ENDIF
            dc_colormap := walllights[ index + 1 ]
            dc_x := rw_x
            IF ( rw_scale & 0xFFFFFFFF ) != 0
                dc_iscale := Int( 0xFFFFFFFF / ( rw_scale & 0xFFFFFFFF ) )
            ELSE
                dc_iscale := 0
            ENDIF
        ELSE
            texturecolumn := 0
        ENDIF

        IF midtexture != 0
            dc_yl := yl
            dc_yh := yh
            dc_texturemid := rw_midtexturemid
            dc_source := R_GetColumn( midtexture, texturecolumn )
            IfaceCall( colfunc )
            ceilingclip[ rw_x + 1 ] := viewheight
            floorclip[ rw_x + 1 ] := -1
        ELSE
            IF toptexture != 0
                mid := Shar( pixhigh, HEIGHTBITS )
                pixhigh += pixhighstep
                IF mid >= floorclip[ rw_x + 1 ]
                    mid := floorclip[ rw_x + 1 ] - 1
                ENDIF
                IF mid >= yl
                    dc_yl := yl
                    dc_yh := mid
                    dc_texturemid := rw_toptexturemid
                    dc_source := R_GetColumn( toptexture, texturecolumn )
                    IfaceCall( colfunc )
                    ceilingclip[ rw_x + 1 ] := mid
                ELSE
                    ceilingclip[ rw_x + 1 ] := yl - 1
                ENDIF
            ELSEIF markceiling
                ceilingclip[ rw_x + 1 ] := yl - 1
            ENDIF

            IF bottomtexture != 0
                mid := Shar( pixlow + HEIGHTUNIT - 1, HEIGHTBITS )
                pixlow += pixlowstep
                IF mid <= ceilingclip[ rw_x + 1 ]
                    mid := ceilingclip[ rw_x + 1 ] + 1
                ENDIF
                IF mid <= yh
                    dc_yl := mid
                    dc_yh := yh
                    dc_texturemid := rw_bottomtexturemid
                    dc_source := R_GetColumn( bottomtexture, texturecolumn )
                    IfaceCall( colfunc )
                    floorclip[ rw_x + 1 ] := mid
                ELSE
                    floorclip[ rw_x + 1 ] := yh + 1
                ENDIF
            ELSEIF markfloor
                floorclip[ rw_x + 1 ] := yh + 1
            ENDIF

            IF maskedtexture
                maskedtexturecol[ rw_x + 1 ] := texturecolumn
            ENDIF
        ENDIF

        rw_scale += rw_scalestep
        topfrac += topstep
        bottomfrac += bottomstep
        rw_x++
    ENDDO
RETURN

PROCEDURE R_StoreWallRange( start, stop )
    LOCAL hyp
    LOCAL sineval
    LOCAL distangle
    LOCAL offsetangle
    LOCAL vtop
    LOCAL lightnum
    LOCAL ds
    MEMVAR backsector
    MEMVAR ceilingclip
    MEMVAR ceilingplane
    MEMVAR centeryfrac
    MEMVAR curline
    MEMVAR drawsegs
    MEMVAR ds_p
    MEMVAR extralight
    MEMVAR finesine
    MEMVAR fixedcolormap
    MEMVAR floorclip
    MEMVAR floorplane
    MEMVAR frontsector
    MEMVAR lastopening
    MEMVAR linedef
    MEMVAR negonearray
    MEMVAR openings
    MEMVAR rw_angle1
    MEMVAR rw_distance
    MEMVAR rw_normalangle
    MEMVAR rw_stopx
    MEMVAR rw_x
    MEMVAR scalelight
    MEMVAR screenheightarray
    MEMVAR sidedef
    MEMVAR skyflatnum
    MEMVAR textureheight
    MEMVAR texturetranslation
    MEMVAR viewangle
    MEMVAR viewz
    MEMVAR walllights
    MEMVAR xtoviewangle

    IF ds_p >= MAXDRAWSEGS
        RETURN
    ENDIF

    ds_p++
    IF Len( drawsegs ) < ds_p
        AAdd( drawsegs, drawseg_t():New() )
    ENDIF
    ds := drawsegs[ ds_p ]
    IF ds == NIL
        ds := drawseg_t():New()
        drawsegs[ ds_p ] := ds
    ENDIF

    sidedef := curline:sidedef
    linedef := curline:linedef
    linedef:flags := ( linedef:flags | ML_MAPPED )

    rw_normalangle := AsU32( curline:angle + ANG90 )
    offsetangle := Abs( AsS32( rw_normalangle - rw_angle1 ) )
    IF offsetangle > ANG90
        offsetangle := ANG90
    ENDIF

    distangle := ANG90 - offsetangle
    hyp := R_PointToDist( curline:v1:x, curline:v1:y )
    sineval := finesine[ UShr( distangle, ANGLETOFINESHIFT ) + 1 ]
    rw_distance := FixedMul( hyp, sineval )

    rw_x := start
    ds:x1 := start
    ds:x2 := stop
    ds:curline := curline
    rw_stopx := stop + 1

    rw_scale := R_ScaleFromGlobalAngle( ;
        AsU32( viewangle + xtoviewangle[ start + 1 ] ) )
    ds:scale1 := rw_scale

    IF stop > start
        ds:scale2 := R_ScaleFromGlobalAngle( ;
            AsU32( viewangle + xtoviewangle[ stop + 1 ] ) )
        rw_scalestep := Int( ( ds:scale2 - rw_scale ) / ( stop - start ) )
        ds:scalestep := rw_scalestep
    ELSE
        ds:scale2 := ds:scale1
        rw_scalestep := 0
        ds:scalestep := 0
    ENDIF

    worldtop := frontsector:ceilingheight - viewz
    worldbottom := frontsector:floorheight - viewz
    midtexture := 0
    toptexture := 0
    bottomtexture := 0
    maskedtexture := .F.
    ds:maskedtexturecol := NIL

    IF backsector == NIL
        midtexture := texturetranslation[ sidedef:midtexture + 1 ]
        markfloor := .T.
        markceiling := .T.

        IF ( linedef:flags & ML_DONTPEGBOTTOM ) != 0
            vtop := frontsector:floorheight + textureheight[ sidedef:midtexture + 1 ]
            rw_midtexturemid := vtop - viewz
        ELSE
            rw_midtexturemid := worldtop
        ENDIF
        rw_midtexturemid += sidedef:rowoffset

        ds:silhouette := SIL_BOTH
        ds:sprtopclip := screenheightarray
        ds:sprbottomclip := negonearray
        ds:bsilheight := INT_MAX
        ds:tsilheight := INT_MIN
    ELSE
        ds:sprtopclip := NIL
        ds:sprbottomclip := NIL
        ds:silhouette := SIL_NONE

        IF frontsector:floorheight > backsector:floorheight
            ds:silhouette := SIL_BOTTOM
            ds:bsilheight := frontsector:floorheight
        ELSEIF backsector:floorheight > viewz
            ds:silhouette := SIL_BOTTOM
            ds:bsilheight := INT_MAX
        ENDIF

        IF frontsector:ceilingheight < backsector:ceilingheight
            ds:silhouette := ( ds:silhouette | SIL_TOP )
            ds:tsilheight := frontsector:ceilingheight
        ELSEIF backsector:ceilingheight < viewz
            ds:silhouette := ( ds:silhouette | SIL_TOP )
            ds:tsilheight := INT_MIN
        ENDIF

        IF backsector:ceilingheight <= frontsector:floorheight
            ds:sprbottomclip := negonearray
            ds:bsilheight := INT_MAX
            ds:silhouette := ( ds:silhouette | SIL_BOTTOM )
        ENDIF
        IF backsector:floorheight >= frontsector:ceilingheight
            ds:sprtopclip := screenheightarray
            ds:tsilheight := INT_MIN
            ds:silhouette := ( ds:silhouette | SIL_TOP )
        ENDIF

        worldhigh := backsector:ceilingheight - viewz
        worldlow := backsector:floorheight - viewz
        IF frontsector:ceilingpic == skyflatnum ;
           .AND. backsector:ceilingpic == skyflatnum
            worldtop := worldhigh
        ENDIF

        markfloor := worldlow != worldbottom ;
                     .OR. backsector:floorpic != frontsector:floorpic ;
                     .OR. backsector:lightlevel != frontsector:lightlevel
        markceiling := worldhigh != worldtop ;
                       .OR. backsector:ceilingpic != frontsector:ceilingpic ;
                       .OR. backsector:lightlevel != frontsector:lightlevel

        IF backsector:ceilingheight <= frontsector:floorheight ;
           .OR. backsector:floorheight >= frontsector:ceilingheight
            markfloor := .T.
            markceiling := .T.
        ENDIF

        IF worldhigh < worldtop
            toptexture := texturetranslation[ sidedef:toptexture + 1 ]
            IF ( linedef:flags & ML_DONTPEGTOP ) != 0
                rw_toptexturemid := worldtop
            ELSE
                vtop := backsector:ceilingheight + textureheight[ sidedef:toptexture + 1 ]
                rw_toptexturemid := vtop - viewz
            ENDIF
        ELSE
            rw_toptexturemid := 0
        ENDIF

        IF worldlow > worldbottom
            bottomtexture := texturetranslation[ sidedef:bottomtexture + 1 ]
            IF ( linedef:flags & ML_DONTPEGBOTTOM ) != 0
                rw_bottomtexturemid := worldtop
            ELSE
                rw_bottomtexturemid := worldlow
            ENDIF
        ELSE
            rw_bottomtexturemid := 0
        ENDIF

        rw_toptexturemid += sidedef:rowoffset
        rw_bottomtexturemid += sidedef:rowoffset

        IF sidedef:midtexture != 0
            maskedtexture := .T.
            maskedtexturecol := NewScreenArray( SHRT_MAX )
            ds:maskedtexturecol := maskedtexturecol
            IF ValType( openings ) != "A"
                openings := {}
            ENDIF
            DO WHILE Len( openings ) < lastopening + rw_stopx - rw_x
                AAdd( openings, SHRT_MAX )
            ENDDO
            lastopening += rw_stopx - rw_x
        ENDIF
    ENDIF

    segtextured := midtexture != 0 .OR. toptexture != 0 ;
                   .OR. bottomtexture != 0 .OR. maskedtexture

    IF segtextured
        offsetangle := AsU32( rw_normalangle - rw_angle1 )
        IF offsetangle > ANG180
            offsetangle := AsU32( 0 - offsetangle )
        ENDIF
        IF offsetangle > ANG90
            offsetangle := ANG90
        ENDIF

        sineval := finesine[ UShr( offsetangle, ANGLETOFINESHIFT ) + 1 ]
        rw_offset := FixedMul( hyp, sineval )
        IF AsU32( rw_normalangle - rw_angle1 ) < ANG180
            rw_offset := -rw_offset
        ENDIF
        rw_offset += sidedef:textureoffset + curline:offset
        rw_centerangle := AsU32( ANG90 + viewangle - rw_normalangle )

        IF Empty( fixedcolormap )
            lightnum := Shar( frontsector:lightlevel, LIGHTSEGSHIFT ) + extralight
            IF curline:v1:y == curline:v2:y
                lightnum--
            ELSEIF curline:v1:x == curline:v2:x
                lightnum++
            ENDIF
            IF lightnum < 0
                walllights := scalelight[ 1 ]
            ELSEIF lightnum >= LIGHTLEVELS
                walllights := scalelight[ LIGHTLEVELS ]
            ELSE
                walllights := scalelight[ lightnum + 1 ]
            ENDIF
        ENDIF
    ENDIF

    IF frontsector:floorheight >= viewz
        markfloor := .F.
    ENDIF
    IF frontsector:ceilingheight <= viewz ;
       .AND. frontsector:ceilingpic != skyflatnum
        markceiling := .F.
    ENDIF

    worldtop := Shar( worldtop, 4 )
    worldbottom := Shar( worldbottom, 4 )
    topstep := -FixedMul( rw_scalestep, worldtop )
    topfrac := Shar( centeryfrac, 4 ) - FixedMul( worldtop, rw_scale )
    bottomstep := -FixedMul( rw_scalestep, worldbottom )
    bottomfrac := Shar( centeryfrac, 4 ) - FixedMul( worldbottom, rw_scale )

    IF backsector != NIL
        worldhigh := Shar( worldhigh, 4 )
        worldlow := Shar( worldlow, 4 )
        IF worldhigh < worldtop
            pixhigh := Shar( centeryfrac, 4 ) - FixedMul( worldhigh, rw_scale )
            pixhighstep := -FixedMul( rw_scalestep, worldhigh )
        ENDIF
        IF worldlow > worldbottom
            pixlow := Shar( centeryfrac, 4 ) - FixedMul( worldlow, rw_scale )
            pixlowstep := -FixedMul( rw_scalestep, worldlow )
        ENDIF
    ENDIF

    IF markceiling
        ceilingplane := R_CheckPlane( ceilingplane, rw_x, rw_stopx - 1 )
    ENDIF
    IF markfloor
        floorplane := R_CheckPlane( floorplane, rw_x, rw_stopx - 1 )
    ENDIF

    R_RenderSegLoop()

    IF ( ( ds:silhouette & SIL_TOP ) != 0 .OR. maskedtexture ) ;
       .AND. ds:sprtopclip == NIL
        ds:sprtopclip := ReserveOpening( ceilingclip, start, rw_stopx - 1 )
    ENDIF
    IF ( ( ds:silhouette & SIL_BOTTOM ) != 0 .OR. maskedtexture ) ;
       .AND. ds:sprbottomclip == NIL
        ds:sprbottomclip := ReserveOpening( floorclip, start, rw_stopx - 1 )
    ENDIF

    IF maskedtexture .AND. ( ds:silhouette & SIL_TOP ) == 0
        ds:silhouette := ( ds:silhouette | SIL_TOP )
        ds:tsilheight := INT_MIN
    ENDIF
    IF maskedtexture .AND. ( ds:silhouette & SIL_BOTTOM ) == 0
        ds:silhouette := ( ds:silhouette | SIL_BOTTOM )
        ds:bsilheight := INT_MAX
    ENDIF
RETURN

INIT PROCEDURE init_r_segs
    PUBLIC walllights

    segtextured := .F.
    markfloor := .F.
    markceiling := .F.
    maskedtexture := .F.
    toptexture := 0
    bottomtexture := 0
    midtexture := 0
    rw_centerangle := 0
    rw_offset := 0
    rw_scale := 0
    rw_scalestep := 0
    rw_midtexturemid := 0
    rw_toptexturemid := 0
    rw_bottomtexturemid := 0
    worldtop := 0
    worldbottom := 0
    worldhigh := 0
    worldlow := 0
    pixhigh := 0
    pixlow := 0
    pixhighstep := 0
    pixlowstep := 0
    topfrac := 0
    topstep := 0
    bottomfrac := 0
    bottomstep := 0
    walllights := {}
    maskedtexturecol := NIL
RETURN
