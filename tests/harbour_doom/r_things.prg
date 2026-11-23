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

STATIC clipbot
STATIC cliptop
STATIC spritelights
STATIC sprtemp
STATIC maxframe
STATIC spritename
STATIC vissprites
STATIC vissprite_p
STATIC overflowsprite
STATIC vsprsortedhead

#include "r_things.ch"
#include "r_defs.ch"
#include "r_state.ch"
#include "r_main.ch"
#include "r_bsp.ch"
#include "r_plane.ch"
#include "r_draw.ch"
#include "r_segs.ch"
#include "p_local.ch"
#include "p_pspr.ch"
#include "doomstat.ch"
#include "info.ch"
#include "d_player.ch"

#define MINZ        ( FRACUNIT * 4 )
#define BASEYCENTER 100
#define PU_CACHE    8
#define INT_MAX     2147483647




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

STATIC FUNCTION ByteAt( cData, nPos )
    IF ValType( cData ) != "C" .OR. nPos < 1 .OR. nPos > Len( cData )
        RETURN 255
    ENDIF
RETURN Asc( SubStr( cData, nPos, 1 ) )

STATIC FUNCTION PeekLong( cData, nPos )
    LOCAL nOff
    LOCAL n

    IF ValType( cData ) != "C" .OR. nPos < 1 .OR. nPos + 3 > Len( cData )
        RETURN 0
    ENDIF
    nOff := nPos - 1
    n := Asc( SubStr( cData, nOff + 1, 1 ) ) ;
       + Asc( SubStr( cData, nOff + 2, 1 ) ) * 256 ;
       + Asc( SubStr( cData, nOff + 3, 1 ) ) * 65536 ;
       + Asc( SubStr( cData, nOff + 4, 1 ) ) * 16777216
    IF n >= 2147483648
        n := n - 4294967296
    ENDIF
RETURN n

STATIC FUNCTION NewFilledArray( nLen, xValue )
    LOCAL aResult := {}
    LOCAL i

    FOR i := 1 TO nLen
        AAdd( aResult, xValue )
    NEXT
RETURN aResult

STATIC FUNCTION SliceArray( aSource, nStart, nCount )
    LOCAL aResult := {}
    LOCAL i

    FOR i := 0 TO nCount - 1
        IF nStart + i > Len( aSource )
            EXIT
        ENDIF
        AAdd( aResult, aSource[ nStart + i ] )
    NEXT
RETURN aResult

STATIC PROCEDURE ResetSpriteTemp()
    LOCAL i

    sprtemp := {}
    FOR i := 1 TO 29
        AAdd( sprtemp, spriteframe_t():New() )
        sprtemp[ i ]:rotate := -1
        sprtemp[ i ]:lump := NewFilledArray( 8, -1 )
        sprtemp[ i ]:flip := NewFilledArray( 8, -1 )
    NEXT
RETURN

STATIC FUNCTION CloneSpriteFrame( src )
    LOCAL dst := spriteframe_t():New()

    dst:rotate := src:rotate
    dst:lump := AClone( src:lump )
    dst:flip := AClone( src:flip )
RETURN dst

PROCEDURE R_InstallSpriteLump( lump, frame, rotation, flipped )
    LOCAL r
    LOCAL sf
    MEMVAR firstspritelump

    IF frame >= 29 .OR. rotation > 8 .OR. frame < 0 .OR. rotation < 0
        I_Error( "R_InstallSpriteLump: Bad frame characters in lump " + hb_ntos( lump ) )
        RETURN
    ENDIF

    IF frame > maxframe
        maxframe := frame
    ENDIF
    sf := sprtemp[ frame + 1 ]

    IF rotation == 0
        IF sf:rotate == 0
            I_Error( "R_InitSprites: Sprite " + spritename + ;
                     " frame " + Chr( Asc( "A" ) + frame ) + ;
                     " has multip rot=0 lump" )
        ENDIF
        IF sf:rotate == 1
            I_Error( "R_InitSprites: Sprite " + spritename + ;
                     " frame " + Chr( Asc( "A" ) + frame ) + ;
                     " has rotations and a rot=0 lump" )
        ENDIF

        sf:rotate := 0
        FOR r := 1 TO 8
            sf:lump[ r ] := lump - firstspritelump
            sf:flip[ r ] := iif( flipped, 1, 0 )
        NEXT
        RETURN
    ENDIF

    IF sf:rotate == 0
        I_Error( "R_InitSprites: Sprite " + spritename + ;
                 " frame " + Chr( Asc( "A" ) + frame ) + ;
                 " has rotations and a rot=0 lump" )
    ENDIF

    sf:rotate := 1
    IF sf:lump[ rotation ] != -1
        I_Error( "R_InitSprites: Sprite " + spritename + " has two lumps mapped" )
    ENDIF
    sf:lump[ rotation ] := lump - firstspritelump
    sf:flip[ rotation ] := iif( flipped, 1, 0 )
RETURN

PROCEDURE R_InitSpriteDefs( namelist )
    LOCAL i
    LOCAL l
    LOCAL frame
    LOCAL rotation
    LOCAL start
    LOCAL finish
    LOCAL patched
    LOCAL name
    LOCAL sprdef
    LOCAL nCount := 0
    MEMVAR firstspritelump
    MEMVAR lastspritelump
    MEMVAR lumpinfo
    MEMVAR modifiedgame
    MEMVAR sprites

    FOR i := 1 TO Len( namelist )
        IF namelist[ i ] == NIL
            EXIT
        ENDIF
        nCount++
    NEXT
    numsprites := nCount
    sprites := {}

    IF numsprites == 0
        RETURN
    ENDIF

    start := firstspritelump - 1
    finish := lastspritelump + 1

    FOR i := 1 TO numsprites
        spritename := DEH_String( namelist[ i ] )
        ResetSpriteTemp()
        maxframe := -1

        FOR l := start + 1 TO finish - 1
            name := lumpinfo[ l + 1 ]:name
            IF hb_strnicmp( name, spritename, 4 ) == 0
                frame := Asc( SubStr( name, 5, 1 ) ) - Asc( "A" )
                rotation := Asc( SubStr( name, 6, 1 ) ) - Asc( "0" )
                IF modifiedgame
                    patched := W_GetNumForName( name )
                ELSE
                    patched := l
                ENDIF
                R_InstallSpriteLump( patched, frame, rotation, .F. )

                IF Len( name ) >= 7 .AND. ByteAt( name, 7 ) != 0
                    frame := Asc( SubStr( name, 7, 1 ) ) - Asc( "A" )
                    rotation := Asc( SubStr( name, 8, 1 ) ) - Asc( "0" )
                    R_InstallSpriteLump( l, frame, rotation, .T. )
                ENDIF
            ENDIF
        NEXT

        sprdef := spritedef_t():New()
        AAdd( sprites, sprdef )
        IF maxframe == -1
            sprdef:numframes := 0
            LOOP
        ENDIF

        maxframe++
        FOR frame := 0 TO maxframe - 1
            IF sprtemp[ frame + 1 ]:rotate == -1
                OutErr( "R_InitSprites: No patches found for " + spritename + ;
                        " frame " + Chr( Asc( "A" ) + frame ) + hb_eol() )
                sprtemp[ frame + 1 ]:rotate := 0
            ELSEIF sprtemp[ frame + 1 ]:rotate == 1
                FOR rotation := 1 TO 8
                    IF sprtemp[ frame + 1 ]:lump[ rotation ] == -1
                        OutErr( "R_InitSprites: Sprite " + spritename + ;
                                " frame " + Chr( Asc( "A" ) + frame ) + ;
                                " is missing rotations" + hb_eol() )
                    ENDIF
                NEXT
            ENDIF
        NEXT

        sprdef:numframes := maxframe
        sprdef:spriteframes := {}
        FOR frame := 1 TO maxframe
            AAdd( sprdef:spriteframes, CloneSpriteFrame( sprtemp[ frame ] ) )
        NEXT
    NEXT
RETURN

PROCEDURE R_InitSprites( namelist )
    LOCAL i
    MEMVAR negonearray
    MEMVAR screenheightarray

    FOR i := 1 TO SCREENWIDTH
        negonearray[ i ] := -1
        screenheightarray[ i ] := viewheight
    NEXT
    R_InitSpriteDefs( namelist )
RETURN

PROCEDURE R_ClearSprites()
    vissprite_p := 0
RETURN

FUNCTION R_NewVisSprite()
    LOCAL vis

    IF vissprite_p >= MAXVISSPRITES
        RETURN overflowsprite
    ENDIF

    vissprite_p++
    vis := vissprites[ vissprite_p ]
    IF vis == NIL
        vis := vissprite_t():New()
        vissprites[ vissprite_p ] := vis
    ENDIF
RETURN vis

PROCEDURE R_DrawMaskedColumn( column )
    LOCAL topscreen
    LOCAL bottomscreen
    LOCAL basetexturemid := dc_texturemid
    LOCAL cData
    LOCAL pos
    LOCAL topdelta
    LOCAL length
    MEMVAR colfunc
    MEMVAR dc_source
    MEMVAR dc_texturemid
    MEMVAR dc_x
    MEMVAR dc_yh
    MEMVAR dc_yl
    MEMVAR mceilingclip
    MEMVAR mfloorclip
    MEMVAR sprtopscreen
    MEMVAR spryscale

    IF ValType( column ) == "O"
        cData := column:data
        pos := column:pos
        IF pos <= 0 .AND. column:length > 0 .AND. column:topdelta != 255
            topscreen := sprtopscreen + spryscale * column:topdelta
            bottomscreen := topscreen + spryscale * column:length
            dc_yl := Shar( topscreen + FRACUNIT - 1, FRACBITS )
            dc_yh := Shar( bottomscreen - 1, FRACBITS )
            IF dc_yh >= mfloorclip[ dc_x + 1 ]
                dc_yh := mfloorclip[ dc_x + 1 ] - 1
            ENDIF
            IF dc_yl <= mceilingclip[ dc_x + 1 ]
                dc_yl := mceilingclip[ dc_x + 1 ] + 1
            ENDIF
            IF dc_yl <= dc_yh
                dc_source := cData
                dc_texturemid := basetexturemid - column:topdelta * FRACUNIT
                IfaceCall( colfunc )
            ENDIF
            dc_texturemid := basetexturemid
            RETURN
        ENDIF
        IF pos <= 0
            pos := 1
        ENDIF
    ELSEIF ValType( column ) == "C"
        cData := column
        pos := 1
    ELSE
        RETURN
    ENDIF

    DO WHILE pos <= Len( cData )
        topdelta := ByteAt( cData, pos )
        IF topdelta == 255
            EXIT
        ENDIF
        length := ByteAt( cData, pos + 1 )
        IF pos + length + 3 > Len( cData )
            EXIT
        ENDIF

        topscreen := sprtopscreen + spryscale * topdelta
        bottomscreen := topscreen + spryscale * length
        dc_yl := Shar( topscreen + FRACUNIT - 1, FRACBITS )
        dc_yh := Shar( bottomscreen - 1, FRACBITS )

        IF dc_yh >= mfloorclip[ dc_x + 1 ]
            dc_yh := mfloorclip[ dc_x + 1 ] - 1
        ENDIF
        IF dc_yl <= mceilingclip[ dc_x + 1 ]
            dc_yl := mceilingclip[ dc_x + 1 ] + 1
        ENDIF

        IF dc_yl <= dc_yh
            dc_source := SubStr( cData, pos + 3, length )
            dc_texturemid := basetexturemid - topdelta * FRACUNIT
            IfaceCall( colfunc )
        ENDIF
        pos += length + 4
    ENDDO

    dc_texturemid := basetexturemid
RETURN

PROCEDURE R_DrawVisSprite( vis, x1, x2 )
    LOCAL column
    LOCAL texturecolumn
    LOCAL frac
    LOCAL patch
    LOCAL cData
    LOCAL nCol
    LOCAL nTrans
    MEMVAR basecolfunc
    MEMVAR centeryfrac
    MEMVAR colfunc
    MEMVAR dc_colormap
    MEMVAR dc_iscale
    MEMVAR dc_texturemid
    MEMVAR dc_translation
    MEMVAR dc_x
    MEMVAR detailshift
    MEMVAR firstspritelump
    MEMVAR fuzzcolfunc
    MEMVAR sprtopscreen
    MEMVAR spryscale
    MEMVAR transcolfunc
    MEMVAR translationtables

    patch := W_CacheLumpNum( vis:patch + firstspritelump, PU_CACHE )
    dc_colormap := vis:colormap

    IF dc_colormap == NIL
        colfunc := fuzzcolfunc
    ELSEIF ( vis:mobjflags & MF_TRANSLATION ) != 0
        colfunc := transcolfunc
        nTrans := UShr( ( vis:mobjflags & MF_TRANSLATION ), MF_TRANSSHIFT - 8 ) - 256
        IF ValType( translationtables ) == "C"
            dc_translation := SubStr( translationtables, nTrans + 1, 256 )
        ELSEIF ValType( translationtables ) == "A"
            dc_translation := SliceArray( translationtables, nTrans + 1, 256 )
        ELSE
            dc_translation := translationtables
        ENDIF
    ENDIF

    dc_iscale := UShr( Abs( vis:xiscale ), detailshift )
    dc_texturemid := vis:texturemid
    frac := vis:startfrac + vis:xiscale * ( x1 - vis:x1 )
    spryscale := vis:scale
    sprtopscreen := centeryfrac - FixedMul( dc_texturemid, spryscale )

    FOR dc_x := x1 TO x2
        texturecolumn := Shar( frac, FRACBITS )
        column := column_t():New()

        IF ValType( patch ) == "O"
            cData := patch:data
            nCol := LONG( patch:columnofs[ texturecolumn + 1 ] ) + 1
        ELSEIF ValType( patch ) == "C"
            cData := patch
            nCol := LONG( PeekLong( patch, 8 + texturecolumn * 4 + 1 ) ) + 1
        ELSE
            EXIT
        ENDIF

        column:data := cData
        column:pos := nCol
        R_DrawMaskedColumn( column )
        frac += vis:xiscale
    NEXT

    colfunc := basecolfunc
RETURN

PROCEDURE R_ProjectSprite( thing )
    LOCAL tr_x
    LOCAL tr_y
    LOCAL gxt
    LOCAL gyt
    LOCAL tx
    LOCAL tz
    LOCAL xscale
    LOCAL x1
    LOCAL x2
    LOCAL sprdef
    LOCAL sprframe
    LOCAL lump
    LOCAL rot
    LOCAL flip
    LOCAL index
    LOCAL vis
    LOCAL ang
    LOCAL iscale
    LOCAL frame
    MEMVAR centerxfrac
    MEMVAR colormaps
    MEMVAR detailshift
    MEMVAR fixedcolormap
    MEMVAR projection
    MEMVAR spriteoffset
    MEMVAR sprites
    MEMVAR spritetopoffset
    MEMVAR spritewidth
    MEMVAR viewcos
    MEMVAR viewsin
    MEMVAR viewwidth
    MEMVAR viewx
    MEMVAR viewy
    MEMVAR viewz

    tr_x := thing:x - viewx
    tr_y := thing:y - viewy
    gxt := FixedMul( tr_x, viewcos )
    gyt := -FixedMul( tr_y, viewsin )
    tz := gxt - gyt
    IF tz < MINZ
        RETURN
    ENDIF

    xscale := FixedDiv( projection, tz )
    gxt := -FixedMul( tr_x, viewsin )
    gyt := FixedMul( tr_y, viewcos )
    tx := -( gyt + gxt )
    IF Abs( tx ) > tz * 4
        RETURN
    ENDIF

    sprdef := sprites[ thing:sprite + 1 ]
    frame := ( thing:frame & FF_FRAMEMASK )
    sprframe := sprdef:spriteframes[ frame + 1 ]

    IF sprframe:rotate != 0
        ang := R_PointToAngle( thing:x, thing:y )
        rot := UShr( ang - thing:angle + Int( ANG45 / 2 ) * 9, 29 )
        lump := sprframe:lump[ rot + 1 ]
        flip := sprframe:flip[ rot + 1 ] != 0
    ELSE
        lump := sprframe:lump[ 1 ]
        flip := sprframe:flip[ 1 ] != 0
    ENDIF

    IF lump < 0
        RETURN
    ENDIF

    tx -= spriteoffset[ lump + 1 ]
    x1 := Shar( centerxfrac + FixedMul( tx, xscale ), FRACBITS )
    IF x1 > viewwidth
        RETURN
    ENDIF

    tx += spritewidth[ lump + 1 ]
    x2 := Shar( centerxfrac + FixedMul( tx, xscale ), FRACBITS ) - 1
    IF x2 < 0
        RETURN
    ENDIF

    vis := R_NewVisSprite()
    vis:mobjflags := thing:flags
    vis:scale := xscale * ( 2 ^ detailshift )
    vis:gx := thing:x
    vis:gy := thing:y
    vis:gz := thing:z
    vis:gzt := thing:z + spritetopoffset[ lump + 1 ]
    vis:texturemid := vis:gzt - viewz
    vis:x1 := Max( x1, 0 )
    vis:x2 := Min( x2, viewwidth - 1 )
    iscale := FixedDiv( FRACUNIT, xscale )

    IF flip
        vis:startfrac := spritewidth[ lump + 1 ] - 1
        vis:xiscale := -iscale
    ELSE
        vis:startfrac := 0
        vis:xiscale := iscale
    ENDIF
    IF vis:x1 > x1
        vis:startfrac += vis:xiscale * ( vis:x1 - x1 )
    ENDIF
    vis:patch := lump

    IF ( thing:flags & MF_SHADOW ) != 0
        vis:colormap := NIL
    ELSEIF ! Empty( fixedcolormap )
        vis:colormap := fixedcolormap
    ELSEIF ( thing:frame & FF_FULLBRIGHT ) != 0
        vis:colormap := colormaps
    ELSE
        index := UShr( xscale, LIGHTSCALESHIFT - detailshift )
        IF index >= MAXLIGHTSCALE
            index := MAXLIGHTSCALE - 1
        ENDIF
        vis:colormap := spritelights[ index + 1 ]
    ENDIF
RETURN

PROCEDURE R_AddSprites( sec )
    LOCAL thing
    LOCAL lightnum
    MEMVAR extralight
    MEMVAR scalelight
    MEMVAR validcount

    IF sec:validcount == validcount
        RETURN
    ENDIF
    sec:validcount := validcount

    lightnum := Shar( sec:lightlevel, LIGHTSEGSHIFT ) + extralight
    IF lightnum < 0
        spritelights := scalelight[ 1 ]
    ELSEIF lightnum >= LIGHTLEVELS
        spritelights := scalelight[ LIGHTLEVELS ]
    ELSE
        spritelights := scalelight[ lightnum + 1 ]
    ENDIF

    thing := sec:thinglist
    DO WHILE thing != NIL
        R_ProjectSprite( thing )
        thing := thing:snext
    ENDDO
RETURN

PROCEDURE R_DrawPSprite( psp )
    LOCAL tx
    LOCAL x1
    LOCAL x2
    LOCAL sprdef
    LOCAL sprframe
    LOCAL lump
    LOCAL flip
    LOCAL vis := vissprite_t():New()
    LOCAL state
    LOCAL frame
    MEMVAR centerxfrac
    MEMVAR colormaps
    MEMVAR detailshift
    MEMVAR fixedcolormap
    MEMVAR pspriteiscale
    MEMVAR pspritescale
    MEMVAR spriteoffset
    MEMVAR sprites
    MEMVAR spritetopoffset
    MEMVAR spritewidth
    MEMVAR states
    MEMVAR viewplayer
    MEMVAR viewwidth

    state := states[ psp:iState + 1 ]
    sprdef := sprites[ state:sprite + 1 ]
    frame := ( state:frame & FF_FRAMEMASK )
    sprframe := sprdef:spriteframes[ frame + 1 ]
    lump := sprframe:lump[ 1 ]
    flip := sprframe:flip[ 1 ] != 0
    IF lump < 0
        RETURN
    ENDIF

    tx := psp:sx - 160 * FRACUNIT
    tx -= spriteoffset[ lump + 1 ]
    x1 := Shar( centerxfrac + FixedMul( tx, pspritescale ), FRACBITS )
    IF x1 > viewwidth
        RETURN
    ENDIF

    tx += spritewidth[ lump + 1 ]
    x2 := Shar( centerxfrac + FixedMul( tx, pspritescale ), FRACBITS ) - 1
    IF x2 < 0
        RETURN
    ENDIF

    vis:mobjflags := 0
    vis:texturemid := BASEYCENTER * FRACUNIT + Int( FRACUNIT / 2 ) ;
                      - ( psp:sy - spritetopoffset[ lump + 1 ] )
    vis:x1 := Max( x1, 0 )
    vis:x2 := Min( x2, viewwidth - 1 )
    vis:scale := pspritescale * ( 2 ^ detailshift )

    IF flip
        vis:xiscale := -pspriteiscale
        vis:startfrac := spritewidth[ lump + 1 ] - 1
    ELSE
        vis:xiscale := pspriteiscale
        vis:startfrac := 0
    ENDIF
    IF vis:x1 > x1
        vis:startfrac += vis:xiscale * ( vis:x1 - x1 )
    ENDIF
    vis:patch := lump

    IF viewplayer:powers[ pw_invisibility + 1 ] > 4 * 32 ;
       .OR. ( viewplayer:powers[ pw_invisibility + 1 ] & 8 ) != 0
        vis:colormap := NIL
    ELSEIF ! Empty( fixedcolormap )
        vis:colormap := fixedcolormap
    ELSEIF ( state:frame & FF_FULLBRIGHT ) != 0
        vis:colormap := colormaps
    ELSE
        vis:colormap := spritelights[ MAXLIGHTSCALE ]
    ENDIF

    R_DrawVisSprite( vis, vis:x1, vis:x2 )
RETURN

PROCEDURE R_DrawPlayerSprites()
    LOCAL i
    LOCAL lightnum
    LOCAL psp
    MEMVAR extralight
    MEMVAR mceilingclip
    MEMVAR mfloorclip
    MEMVAR negonearray
    MEMVAR scalelight
    MEMVAR screenheightarray
    MEMVAR viewplayer

    lightnum := Shar( viewplayer:mo:subsector:sector:lightlevel, LIGHTSEGSHIFT ) ;
                + extralight
    IF lightnum < 0
        spritelights := scalelight[ 1 ]
    ELSEIF lightnum >= LIGHTLEVELS
        spritelights := scalelight[ LIGHTLEVELS ]
    ELSE
        spritelights := scalelight[ lightnum + 1 ]
    ENDIF

    mfloorclip := screenheightarray
    mceilingclip := negonearray
    FOR i := 0 TO NUMPSPRITES - 1
        psp := viewplayer:psprites[ i + 1 ]
        IF psp:iState != S_NULL
            R_DrawPSprite( psp )
        ENDIF
    NEXT
RETURN

PROCEDURE R_SortVisSprites()
    LOCAL i
    LOCAL ds
    LOCAL best
    LOCAL unsorted := vissprite_t():New()
    LOCAL bestscale

    unsorted:next := unsorted
    unsorted:prev := unsorted
    vsprsortedhead:next := vsprsortedhead
    vsprsortedhead:prev := vsprsortedhead
    IF vissprite_p == 0
        RETURN
    ENDIF

    FOR i := 1 TO vissprite_p
        ds := vissprites[ i ]
        IF i < vissprite_p
            ds:next := vissprites[ i + 1 ]
        ELSE
            ds:next := unsorted
        ENDIF
        IF i > 1
            ds:prev := vissprites[ i - 1 ]
        ELSE
            ds:prev := unsorted
        ENDIF
    NEXT
    unsorted:next := vissprites[ 1 ]
    unsorted:prev := vissprites[ vissprite_p ]

    FOR i := 1 TO vissprite_p
        bestscale := INT_MAX
        best := unsorted:next
        ds := unsorted:next
        DO WHILE !( ds == unsorted )
            IF ds:scale < bestscale
                bestscale := ds:scale
                best := ds
            ENDIF
            ds := ds:next
        ENDDO

        best:next:prev := best:prev
        best:prev:next := best:next
        best:next := vsprsortedhead
        best:prev := vsprsortedhead:prev
        vsprsortedhead:prev:next := best
        vsprsortedhead:prev := best
    NEXT
RETURN

PROCEDURE R_DrawSprite( spr )
    LOCAL ds
    LOCAL i
    LOCAL x
    LOCAL r1
    LOCAL r2
    LOCAL scale
    LOCAL lowscale
    LOCAL silhouette
    MEMVAR drawsegs
    MEMVAR ds_p
    MEMVAR mceilingclip
    MEMVAR mfloorclip

    FOR x := spr:x1 TO spr:x2
        clipbot[ x + 1 ] := -2
        cliptop[ x + 1 ] := -2
    NEXT

    FOR i := ds_p TO 1 STEP -1
        ds := drawsegs[ i ]
        IF ds:x1 > spr:x2 .OR. ds:x2 < spr:x1 ;
           .OR. ( ds:silhouette == 0 .AND. ds:maskedtexturecol == NIL )
            LOOP
        ENDIF

        r1 := Max( ds:x1, spr:x1 )
        r2 := Min( ds:x2, spr:x2 )
        scale := Max( ds:scale1, ds:scale2 )
        lowscale := Min( ds:scale1, ds:scale2 )

        IF scale < spr:scale ;
           .OR. ( lowscale < spr:scale ;
                 .AND. R_PointOnSegSide( spr:gx, spr:gy, ds:curline ) == 0 )
            IF ds:maskedtexturecol != NIL
                R_RenderMaskedSegRange( ds, r1, r2 )
            ENDIF
            LOOP
        ENDIF

        silhouette := ds:silhouette
        IF spr:gz >= ds:bsilheight
            silhouette := ( silhouette & ( SIL_BOTTOM ^^ 0xFFFFFFFF ) )
        ENDIF
        IF spr:gzt <= ds:tsilheight
            silhouette := ( silhouette & ( SIL_TOP ^^ 0xFFFFFFFF ) )
        ENDIF

        IF ( silhouette & SIL_BOTTOM ) != 0
            FOR x := r1 TO r2
                IF clipbot[ x + 1 ] == -2
                    clipbot[ x + 1 ] := ds:sprbottomclip[ x + 1 ]
                ENDIF
            NEXT
        ENDIF
        IF ( silhouette & SIL_TOP ) != 0
            FOR x := r1 TO r2
                IF cliptop[ x + 1 ] == -2
                    cliptop[ x + 1 ] := ds:sprtopclip[ x + 1 ]
                ENDIF
            NEXT
        ENDIF
    NEXT

    FOR x := spr:x1 TO spr:x2
        IF clipbot[ x + 1 ] == -2
            clipbot[ x + 1 ] := viewheight
        ENDIF
        IF cliptop[ x + 1 ] == -2
            cliptop[ x + 1 ] := -1
        ENDIF
    NEXT

    mfloorclip := clipbot
    mceilingclip := cliptop
    R_DrawVisSprite( spr, spr:x1, spr:x2 )
RETURN

PROCEDURE R_DrawMasked()
    LOCAL spr
    LOCAL ds
    LOCAL i
    MEMVAR drawsegs
    MEMVAR ds_p
    MEMVAR viewangleoffset

    R_SortVisSprites()
    IF vissprite_p > 0
        spr := vsprsortedhead:next
        DO WHILE !( spr == vsprsortedhead )
            R_DrawSprite( spr )
            spr := spr:next
        ENDDO
    ENDIF

    FOR i := ds_p TO 1 STEP -1
        ds := drawsegs[ i ]
        IF ds:maskedtexturecol != NIL
            R_RenderMaskedSegRange( ds, ds:x1, ds:x2 )
        ENDIF
    NEXT

    IF viewangleoffset == 0
        R_DrawPlayerSprites()
    ENDIF
RETURN

INIT PROCEDURE init_r_things
    LOCAL i
    LOCAL nViewHeight := 0

    PUBLIC pspritescale
    PUBLIC pspriteiscale
    PUBLIC negonearray
    PUBLIC screenheightarray
    PUBLIC sprites
    PUBLIC numsprites
    PUBLIC mfloorclip
    PUBLIC mceilingclip
    PUBLIC spryscale
    PUBLIC sprtopscreen

    pspritescale := 0
    pspriteiscale := 0
    spritelights := {}
    negonearray := {}
    screenheightarray := {}
    sprites := {}
    numsprites := 0
    sprtemp := {}
    maxframe := -1
    spritename := ""
    vissprites := {}
    clipbot := {}
    cliptop := {}

    IF __mvExist( "VIEWHEIGHT" )
        nViewHeight := viewheight
    ENDIF
    FOR i := 1 TO SCREENWIDTH
        AAdd( negonearray, -1 )
        AAdd( screenheightarray, nViewHeight )
        AAdd( clipbot, -2 )
        AAdd( cliptop, -2 )
    NEXT
    FOR i := 1 TO MAXVISSPRITES
        AAdd( vissprites, vissprite_t():New() )
    NEXT

    vissprite_p := 0
    overflowsprite := vissprite_t():New()
    mfloorclip := screenheightarray
    mceilingclip := negonearray
    spryscale := 0
    sprtopscreen := 0
    vsprsortedhead := vissprite_t():New()
    vsprsortedhead:next := vsprsortedhead
    vsprsortedhead:prev := vsprsortedhead
RETURN
