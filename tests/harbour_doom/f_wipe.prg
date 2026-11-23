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

STATIC go := .F.
STATIC wipe_scr_start := ""
STATIC wipe_scr_end := ""
STATIC melt_y := {}
STATIC wipes := {}

#include "doomtype.ch"
#include "f_wipe.ch"

#ifndef SCREENWIDTH
#define SCREENWIDTH  320
#define SCREENHEIGHT 200
#endif



INIT PROCEDURE init_f_wipe

    go := .F.
    wipe_scr_start := ""
    wipe_scr_end := ""
    melt_y := {}

    wipes := {}
    AAdd( wipes, {| width, height, ticks | wipe_initColorXForm( width, height, ticks ) } )
    AAdd( wipes, {| width, height, ticks | wipe_doColorXForm( width, height, ticks ) } )
    AAdd( wipes, {| width, height, ticks | wipe_exitColorXForm( width, height, ticks ) } )
    AAdd( wipes, {| width, height, ticks | wipe_initMelt( width, height, ticks ) } )
    AAdd( wipes, {| width, height, ticks | wipe_doMelt( width, height, ticks ) } )
    AAdd( wipes, {| width, height, ticks | wipe_exitMelt( width, height, ticks ) } )
RETURN

STATIC PROCEDURE SetVideo( cBuf, nLen )
    MEMVAR I_VideoBuffer
    IF ValType( I_VideoBuffer ) == "C" .AND. Len( I_VideoBuffer ) > nLen
        I_VideoBuffer := Left( cBuf, nLen ) + SubStr( I_VideoBuffer, nLen + 1 )
    ELSE
        I_VideoBuffer := Left( cBuf, nLen )
    ENDIF
RETURN

STATIC FUNCTION GetWord( cBuf, nIdx0 )
    LOCAL nPos

    nPos := nIdx0 * 2 + 1
    IF ValType( cBuf ) != "C" .OR. nPos + 1 > Len( cBuf )
        RETURN 0
    ENDIF
RETURN Asc( SubStr( cBuf, nPos, 1 ) ) + Asc( SubStr( cBuf, nPos + 1, 1 ) ) * 256

STATIC FUNCTION SetWord( cBuf, nIdx0, nVal )
    LOCAL nPos

    nPos := nIdx0 * 2 + 1
    nVal := ( nVal & 0xFFFF )
    IF ValType( cBuf ) == "C" .AND. nPos + 1 <= Len( cBuf )
        cBuf := Stuff( cBuf, nPos, 2, Chr( ( nVal & 0xFF ) ) + Chr( Int( nVal / 256 ) ) )
    ENDIF
RETURN cBuf

FUNCTION wipe_shittyColMajorXform( array, width, height )
    LOCAL x
    LOCAL y
    LOCAL dest
    LOCAL nSrc
    LOCAL nDst

    dest := Replicate( Chr( 0 ), width * height * 2 )
    FOR y := 0 TO height - 1
        FOR x := 0 TO width - 1
            nSrc := y * width + x
            nDst := x * height + y
            dest := SetWord( dest, nDst, GetWord( array, nSrc ) )
        NEXT
    NEXT
    array := dest
RETURN array

FUNCTION wipe_initColorXForm( width, height, ticks )
    HB_SYMBOL_UNUSED( ticks )
    SetVideo( wipe_scr_start, width * height )
RETURN 0

FUNCTION wipe_doColorXForm( width, height, ticks )
    LOCAL lChanged
    LOCAL n
    LOCAL nLen
    LOCAL nW
    LOCAL nE
    LOCAL nNew
    LOCAL cScr
    MEMVAR I_VideoBuffer

    lChanged := .F.
    nLen := width * height
    cScr := iif( ValType( I_VideoBuffer ) == "C", I_VideoBuffer, Replicate( Chr( 0 ), nLen ) )

    FOR n := 1 TO nLen
        nW := Asc( SubStr( cScr, n, 1 ) )
        nE := Asc( SubStr( wipe_scr_end, n, 1 ) )
        IF nW != nE
            IF nW > nE
                nNew := nW - ticks
                IF nNew < nE
                    nNew := nE
                ENDIF
                cScr := Stuff( cScr, n, 1, Chr( nNew ) )
                lChanged := .T.
            ELSEIF nW < nE
                nNew := nW + ticks
                IF nNew > nE
                    nNew := nE
                ENDIF
                cScr := Stuff( cScr, n, 1, Chr( nNew ) )
                lChanged := .T.
            ENDIF
        ENDIF
    NEXT

    SetVideo( cScr, nLen )
RETURN iif( lChanged, 0, 1 )

FUNCTION wipe_exitColorXForm( width, height, ticks )
    HB_SYMBOL_UNUSED( width )
    HB_SYMBOL_UNUSED( height )
    HB_SYMBOL_UNUSED( ticks )
RETURN 0

FUNCTION wipe_initMelt( width, height, ticks )
    LOCAL i
    LOCAL r

    HB_SYMBOL_UNUSED( ticks )

    SetVideo( wipe_scr_start, width * height )

    wipe_scr_start := wipe_shittyColMajorXform( wipe_scr_start, Int( width / 2 ), height )
    wipe_scr_end   := wipe_shittyColMajorXform( wipe_scr_end,   Int( width / 2 ), height )

    melt_y := {}
    AAdd( melt_y, -( M_Random() % 16 ) )
    FOR i := 1 TO width - 1
        r := ( M_Random() % 3 ) - 1
        AAdd( melt_y, melt_y[ i ] + r )
        IF melt_y[ i + 1 ] > 0
            melt_y[ i + 1 ] := 0
        ELSEIF melt_y[ i + 1 ] == -16
            melt_y[ i + 1 ] := -15
        ENDIF
    NEXT
RETURN 0

FUNCTION wipe_doMelt( width, height, ticks )
    LOCAL i
    LOCAL j
    LOCAL dy
    LOCAL nYi
    LOCAL nWord
    LOCAL cScr
    LOCAL lDone
    MEMVAR I_VideoBuffer

    lDone := .T.
    width := Int( width / 2 )
    cScr := iif( ValType( I_VideoBuffer ) == "C", I_VideoBuffer, Replicate( Chr( 0 ), width * height * 2 ) )

    DO WHILE ticks > 0
        ticks := ticks - 1
        FOR i := 0 TO width - 1
            nYi := melt_y[ i + 1 ]
            IF nYi < 0
                melt_y[ i + 1 ] := nYi + 1
                lDone := .F.
            ELSEIF nYi < height
                dy := iif( nYi < 16, nYi + 1, 8 )
                IF nYi + dy >= height
                    dy := height - nYi
                ENDIF
                FOR j := 0 TO dy - 1
                    nWord := GetWord( wipe_scr_end, i * height + nYi + j )
                    cScr := SetWord( cScr, ( nYi + j ) * width + i, nWord )
                NEXT
                nYi := nYi + dy
                melt_y[ i + 1 ] := nYi
                FOR j := 0 TO height - nYi - 1
                    nWord := GetWord( wipe_scr_start, i * height + j )
                    cScr := SetWord( cScr, ( nYi + j ) * width + i, nWord )
                NEXT
                lDone := .F.
            ENDIF
        NEXT
    ENDDO

    I_VideoBuffer := cScr
RETURN iif( lDone, 1, 0 )

FUNCTION wipe_exitMelt( width, height, ticks )
    HB_SYMBOL_UNUSED( width )
    HB_SYMBOL_UNUSED( height )
    HB_SYMBOL_UNUSED( ticks )
    melt_y := {}
    wipe_scr_start := ""
    wipe_scr_end := ""
RETURN 0

FUNCTION wipe_StartScreen( x, y, width, height )
    HB_SYMBOL_UNUSED( x )
    HB_SYMBOL_UNUSED( y )
    HB_SYMBOL_UNUSED( width )
    HB_SYMBOL_UNUSED( height )
    wipe_scr_start := Replicate( Chr( 0 ), SCREENWIDTH * SCREENHEIGHT )
    I_ReadScreen( @wipe_scr_start )
RETURN 0

FUNCTION wipe_EndScreen( x, y, width, height )
    wipe_scr_end := Replicate( Chr( 0 ), SCREENWIDTH * SCREENHEIGHT )
    I_ReadScreen( @wipe_scr_end )
    V_DrawBlock( x, y, width, height, wipe_scr_start )
RETURN 0

FUNCTION wipe_ScreenWipe( wipeno, x, y, width, height, ticks )
    LOCAL rc

    HB_SYMBOL_UNUSED( x )
    HB_SYMBOL_UNUSED( y )

    IF ! go
        go := .T.
        Eval( wipes[ wipeno * 3 + 1 ], width, height, ticks )
    ENDIF

    V_MarkRect( 0, 0, width, height )
    rc := Eval( wipes[ wipeno * 3 + 1 + 1 ], width, height, ticks )

    IF rc != 0
        go := .F.
        Eval( wipes[ wipeno * 3 + 2 + 1 ], width, height, ticks )
    ENDIF
RETURN iif( go, 0, 1 )
