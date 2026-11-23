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

STATIC mode_scale_1x
STATIC mode_scale_2x
STATIC mode_scale_3x
STATIC mode_scale_4x
STATIC mode_scale_5x
STATIC mode_stretch_1x
STATIC mode_stretch_2x
STATIC mode_stretch_3x
STATIC mode_stretch_4x
STATIC mode_stretch_5x
STATIC mode_squash_1x
STATIC mode_squash_2x
STATIC mode_squash_3x
STATIC mode_squash_4x
STATIC mode_squash_5x

#include "i_scale.ch"

CLASS screen_mode_t
    DATA width
    DATA height
    DATA InitMode
    DATA DrawScreen
    DATA poor_quality
    METHOD New()
ENDCLASS

METHOD New() CLASS screen_mode_t
    ::width := 0
    ::height := 0
    ::InitMode := NIL
    ::DrawScreen := NIL
    ::poor_quality := .F.
RETURN Self

STATIC FUNCTION ScaleNoop()
RETURN .F.

STATIC PROCEDURE ScaleNoopPal( pal )
    HB_SYMBOL_UNUSED( pal )
RETURN

STATIC FUNCTION NewMode( nW, nH, bInit, bDraw, lPoor )
    LOCAL oMode

    oMode := screen_mode_t():New()
    oMode:width := nW
    oMode:height := nH
    oMode:InitMode := bInit
    oMode:DrawScreen := bDraw
    oMode:poor_quality := lPoor
RETURN oMode

INIT PROCEDURE init_i_scale
    mode_scale_1x := NewMode( SCREENWIDTH, SCREENHEIGHT, NIL, {|| ScaleNoop() }, .F. )
    mode_scale_2x := NewMode( SCREENWIDTH * 2, SCREENHEIGHT * 2, NIL, {|| ScaleNoop() }, .F. )
    mode_scale_3x := NewMode( SCREENWIDTH * 3, SCREENHEIGHT * 3, NIL, {|| ScaleNoop() }, .F. )
    mode_scale_4x := NewMode( SCREENWIDTH * 4, SCREENHEIGHT * 4, NIL, {|| ScaleNoop() }, .F. )
    mode_scale_5x := NewMode( SCREENWIDTH * 5, SCREENHEIGHT * 5, NIL, {|| ScaleNoop() }, .F. )

    mode_stretch_1x := NewMode( SCREENWIDTH, SCREENHEIGHT_4_3, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .T. )
    mode_stretch_2x := NewMode( SCREENWIDTH * 2, SCREENHEIGHT_4_3 * 2, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .F. )
    mode_stretch_3x := NewMode( SCREENWIDTH * 3, SCREENHEIGHT_4_3 * 3, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .F. )
    mode_stretch_4x := NewMode( SCREENWIDTH * 4, SCREENHEIGHT_4_3 * 4, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .F. )
    mode_stretch_5x := NewMode( SCREENWIDTH * 5, SCREENHEIGHT_4_3 * 5, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .F. )

    mode_squash_1x := NewMode( SCREENWIDTH_4_3, SCREENHEIGHT, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .T. )
    mode_squash_2x := NewMode( SCREENWIDTH_4_3 * 2, SCREENHEIGHT * 2, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .F. )
    mode_squash_3x := NewMode( 800, 600, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .F. )
    mode_squash_4x := NewMode( SCREENWIDTH_4_3 * 4, SCREENHEIGHT * 4, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .F. )
    mode_squash_5x := NewMode( SCREENWIDTH_4_3 * 5, SCREENHEIGHT * 5, {| pal | ScaleNoopPal( pal ) }, {|| ScaleNoop() }, .F. )
RETURN

FUNCTION I_InitScale( src, dest, nPitch )
    HB_SYMBOL_UNUSED( src )
    HB_SYMBOL_UNUSED( dest )
    HB_SYMBOL_UNUSED( nPitch )
RETURN NIL

FUNCTION I_ResetScaleTables( palette )
    HB_SYMBOL_UNUSED( palette )
RETURN NIL
