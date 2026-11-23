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

#include "hu_lib.ch"

CLASS hu_textline_t
    DATA x
    DATA y
    DATA f
    DATA sc
    DATA l
    DATA len
    DATA needsupdate
    METHOD New()
ENDCLASS
CLASS hu_stext_t
    DATA l
    DATA h
    DATA cl
    DATA on
    DATA laston
    METHOD New()
ENDCLASS
CLASS hu_itext_t
    DATA l
    DATA lm
    DATA on
    DATA laston
    METHOD New()
ENDCLASS

#ifndef SCREENWIDTH
#define SCREENWIDTH  320
#define SCREENHEIGHT 200
#endif

#ifndef SHORT
#define SHORT( x ) ( x )
#endif


METHOD New() CLASS hu_textline_t
    ::x := 0
    ::y := 0
    ::f := {}
    ::sc := 0
    ::l := ""
    ::len := 0
    ::needsupdate := 0
RETURN Self

METHOD New() CLASS hu_stext_t
    LOCAL i

    ::l := {}
    FOR i := 1 TO HU_MAXLINES
        AAdd( ::l, hu_textline_t():New() )
    NEXT
    ::h := 0
    ::cl := 0
    ::on := {|| .F. }
    ::laston := .T.
RETURN Self

METHOD New() CLASS hu_itext_t
    ::l := hu_textline_t():New()
    ::lm := 0
    ::on := {|| .F. }
    ::laston := .T.
RETURN Self

STATIC FUNCTION OnVal( bOn )
    IF ValType( bOn ) == "B"
        RETURN Eval( bOn )
    ENDIF
RETURN ! Empty( bOn )

STATIC FUNCTION PeekShort( cBuf, nPos )
    LOCAL n

    IF ValType( cBuf ) != "C" .OR. nPos < 1 .OR. nPos + 1 > Len( cBuf )
        RETURN 0
    ENDIF
    n := Asc( SubStr( cBuf, nPos, 1 ) ) + Asc( SubStr( cBuf, nPos + 1, 1 ) ) * 256
    IF n >= 32768
        n := n - 65536
    ENDIF
RETURN n

STATIC FUNCTION PatchWidth( patch )
    IF ValType( patch ) == "O"
        RETURN SHORT( patch:width )
    ENDIF
    IF ValType( patch ) == "C"
        RETURN SHORT( PeekShort( patch, 1 ) )
    ENDIF
RETURN 0

STATIC FUNCTION PatchHeight( patch )
    IF ValType( patch ) == "O"
        RETURN SHORT( patch:height )
    ENDIF
    IF ValType( patch ) == "C"
        RETURN SHORT( PeekShort( patch, 3 ) )
    ENDIF
RETURN 0

STATIC FUNCTION FontPatch( aFont, nIdx0 )
    IF ValType( aFont ) != "A" .OR. nIdx0 < 0 .OR. nIdx0 + 1 > Len( aFont )
        RETURN NIL
    ENDIF
RETURN aFont[ nIdx0 + 1 ]

FUNCTION HUlib_init()
RETURN NIL

FUNCTION HUlib_clearTextLine( t )
    t:len := 0
    t:l := ""
    t:needsupdate := 1
RETURN NIL

FUNCTION HUlib_initTextLine( t, x, y, f, sc )
    t:x := x
    t:y := y
    t:f := iif( f == NIL, {}, f )
    t:sc := sc
    HUlib_clearTextLine( t )
RETURN NIL

FUNCTION HUlib_addCharToTextLine( t, ch )
    IF t:len == HU_MAXLINELENGTH
        RETURN .F.
    ENDIF
    IF ValType( ch ) == "N"
        ch := Chr( ( ch & 0xFF ) )
    ENDIF
    t:l := t:l + ch
    t:len := Len( t:l )
    t:needsupdate := 4
RETURN .T.

FUNCTION HUlib_delCharFromTextLine( t )
    IF t:len == 0
        RETURN .F.
    ENDIF
    t:l := Left( t:l, t:len - 1 )
    t:len := Len( t:l )
    t:needsupdate := 4
RETURN .T.

FUNCTION HUlib_drawTextLine( l, lCursor )
    LOCAL i
    LOCAL w
    LOCAL x
    LOCAL c
    LOCAL patch
    LOCAL nUnd

    x := l:x
    FOR i := 0 TO l:len - 1
        c := Asc( Upper( SubStr( l:l, i + 1, 1 ) ) )
        IF c != 32 .AND. c >= l:sc .AND. c <= 95
            patch := FontPatch( l:f, c - l:sc )
            w := PatchWidth( patch )
            IF x + w > SCREENWIDTH
                EXIT
            ENDIF
            V_DrawPatchDirect( x, l:y, patch )
            x := x + w
        ELSE
            x := x + 4
            IF x >= SCREENWIDTH
                EXIT
            ENDIF
        ENDIF
    NEXT

    IF lCursor
        nUnd := 95 - l:sc
        patch := FontPatch( l:f, nUnd )
        IF x + PatchWidth( patch ) <= SCREENWIDTH
            V_DrawPatchDirect( x, l:y, patch )
        ENDIF
    ENDIF
RETURN NIL

FUNCTION HUlib_eraseTextLine( l )
    LOCAL lh
    LOCAL y
    LOCAL yoffset
    LOCAL patch
    MEMVAR automapactive
    MEMVAR viewwidth
    MEMVAR viewwindowx
    MEMVAR viewwindowy

    IF ! automapactive .AND. viewwindowx != 0 .AND. l:needsupdate != 0
        patch := FontPatch( l:f, 0 )
        lh := PatchHeight( patch ) + 1
        y := l:y
        yoffset := y * SCREENWIDTH
        DO WHILE y < l:y + lh
            IF y < viewwindowy .OR. y >= viewwindowy + viewheight
                R_VideoErase( yoffset, SCREENWIDTH )
            ELSE
                R_VideoErase( yoffset, viewwindowx )
                R_VideoErase( yoffset + viewwindowx + viewwidth, viewwindowx )
            ENDIF
            y := y + 1
            yoffset := yoffset + SCREENWIDTH
        ENDDO
    ENDIF

    IF l:needsupdate != 0
        l:needsupdate := l:needsupdate - 1
    ENDIF
RETURN NIL

FUNCTION HUlib_initSText( s, x, y, h, font, startchar, bOn )
    LOCAL i
    LOCAL nH

    s:h := h
    s:on := iif( ValType( bOn ) == "B", bOn, {|| .F. } )
    s:laston := .T.
    s:cl := 0
    nH := PatchHeight( FontPatch( font, 0 ) ) + 1
    FOR i := 0 TO h - 1
        HUlib_initTextLine( s:l[ i + 1 ], x, y - i * nH, font, startchar )
    NEXT
RETURN NIL

FUNCTION HUlib_addLineToSText( s )
    LOCAL i

    s:cl := s:cl + 1
    IF s:cl == s:h
        s:cl := 0
    ENDIF
    HUlib_clearTextLine( s:l[ s:cl + 1 ] )

    FOR i := 0 TO s:h - 1
        s:l[ i + 1 ]:needsupdate := 4
    NEXT
RETURN NIL

FUNCTION HUlib_addMessageToSText( s, cPrefix, cMsg )
    LOCAL nPos

    HUlib_addLineToSText( s )
    IF ! Empty( cPrefix )
        FOR nPos := 1 TO Len( cPrefix )
            HUlib_addCharToTextLine( s:l[ s:cl + 1 ], SubStr( cPrefix, nPos, 1 ) )
        NEXT
    ENDIF
    IF ! Empty( cMsg )
        FOR nPos := 1 TO Len( cMsg )
            HUlib_addCharToTextLine( s:l[ s:cl + 1 ], SubStr( cMsg, nPos, 1 ) )
        NEXT
    ENDIF
RETURN NIL

FUNCTION HUlib_drawSText( s )
    LOCAL i
    LOCAL idx

    IF ! OnVal( s:on )
        RETURN NIL
    ENDIF

    FOR i := 0 TO s:h - 1
        idx := s:cl - i
        IF idx < 0
            idx := idx + s:h
        ENDIF
        HUlib_drawTextLine( s:l[ idx + 1 ], .F. )
    NEXT
RETURN NIL

FUNCTION HUlib_eraseSText( s )
    LOCAL i

    FOR i := 0 TO s:h - 1
        IF s:laston .AND. ! OnVal( s:on )
            s:l[ i + 1 ]:needsupdate := 4
        ENDIF
        HUlib_eraseTextLine( s:l[ i + 1 ] )
    NEXT
    s:laston := OnVal( s:on )
RETURN NIL

FUNCTION HUlib_initIText( it, x, y, font, startchar, bOn )
    it:lm := 0
    it:on := iif( ValType( bOn ) == "B", bOn, {|| .F. } )
    it:laston := .T.
    HUlib_initTextLine( it:l, x, y, font, startchar )
RETURN NIL

FUNCTION HUlib_delCharFromIText( it )
    IF it:l:len != it:lm
        HUlib_delCharFromTextLine( it:l )
    ENDIF
RETURN NIL

FUNCTION HUlib_eraseLineFromIText( it )
    DO WHILE it:lm != it:l:len
        HUlib_delCharFromTextLine( it:l )
    ENDDO
RETURN NIL

FUNCTION HUlib_resetIText( it )
    it:lm := 0
    HUlib_clearTextLine( it:l )
RETURN NIL

FUNCTION HUlib_addPrefixToIText( it, cStr )
    LOCAL nPos

    IF ! Empty( cStr )
        FOR nPos := 1 TO Len( cStr )
            HUlib_addCharToTextLine( it:l, SubStr( cStr, nPos, 1 ) )
        NEXT
    ENDIF
    it:lm := it:l:len
RETURN NIL

FUNCTION HUlib_keyInIText( it, ch )
    IF ValType( ch ) == "C"
        ch := Asc( ch )
    ENDIF
    ch := Asc( Upper( Chr( ( ch & 0xFF ) ) ) )

    IF ch >= 32 .AND. ch <= 95
        HUlib_addCharToTextLine( it:l, Chr( ch ) )
    ELSEIF ch == KEY_BACKSPACE
        HUlib_delCharFromIText( it )
    ELSEIF ch != KEY_ENTER
        RETURN .F.
    ENDIF
RETURN .T.

FUNCTION HUlib_drawIText( it )
    IF ! OnVal( it:on )
        RETURN NIL
    ENDIF
    HUlib_drawTextLine( it:l, .T. )
RETURN NIL

FUNCTION HUlib_eraseIText( it )
    IF it:laston .AND. ! OnVal( it:on )
        it:l:needsupdate := 4
    ENDIF
    HUlib_eraseTextLine( it:l )
    it:laston := OnVal( it:on )
RETURN NIL
