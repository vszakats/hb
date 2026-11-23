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

STATIC patchclip_callback := NIL
STATIC dirtybox
STATIC tinttable
STATIC xlatab

#include "v_video.ch"
#include "i_video.ch"
#include "m_bbox.ch"
#include "i_swap.ch"
#include "z_zone.ch"

#define MOUSE_SPEED_BOX_WIDTH  120
#define MOUSE_SPEED_BOX_HEIGHT 9



STATIC FUNCTION PatchBytes( patch )
    IF ValType( patch ) == "C"
        RETURN patch
    ENDIF
    IF ValType( patch ) == "O" .AND. __objHasMsg( patch, "DATA" ) .AND. ValType( patch:data ) == "C"
        RETURN patch:data
    ENDIF
RETURN ""

STATIC FUNCTION BAt( c, nOff0 )
RETURN Asc( SubStr( c, nOff0 + 1, 1 ) )

STATIC FUNCTION PatchShort( c, nOff0 )
RETURN SHORT( BAt( c, nOff0 ) + BAt( c, nOff0 + 1 ) * 256 )

STATIC FUNCTION PatchLong( c, nOff0 )
    LOCAL n := BAt( c, nOff0 ) + BAt( c, nOff0 + 1 ) * 256 + BAt( c, nOff0 + 2 ) * 65536 + BAt( c, nOff0 + 3 ) * 16777216
RETURN LONG( n )

STATIC FUNCTION GetDest()
    MEMVAR dest_screen
    MEMVAR I_VideoBuffer
    IF dest_screen == NIL .OR. ValType( dest_screen ) != "C"
        RETURN I_VideoBuffer
    ENDIF
RETURN dest_screen

STATIC PROCEDURE PutDest( cBuf )
    MEMVAR dest_screen
    MEMVAR I_VideoBuffer
    IF dest_screen == NIL
        I_VideoBuffer := cBuf
    ELSE
        dest_screen := cBuf
    ENDIF
RETURN

STATIC PROCEDURE SetDestByte( nOff, nByte )
    LOCAL c := GetDest()
    c := Stuff( c, nOff + 1, 1, Chr( nByte & 0xFF ) )
    PutDest( c )
RETURN

STATIC FUNCTION GetDestByte( nOff )
RETURN Asc( SubStr( GetDest(), nOff + 1, 1 ) )

PROCEDURE V_MarkRect( x, y, width, height )
    MEMVAR dest_screen
    IF dest_screen == NIL
        M_AddToBox( dirtybox, x, y )
        M_AddToBox( dirtybox, x + width - 1, y + height - 1 )
    ENDIF
RETURN

PROCEDURE V_CopyRect( srcx, srcy, source, width, height, destx, desty )
    LOCAL src
    LOCAL dest
    LOCAL cSrc
    LOCAL row

    IF srcx < 0 .OR. srcx + width > SCREENWIDTH .OR. srcy < 0 .OR. srcy + height > SCREENHEIGHT ;
         .OR. destx < 0 .OR. destx + width > SCREENWIDTH .OR. desty < 0 .OR. desty + height > SCREENHEIGHT
        I_Error( "Bad V_CopyRect" )
    ENDIF

    V_MarkRect( destx, desty, width, height )
    cSrc := iif( ValType( source ) == "C", source, GetDest() )
    dest := GetDest()
    src := SCREENWIDTH * srcy + srcx
    destx := SCREENWIDTH * desty + destx
    DO WHILE height > 0
        row := SubStr( cSrc, src + 1, width )
        dest := Stuff( dest, destx + 1, width, row )
        src += SCREENWIDTH
        destx += SCREENWIDTH
        height--
    ENDDO
    PutDest( dest )
RETURN

PROCEDURE V_SetPatchClipCallback( func )
    patchclip_callback := func
RETURN

STATIC PROCEDURE DrawPatchPosts( x, y, cPatch, lFlip )
    LOCAL col
    LOCAL w
    LOCAL desttop
    LOCAL column
    LOCAL source
    LOCAL dest
    LOCAL count
    LOCAL topdelta
    LOCAL length
    LOCAL cDest
    LOCAL colofs

    w := PatchShort( cPatch, 0 )
    V_MarkRect( x, y, w, PatchShort( cPatch, 2 ) )
    cDest := GetDest()
    desttop := y * SCREENWIDTH + x
    FOR col := 0 TO w - 1
        IF lFlip
            colofs := PatchLong( cPatch, 8 + ( w - 1 - col ) * 4 )
        ELSE
            colofs := PatchLong( cPatch, 8 + col * 4 )
        ENDIF
        column := colofs
        DO WHILE column < Len( cPatch )
            topdelta := BAt( cPatch, column )
            IF topdelta == 0xFF
                EXIT
            ENDIF
            length := BAt( cPatch, column + 1 )
            source := column + 3
            dest := desttop + topdelta * SCREENWIDTH
            count := length
            DO WHILE count > 0
                cDest := Stuff( cDest, dest + 1, 1, SubStr( cPatch, source + 1, 1 ) )
                source++
                dest += SCREENWIDTH
                count--
            ENDDO
            column += length + 4
        ENDDO
        desttop++
    NEXT
    PutDest( cDest )
RETURN

PROCEDURE V_DrawPatch( x, y, patch )
    LOCAL cPatch := PatchBytes( patch )

    y -= PatchShort( cPatch, 6 )
    x -= PatchShort( cPatch, 4 )
    IF patchclip_callback != NIL
        IF ValType( patchclip_callback ) == "B"
            IF ! Eval( patchclip_callback, patch, x, y )
                RETURN
            ENDIF
        ENDIF
    ENDIF
    IF x < 0 .OR. x + PatchShort( cPatch, 0 ) > SCREENWIDTH .OR. y < 0 .OR. y + PatchShort( cPatch, 2 ) > SCREENHEIGHT
        I_Error( "Bad V_DrawPatch" )
    ENDIF
    DrawPatchPosts( x, y, cPatch, .F. )
RETURN

PROCEDURE V_DrawPatchFlipped( x, y, patch )
    LOCAL cPatch := PatchBytes( patch )

    y -= PatchShort( cPatch, 6 )
    x -= PatchShort( cPatch, 4 )
    IF patchclip_callback != NIL .AND. ValType( patchclip_callback ) == "B"
        IF ! Eval( patchclip_callback, patch, x, y )
            RETURN
        ENDIF
    ENDIF
    IF x < 0 .OR. x + PatchShort( cPatch, 0 ) > SCREENWIDTH .OR. y < 0 .OR. y + PatchShort( cPatch, 2 ) > SCREENHEIGHT
        I_Error( "Bad V_DrawPatchFlipped" )
    ENDIF
    DrawPatchPosts( x, y, cPatch, .T. )
RETURN

PROCEDURE V_DrawPatchDirect( x, y, patch )
    V_DrawPatch( x, y, patch )
RETURN

STATIC PROCEDURE DrawTintedPatch( x, y, patch, lAlt )
    LOCAL cPatch := PatchBytes( patch )
    LOCAL col
    LOCAL w
    LOCAL desttop
    LOCAL column
    LOCAL source
    LOCAL dest
    LOCAL count
    LOCAL topdelta
    LOCAL length
    LOCAL cDest
    LOCAL pix
    LOCAL src
    LOCAL colofs

    y -= PatchShort( cPatch, 6 )
    x -= PatchShort( cPatch, 4 )
    IF x < 0 .OR. x + PatchShort( cPatch, 0 ) > SCREENWIDTH .OR. y < 0 .OR. y + PatchShort( cPatch, 2 ) > SCREENHEIGHT
        I_Error( "Bad tinted patch" )
    ENDIF
    w := PatchShort( cPatch, 0 )
    cDest := GetDest()
    desttop := y * SCREENWIDTH + x
    FOR col := 0 TO w - 1
        colofs := PatchLong( cPatch, 8 + col * 4 )
        column := colofs
        DO WHILE column < Len( cPatch )
            topdelta := BAt( cPatch, column )
            IF topdelta == 0xFF
                EXIT
            ENDIF
            length := BAt( cPatch, column + 1 )
            source := column + 3
            dest := desttop + topdelta * SCREENWIDTH
            count := length
            DO WHILE count > 0
                pix := Asc( SubStr( cDest, dest + 1, 1 ) )
                src := BAt( cPatch, source )
                IF lAlt == 2
                    pix := BAt( xlatab, pix + src * 256 )
                ELSEIF lAlt == 1
                    pix := BAt( tinttable, pix + src * 256 )
                ELSE
                    pix := BAt( tinttable, pix * 256 + src )
                ENDIF
                cDest := Stuff( cDest, dest + 1, 1, Chr( pix ) )
                source++
                dest += SCREENWIDTH
                count--
            ENDDO
            column += length + 4
        ENDDO
        desttop++
    NEXT
    PutDest( cDest )
RETURN

PROCEDURE V_DrawTLPatch( x, y, patch )
    DrawTintedPatch( x, y, patch, 0 )
RETURN

PROCEDURE V_DrawXlaPatch( x, y, patch )
    LOCAL cPatch := PatchBytes( patch )
    LOCAL x2 := x - PatchShort( cPatch, 4 )
    LOCAL y2 := y - PatchShort( cPatch, 6 )

    IF patchclip_callback != NIL .AND. ValType( patchclip_callback ) == "B"
        IF ! Eval( patchclip_callback, patch, x2, y2 )
            RETURN
        ENDIF
    ENDIF
    DrawTintedPatch( x, y, patch, 2 )
RETURN

PROCEDURE V_DrawAltTLPatch( x, y, patch )
    DrawTintedPatch( x, y, patch, 1 )
RETURN

PROCEDURE V_DrawShadowedPatch( x, y, patch )
    DrawTintedPatch( x, y, patch, 0 )
RETURN

PROCEDURE V_LoadTintTable()
    tinttable := W_CacheLumpName( "TINTTAB", PU_STATIC )
RETURN

PROCEDURE V_LoadXlaTable()
    xlatab := W_CacheLumpName( "XLATAB", PU_STATIC )
RETURN

PROCEDURE V_DrawBlock( x, y, width, height, src )
    LOCAL dest
    LOCAL cDest
    LOCAL nSrc := 0

    IF x < 0 .OR. x + width > SCREENWIDTH .OR. y < 0 .OR. y + height > SCREENHEIGHT
        I_Error( "Bad V_DrawBlock" )
    ENDIF
    V_MarkRect( x, y, width, height )
    cDest := GetDest()
    dest := y * SCREENWIDTH + x
    DO WHILE height > 0
        cDest := Stuff( cDest, dest + 1, width, SubStr( src, nSrc + 1, width ) )
        nSrc += width
        dest += SCREENWIDTH
        height--
    ENDDO
    PutDest( cDest )
RETURN

PROCEDURE V_DrawFilledBox( x, y, w, h, c )
    LOCAL y1
    LOCAL row
    LOCAL cBuf := I_VideoBuffer
    LOCAL nOff
    MEMVAR I_VideoBuffer

    row := Replicate( Chr( c & 0xFF ), w )
    FOR y1 := 0 TO h - 1
        nOff := SCREENWIDTH * ( y + y1 ) + x
        cBuf := Stuff( cBuf, nOff + 1, w, row )
    NEXT
    I_VideoBuffer := cBuf
RETURN

PROCEDURE V_DrawHorizLine( x, y, w, c )
    LOCAL cBuf := I_VideoBuffer
    LOCAL nOff := SCREENWIDTH * y + x
    MEMVAR I_VideoBuffer
    cBuf := Stuff( cBuf, nOff + 1, w, Replicate( Chr( c & 0xFF ), w ) )
    I_VideoBuffer := cBuf
RETURN

PROCEDURE V_DrawVertLine( x, y, h, c )
    LOCAL y1
    LOCAL cBuf := I_VideoBuffer
    LOCAL nOff
    MEMVAR I_VideoBuffer
    FOR y1 := 0 TO h - 1
        nOff := SCREENWIDTH * ( y + y1 ) + x
        cBuf := Stuff( cBuf, nOff + 1, 1, Chr( c & 0xFF ) )
    NEXT
    I_VideoBuffer := cBuf
RETURN

PROCEDURE V_DrawBox( x, y, w, h, c )
    V_DrawHorizLine( x, y, w, c )
    V_DrawHorizLine( x, y + h - 1, w, c )
    V_DrawVertLine( x, y, h, c )
    V_DrawVertLine( x + w - 1, y, h, c )
RETURN

PROCEDURE V_DrawRawScreen( raw )
    PutDest( Left( raw + Replicate( Chr( 0 ), SCREENWIDTH * SCREENHEIGHT ), SCREENWIDTH * SCREENHEIGHT ) )
RETURN

PROCEDURE V_Init()
RETURN

PROCEDURE V_UseBuffer( buffer )
    MEMVAR dest_screen
    dest_screen := buffer
RETURN

PROCEDURE V_RestoreBuffer()
    MEMVAR dest_screen
    dest_screen := NIL
RETURN

PROCEDURE WritePCXfile( filename, data, width, height, palette )
    LOCAL cPcx
    LOCAL i
    LOCAL pix
    LOCAL nPack

    cPcx := Replicate( Chr( 0 ), 128 )
    cPcx := Stuff( cPcx, 1, 1, Chr( 0x0a ) )
    cPcx := Stuff( cPcx, 2, 1, Chr( 5 ) )
    cPcx := Stuff( cPcx, 3, 1, Chr( 1 ) )
    cPcx := Stuff( cPcx, 4, 1, Chr( 8 ) )
    cPcx := Stuff( cPcx, 9, 2, Chr( ( width - 1 ) & 0xFF ) + Chr( Int( ( width - 1 ) / 256 ) & 0xFF ) )
    cPcx := Stuff( cPcx, 11, 2, Chr( ( height - 1 ) & 0xFF ) + Chr( Int( ( height - 1 ) / 256 ) & 0xFF ) )
    cPcx := Stuff( cPcx, 13, 2, Chr( width & 0xFF ) + Chr( Int( width / 256 ) & 0xFF ) )
    cPcx := Stuff( cPcx, 15, 2, Chr( height & 0xFF ) + Chr( Int( height / 256 ) & 0xFF ) )
    cPcx := Stuff( cPcx, 66, 1, Chr( 1 ) )
    cPcx := Stuff( cPcx, 67, 2, Chr( width & 0xFF ) + Chr( Int( width / 256 ) & 0xFF ) )
    cPcx := Stuff( cPcx, 69, 2, Chr( 2 ) + Chr( 0 ) )

    FOR i := 0 TO width * height - 1
        pix := Asc( SubStr( data, i + 1, 1 ) )
        IF ( pix & 0xC0 ) != 0xC0
            cPcx += Chr( pix )
        ELSE
            cPcx += Chr( 0xC1 ) + Chr( pix )
        ENDIF
    NEXT
    cPcx += Chr( 0x0C )
    cPcx += Left( palette + Replicate( Chr( 0 ), 768 ), 768 )
    M_WriteFile( filename, cPcx, Len( cPcx ) )
RETURN

PROCEDURE V_ScreenShot( format )
    LOCAL i
    LOCAL lbmname
    LOCAL ext := "pcx"
    MEMVAR I_VideoBuffer

    FOR i := 0 TO 99
        lbmname := ""
        M_snprintf( @lbmname, 16, format, i, ext )
        IF ! M_FileExists( lbmname )
            EXIT
        ENDIF
    NEXT
    IF i == 100
        I_Error( "V_ScreenShot: Couldn't create a PCX" )
    ENDIF
    WritePCXfile( lbmname, I_VideoBuffer, SCREENWIDTH, SCREENHEIGHT, ;
                  W_CacheLumpName( DEH_String( "PLAYPAL" ), PU_CACHE ) )
RETURN

PROCEDURE V_DrawMouseSpeedBox( speed )
    LOCAL bgcolor
    LOCAL bordercolor
    LOCAL red
    LOCAL black
    LOCAL white
    LOCAL yellow
    LOCAL box_x
    LOCAL box_y
    LOCAL original_speed
    LOCAL redline_x
    LOCAL linelen
    MEMVAR mouse_acceleration
    MEMVAR mouse_threshold
    MEMVAR usemouse

    bgcolor := I_GetPaletteIndex( 0x77, 0x77, 0x77 )
    bordercolor := I_GetPaletteIndex( 0x55, 0x55, 0x55 )
    red := I_GetPaletteIndex( 0xFF, 0x00, 0x00 )
    black := I_GetPaletteIndex( 0x00, 0x00, 0x00 )
    yellow := I_GetPaletteIndex( 0xFF, 0xFF, 0x00 )
    white := I_GetPaletteIndex( 0xFF, 0xFF, 0xFF )

    IF ! usemouse .OR. Abs( mouse_acceleration - 1 ) < 0.01
        RETURN
    ENDIF

    box_x := SCREENWIDTH - MOUSE_SPEED_BOX_WIDTH - 10
    box_y := 15
    V_DrawFilledBox( box_x, box_y, MOUSE_SPEED_BOX_WIDTH, MOUSE_SPEED_BOX_HEIGHT, bgcolor )
    V_DrawBox( box_x, box_y, MOUSE_SPEED_BOX_WIDTH, MOUSE_SPEED_BOX_HEIGHT, bordercolor )
    redline_x := Int( MOUSE_SPEED_BOX_WIDTH / 3 )

    IF speed < mouse_threshold
        original_speed := speed
    ELSE
        original_speed := speed - mouse_threshold
        original_speed := Int( original_speed / mouse_acceleration )
        original_speed += mouse_threshold
    ENDIF

    IF mouse_threshold != 0
        linelen := Int( ( original_speed * redline_x ) / mouse_threshold )
    ELSE
        linelen := 0
    ENDIF
    IF linelen > MOUSE_SPEED_BOX_WIDTH - 1
        linelen := MOUSE_SPEED_BOX_WIDTH - 1
    ENDIF

    V_DrawHorizLine( box_x + 1, box_y + 4, MOUSE_SPEED_BOX_WIDTH - 2, black )
    IF linelen < redline_x
        V_DrawHorizLine( box_x + 1, box_y + Int( MOUSE_SPEED_BOX_HEIGHT / 2 ), linelen, white )
    ELSE
        V_DrawHorizLine( box_x + 1, box_y + Int( MOUSE_SPEED_BOX_HEIGHT / 2 ), redline_x, white )
        V_DrawHorizLine( box_x + redline_x, box_y + Int( MOUSE_SPEED_BOX_HEIGHT / 2 ), linelen - redline_x, yellow )
    ENDIF
    V_DrawVertLine( box_x + redline_x, box_y + 1, MOUSE_SPEED_BOX_HEIGHT - 2, red )
RETURN

INIT PROCEDURE init_v_video
    LOCAL i
    PUBLIC dest_screen
    dest_screen := NIL
    dirtybox := {}
    FOR i := 1 TO 4
        AAdd( dirtybox, 0 )
    NEXT
    tinttable := NIL
    xlatab := NIL
    patchclip_callback := NIL
RETURN
