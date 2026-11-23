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

STATIC sttminus

#include "st_lib.ch"

CLASS st_number_t
    DATA x
    DATA y
    DATA width
    DATA oldnum
    DATA num
    DATA on
    DATA p
    DATA data
    METHOD New()
ENDCLASS
CLASS st_percent_t
    DATA n
    DATA p
    METHOD New()
ENDCLASS
CLASS st_multicon_t
    DATA x
    DATA y
    DATA oldinum
    DATA inum
    DATA on
    DATA p
    DATA data
    METHOD New()
ENDCLASS
CLASS st_binicon_t
    DATA x
    DATA y
    DATA oldval
    DATA val
    DATA on
    DATA p
    DATA data
    METHOD New()
ENDCLASS
#include "st_stuff.ch"
#include "z_zone.ch"
#include "i_swap.ch"



METHOD New() CLASS st_number_t
    ::x := 0
    ::y := 0
    ::width := 0
    ::oldnum := 0
    ::num := NIL
    ::on := NIL
    ::p := {}
    ::data := 0
RETURN Self

METHOD New() CLASS st_percent_t
    ::n := st_number_t():New()
    ::p := NIL
RETURN Self

METHOD New() CLASS st_multicon_t
    ::x := 0
    ::y := 0
    ::oldinum := -1
    ::inum := NIL
    ::on := NIL
    ::p := {}
    ::data := 0
RETURN Self

METHOD New() CLASS st_binicon_t
    ::x := 0
    ::y := 0
    ::oldval := .F.
    ::val := NIL
    ::on := NIL
    ::p := NIL
    ::data := 0
RETURN Self

STATIC FUNCTION RefVal( x )
    IF ValType( x ) == "B"
        RETURN Eval( x )
    ENDIF
RETURN x

STATIC FUNCTION CTrue( x )
    SWITCH ValType( x )
    CASE "L"
        RETURN x
    CASE "N"
        RETURN x != 0
    CASE "B"
        RETURN CTrue( Eval( x ) )
    ENDSWITCH
RETURN ! Empty( x )

STATIC FUNCTION PatchW( p )
    IF ValType( p ) == "C"
        RETURN SHORT( Asc( SubStr( p, 1, 1 ) ) + Asc( SubStr( p, 2, 1 ) ) * 256 )
    ENDIF
    IF ValType( p ) == "O"
        RETURN SHORT( p:width )
    ENDIF
RETURN 0

STATIC FUNCTION PatchH( p )
    IF ValType( p ) == "C"
        RETURN SHORT( Asc( SubStr( p, 3, 1 ) ) + Asc( SubStr( p, 4, 1 ) ) * 256 )
    ENDIF
    IF ValType( p ) == "O"
        RETURN SHORT( p:height )
    ENDIF
RETURN 0

STATIC FUNCTION PatchLeft( p )
    IF ValType( p ) == "C"
        RETURN SHORT( Asc( SubStr( p, 5, 1 ) ) + Asc( SubStr( p, 6, 1 ) ) * 256 )
    ENDIF
    IF ValType( p ) == "O"
        RETURN SHORT( p:leftoffset )
    ENDIF
RETURN 0

STATIC FUNCTION PatchTop( p )
    IF ValType( p ) == "C"
        RETURN SHORT( Asc( SubStr( p, 7, 1 ) ) + Asc( SubStr( p, 8, 1 ) ) * 256 )
    ENDIF
    IF ValType( p ) == "O"
        RETURN SHORT( p:topoffset )
    ENDIF
RETURN 0

PROCEDURE STlib_init()
    sttminus := W_CacheLumpName( DEH_String( "STTMINUS" ), PU_STATIC )
RETURN

PROCEDURE STlib_initNum( n, x, y, pl, num, on, width )
    n:x := x
    n:y := y
    n:oldnum := 0
    n:width := width
    n:num := num
    n:on := on
    n:p := pl
RETURN

PROCEDURE STlib_drawNum( n, refresh )
    LOCAL numdigits := n:width
    LOCAL num := RefVal( n:num )
    LOCAL w := PatchW( n:p[ 1 ] )
    LOCAL h := PatchH( n:p[ 1 ] )
    LOCAL x := n:x
    LOCAL neg
    MEMVAR st_backing_screen

    HB_SYMBOL_UNUSED( refresh )
    n:oldnum := RefVal( n:num )
    neg := num < 0
    IF neg
        IF numdigits == 2 .AND. num < -9
            num := -9
        ELSEIF numdigits == 3 .AND. num < -99
            num := -99
        ENDIF
        num := -num
    ENDIF

    x := n:x - numdigits * w
    IF n:y - ST_Y < 0
        I_Error( "drawNum: n->y - ST_Y < 0" )
    ENDIF
    V_CopyRect( x, n:y - ST_Y, st_backing_screen, w * numdigits, h, x, n:y )

    IF num == 1994
        RETURN
    ENDIF

    x := n:x
    IF num == 0
        V_DrawPatch( x - w, n:y, n:p[ 1 ] )
    ENDIF

    DO WHILE num != 0 .AND. numdigits > 0
        numdigits--
        x -= w
        V_DrawPatch( x, n:y, n:p[ ( num % 10 ) + 1 ] )
        num := Int( num / 10 )
    ENDDO

    IF neg
        V_DrawPatch( x - 8, n:y, sttminus )
    ENDIF
RETURN

PROCEDURE STlib_updateNum( n, refresh )
    IF CTrue( RefVal( n:on ) )
        STlib_drawNum( n, refresh )
    ENDIF
RETURN

PROCEDURE STlib_initPercent( p, x, y, pl, num, on, percent )
    STlib_initNum( p:n, x, y, pl, num, on, 3 )
    p:p := percent
RETURN

PROCEDURE STlib_updatePercent( per, refresh )
    IF CTrue( refresh ) .AND. CTrue( RefVal( per:n:on ) )
        V_DrawPatch( per:n:x, per:n:y, per:p )
    ENDIF
    STlib_updateNum( per:n, refresh )
RETURN

PROCEDURE STlib_initMultIcon( i, x, y, il, inum, on )
    i:x := x
    i:y := y
    i:oldinum := -1
    i:inum := inum
    i:on := on
    i:p := il
RETURN

PROCEDURE STlib_updateMultIcon( mi, refresh )
    LOCAL w
    LOCAL h
    LOCAL x
    LOCAL y
    LOCAL nInum := RefVal( mi:inum )
    MEMVAR st_backing_screen

    IF CTrue( RefVal( mi:on ) ) .AND. ( mi:oldinum != nInum .OR. CTrue( refresh ) ) .AND. nInum != -1
        IF mi:oldinum != -1
            x := mi:x - PatchLeft( mi:p[ mi:oldinum + 1 ] )
            y := mi:y - PatchTop( mi:p[ mi:oldinum + 1 ] )
            w := PatchW( mi:p[ mi:oldinum + 1 ] )
            h := PatchH( mi:p[ mi:oldinum + 1 ] )
            IF y - ST_Y < 0
                I_Error( "updateMultIcon: y - ST_Y < 0" )
            ENDIF
            V_CopyRect( x, y - ST_Y, st_backing_screen, w, h, x, y )
        ENDIF
        V_DrawPatch( mi:x, mi:y, mi:p[ nInum + 1 ] )
        mi:oldinum := nInum
    ENDIF
RETURN

PROCEDURE STlib_initBinIcon( b, x, y, i, val, on )
    b:x := x
    b:y := y
    b:oldval := .F.
    b:val := val
    b:on := on
    b:p := i
RETURN

PROCEDURE STlib_updateBinIcon( bi, refresh )
    LOCAL x
    LOCAL y
    LOCAL w
    LOCAL h
    LOCAL nVal := RefVal( bi:val )
    MEMVAR st_backing_screen

    IF CTrue( RefVal( bi:on ) ) .AND. ( bi:oldval != nVal .OR. CTrue( refresh ) )
        x := bi:x - PatchLeft( bi:p )
        y := bi:y - PatchTop( bi:p )
        w := PatchW( bi:p )
        h := PatchH( bi:p )
        IF y - ST_Y < 0
            I_Error( "updateBinIcon: y - ST_Y < 0" )
        ENDIF
        IF CTrue( nVal )
            V_DrawPatch( bi:x, bi:y, bi:p )
        ELSE
            V_CopyRect( x, y - ST_Y, st_backing_screen, w, h, x, y )
        ENDIF
        bi:oldval := nVal
    ENDIF
RETURN

INIT PROCEDURE init_st_lib
    sttminus := NIL
RETURN
