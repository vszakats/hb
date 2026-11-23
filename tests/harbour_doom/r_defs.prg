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

#include "r_defs.ch"

CLASS drawseg_t
    DATA curline
    DATA x1
    DATA x2
    DATA scale1
    DATA scale2
    DATA scalestep
    DATA silhouette
    DATA bsilheight
    DATA tsilheight
    DATA sprtopclip
    DATA sprbottomclip
    DATA maskedtexturecol
    METHOD New()
ENDCLASS
CLASS vissprite_t
    DATA prev
    DATA next
    DATA x1
    DATA x2
    DATA gx
    DATA gy
    DATA gz
    DATA gzt
    DATA startfrac
    DATA scale
    DATA xiscale
    DATA texturemid
    DATA patch
    DATA colormap
    DATA mobjflags
    METHOD New()
ENDCLASS
CLASS spriteframe_t
    DATA rotate
    DATA lump
    DATA flip
    METHOD New()
ENDCLASS
CLASS spritedef_t
    DATA numframes
    DATA spriteframes
    METHOD New()
ENDCLASS
CLASS visplane_t
    DATA height
    DATA picnum
    DATA lightlevel
    DATA minx
    DATA maxx
    DATA pad1
    DATA top
    DATA pad2
    DATA pad3
    DATA bottom
    DATA pad4
    METHOD New()
ENDCLASS
CLASS patch_t
    DATA width
    DATA height
    DATA leftoffset
    DATA topoffset
    DATA columnofs
    DATA data
    METHOD New()
ENDCLASS
CLASS column_t
    DATA topdelta
    DATA length
    DATA data
    DATA pos
    METHOD New()
ENDCLASS

METHOD New() CLASS drawseg_t
    ::curline := NIL
    ::x1 := 0
    ::x2 := 0
    ::scale1 := 0
    ::scale2 := 0
    ::scalestep := 0
    ::silhouette := SIL_NONE
    ::bsilheight := 0
    ::tsilheight := 0
    ::sprtopclip := NIL
    ::sprbottomclip := NIL
    ::maskedtexturecol := NIL
RETURN Self

METHOD New() CLASS vissprite_t
    ::prev := NIL
    ::next := NIL
    ::x1 := 0
    ::x2 := 0
    ::gx := 0
    ::gy := 0
    ::gz := 0
    ::gzt := 0
    ::startfrac := 0
    ::scale := 0
    ::xiscale := 0
    ::texturemid := 0
    ::patch := 0
    ::colormap := NIL
    ::mobjflags := 0
RETURN Self

METHOD New() CLASS spriteframe_t
    LOCAL i
    ::rotate := 0
    ::lump := {}
    ::flip := {}
    FOR i := 1 TO 8
        AAdd( ::lump, 0 )
        AAdd( ::flip, 0 )
    NEXT
RETURN Self

METHOD New() CLASS spritedef_t
    ::numframes := 0
    ::spriteframes := {}
RETURN Self

METHOD New() CLASS visplane_t
    LOCAL i
    ::height := 0
    ::picnum := 0
    ::lightlevel := 0
    ::minx := 0
    ::maxx := 0
    ::pad1 := 0
    ::top := {}
    ::pad2 := 0
    ::pad3 := 0
    ::bottom := {}
    ::pad4 := 0
    FOR i := 1 TO SCREENWIDTH
        AAdd( ::top, 0 )
        AAdd( ::bottom, 0 )
    NEXT
RETURN Self

METHOD New() CLASS patch_t
    LOCAL i
    ::width := 0
    ::height := 0
    ::leftoffset := 0
    ::topoffset := 0
    ::columnofs := {}
    ::data := ""
    FOR i := 1 TO 8
        AAdd( ::columnofs, 0 )
    NEXT
RETURN Self

METHOD New() CLASS column_t
    ::topdelta := 0
    ::length := 0
    ::data := ""
    ::pos := 0
RETURN Self
