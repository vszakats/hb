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

#include "p_local.ch"

CLASS vertex_t
    DATA x
    DATA y
    METHOD New()
ENDCLASS
CLASS degenmobj_t
    DATA thinker
    DATA x
    DATA y
    DATA z
    METHOD New()
ENDCLASS
CLASS sector_t
    DATA iSector
    DATA floorheight
    DATA ceilingheight
    DATA floorpic
    DATA ceilingpic
    DATA lightlevel
    DATA special
    DATA tag
    DATA soundtraversed
    DATA soundtarget
    DATA blockbox
    DATA soundorg
    DATA validcount
    DATA thinglist
    DATA specialdata
    DATA linecount
    DATA lines
    METHOD New()
ENDCLASS
CLASS side_t
    DATA textureoffset
    DATA rowoffset
    DATA toptexture
    DATA bottomtexture
    DATA midtexture
    DATA sector
    METHOD New()
ENDCLASS
CLASS line_t
    DATA iLine
    DATA v1
    DATA v2
    DATA dx
    DATA dy
    DATA flags
    DATA special
    DATA tag
    DATA sidenum
    DATA bbox
    DATA slopetype
    DATA frontsector
    DATA backsector
    DATA validcount
    DATA specialdata
    METHOD New()
ENDCLASS
CLASS subsector_t
    DATA sector
    DATA numlines
    DATA firstline
    METHOD New()
ENDCLASS
CLASS seg_t
    DATA v1
    DATA v2
    DATA offset
    DATA angle
    DATA sidedef
    DATA linedef
    DATA frontsector
    DATA backsector
    METHOD New()
ENDCLASS
CLASS node_t
    DATA x
    DATA y
    DATA dx
    DATA dy
    DATA bbox
    DATA children
    METHOD New()
ENDCLASS
CLASS divline_t
    DATA x
    DATA y
    DATA dx
    DATA dy
    METHOD New()
ENDCLASS
CLASS intercept_d_t
    DATA thing
    DATA line
    METHOD New()
ENDCLASS
CLASS intercept_t
    DATA frac
    DATA isaline
    DATA d
    METHOD New()
ENDCLASS

METHOD New() CLASS vertex_t
    ::x := 0
    ::y := 0
RETURN Self

METHOD New() CLASS degenmobj_t
    ::thinker := thinker_t():New()
    ::x := 0
    ::y := 0
    ::z := 0
RETURN Self

METHOD New() CLASS sector_t
    LOCAL i
    ::iSector := 0
    ::floorheight := 0
    ::ceilingheight := 0
    ::floorpic := 0
    ::ceilingpic := 0
    ::lightlevel := 0
    ::special := 0
    ::tag := 0
    ::soundtraversed := 0
    ::soundtarget := NIL
    ::blockbox := {}
    FOR i := 1 TO 4
        AAdd( ::blockbox, 0 )
    NEXT
    ::soundorg := degenmobj_t():New()
    ::validcount := 0
    ::thinglist := NIL
    ::specialdata := NIL
    ::linecount := 0
    ::lines := {}
RETURN Self

METHOD New() CLASS side_t
    ::textureoffset := 0
    ::rowoffset := 0
    ::toptexture := 0
    ::bottomtexture := 0
    ::midtexture := 0
    ::sector := NIL
RETURN Self

METHOD New() CLASS line_t
    LOCAL i
    ::iLine := 0
    ::v1 := NIL
    ::v2 := NIL
    ::dx := 0
    ::dy := 0
    ::flags := 0
    ::special := 0
    ::tag := 0
    ::sidenum := { -1, -1 }
    ::bbox := {}
    FOR i := 1 TO 4
        AAdd( ::bbox, 0 )
    NEXT
    ::slopetype := ST_HORIZONTAL
    ::frontsector := NIL
    ::backsector := NIL
    ::validcount := 0
    ::specialdata := NIL
RETURN Self

METHOD New() CLASS subsector_t
    ::sector := NIL
    ::numlines := 0
    ::firstline := 0
RETURN Self

METHOD New() CLASS seg_t
    ::v1 := NIL
    ::v2 := NIL
    ::offset := 0
    ::angle := 0
    ::sidedef := NIL
    ::linedef := NIL
    ::frontsector := NIL
    ::backsector := NIL
RETURN Self

METHOD New() CLASS node_t
    LOCAL j
    LOCAL k
    ::x := 0
    ::y := 0
    ::dx := 0
    ::dy := 0
    ::bbox := {}
    FOR j := 1 TO 2
        AAdd( ::bbox, {} )
        FOR k := 1 TO 4
            AAdd( ::bbox[ j ], 0 )
        NEXT
    NEXT
    ::children := { 0, 0 }
RETURN Self

METHOD New() CLASS divline_t
    ::x := 0
    ::y := 0
    ::dx := 0
    ::dy := 0
RETURN Self

METHOD New() CLASS intercept_d_t
    ::thing := NIL
    ::line := NIL
RETURN Self

METHOD New() CLASS intercept_t
    ::frac := 0
    ::isaline := .F.
    ::d := intercept_d_t():New()
RETURN Self

INIT PROCEDURE init_p_local
RETURN
