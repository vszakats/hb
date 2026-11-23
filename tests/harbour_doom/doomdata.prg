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

#include "doomdata.ch"

CLASS mapvertex_t
    DATA x
    DATA y
    METHOD New()
ENDCLASS
CLASS mapsidedef_t
    DATA textureoffset
    DATA rowoffset
    DATA toptexture
    DATA bottomtexture
    DATA midtexture
    DATA sector
    METHOD New()
ENDCLASS
CLASS maplinedef_t
    DATA v1
    DATA v2
    DATA flags
    DATA special
    DATA tag
    DATA sidenum
    METHOD New()
ENDCLASS
CLASS mapsector_t
    DATA floorheight
    DATA ceilingheight
    DATA floorpic
    DATA ceilingpic
    DATA lightlevel
    DATA special
    DATA tag
    METHOD New()
ENDCLASS
CLASS mapsubsector_t
    DATA numsegs
    DATA firstseg
    METHOD New()
ENDCLASS
CLASS mapseg_t
    DATA v1
    DATA v2
    DATA angle
    DATA linedef
    DATA side
    DATA offset
    METHOD New()
ENDCLASS
CLASS mapnode_t
    DATA x
    DATA y
    DATA dx
    DATA dy
    DATA bbox
    DATA children
    METHOD New()
ENDCLASS
CLASS mapthing_t
    DATA x
    DATA y
    DATA angle
    DATA type
    DATA options
    METHOD New()
ENDCLASS

METHOD New() CLASS mapvertex_t
    ::x := 0
    ::y := 0
RETURN Self

METHOD New() CLASS mapsidedef_t
    ::textureoffset := 0
    ::rowoffset     := 0
    ::toptexture    := ""
    ::bottomtexture := ""
    ::midtexture    := ""
    ::sector        := 0
RETURN Self

METHOD New() CLASS maplinedef_t
    ::v1      := 0
    ::v2      := 0
    ::flags   := 0
    ::special := 0
    ::tag     := 0
    /* sidenum[2] no C; sidenum[1] == -1 se one-sided. Harbour: 1-based. */
    ::sidenum := { 0, 0 }
RETURN Self

METHOD New() CLASS mapsector_t
    ::floorheight   := 0
    ::ceilingheight := 0
    ::floorpic      := ""
    ::ceilingpic    := ""
    ::lightlevel    := 0
    ::special       := 0
    ::tag           := 0
RETURN Self

METHOD New() CLASS mapsubsector_t
    ::numsegs  := 0
    ::firstseg := 0
RETURN Self

METHOD New() CLASS mapseg_t
    ::v1      := 0
    ::v2      := 0
    ::angle   := 0
    ::linedef := 0
    ::side    := 0
    ::offset  := 0
RETURN Self

METHOD New() CLASS mapnode_t
    ::x  := 0
    ::y  := 0
    ::dx := 0
    ::dy := 0
    /* bbox[2][4] no C; Harbour 1-based: bbox[1..2][1..4] */
    ::bbox     := { { 0, 0, 0, 0 }, { 0, 0, 0, 0 } }
    ::children := { 0, 0 }
RETURN Self

METHOD New() CLASS mapthing_t
    ::x       := 0
    ::y       := 0
    ::angle   := 0
    ::type    := 0
    ::options := 0
RETURN Self
