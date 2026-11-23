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

STATIC sightzstart
STATIC strace
STATIC t2x
STATIC t2y
STATIC sightcounts

#include "p_sight.ch"
#include "doomdata.ch"
#include "doomstat.ch"


INIT PROCEDURE init_p_sight
    PUBLIC topslope
    PUBLIC bottomslope

    sightzstart := 0
    topslope := 0
    bottomslope := 0
    strace := divline_t():New()
    t2x := 0
    t2y := 0
    sightcounts := { 0, 0 }
RETURN

FUNCTION P_DivlineSide( x, y, node )
    LOCAL dx
    LOCAL dy
    LOCAL left
    LOCAL right

    IF node:dx == 0
        IF x == node:x
            RETURN 2
        ENDIF
        IF x <= node:x
            RETURN iif( node:dy > 0, 1, 0 )
        ENDIF
        RETURN iif( node:dy < 0, 1, 0 )
    ENDIF

    IF node:dy == 0
        IF x == node:y
            RETURN 2
        ENDIF
        IF y <= node:y
            RETURN iif( node:dx < 0, 1, 0 )
        ENDIF
        RETURN iif( node:dx > 0, 1, 0 )
    ENDIF

    dx := x - node:x
    dy := y - node:y

    left  := Shar( node:dy, FRACBITS ) * Shar( dx, FRACBITS )
    right := Shar( dy, FRACBITS ) * Shar( node:dx, FRACBITS )

    IF right < left
        RETURN 0
    ENDIF

    IF left == right
        RETURN 2
    ENDIF
RETURN 1

FUNCTION P_InterceptVector2( v2, v1 )
    LOCAL frac
    LOCAL num
    LOCAL den

    den := FixedMul( Shar( v1:dy, 8 ), v2:dx ) - FixedMul( Shar( v1:dx, 8 ), v2:dy )

    IF den == 0
        RETURN 0
    ENDIF

    num := FixedMul( Shar( v1:x - v2:x, 8 ), v1:dy ) + ;
           FixedMul( Shar( v2:y - v1:y, 8 ), v1:dx )
    frac := FixedDiv( num, den )

RETURN frac

FUNCTION P_CrossSubsector( num )
    LOCAL seg
    LOCAL line
    LOCAL s1
    LOCAL s2
    LOCAL count
    LOCAL nSeg
    LOCAL sub
    LOCAL front
    LOCAL back
    LOCAL opentop
    LOCAL openbottom
    LOCAL divl
    LOCAL v1
    LOCAL v2
    LOCAL frac
    LOCAL slope
    MEMVAR bottomslope
    MEMVAR numsubsectors
    MEMVAR segs
    MEMVAR subsectors
    MEMVAR topslope
    MEMVAR validcount

#ifdef RANGECHECK
    IF num >= numsubsectors
        I_Error( "P_CrossSubsector: ss " + LTrim( Str( num ) ) + ;
                 " with numss = " + LTrim( Str( numsubsectors ) ) )
    ENDIF
#endif

    sub := subsectors[ num + 1 ]

    nSeg := sub:firstline
    divl := divline_t():New()

    FOR count := sub:numlines TO 1 STEP -1
        seg := segs[ nSeg + 1 ]
        nSeg++
        line := seg:linedef

        IF line:validcount == validcount
            LOOP
        ENDIF

        line:validcount := validcount

        v1 := line:v1
        v2 := line:v2
        s1 := P_DivlineSide( v1:x, v1:y, strace )
        s2 := P_DivlineSide( v2:x, v2:y, strace )

        IF s1 == s2
            LOOP
        ENDIF

        divl:x  := v1:x
        divl:y  := v1:y
        divl:dx := v2:x - v1:x
        divl:dy := v2:y - v1:y
        s1 := P_DivlineSide( strace:x, strace:y, divl )
        s2 := P_DivlineSide( t2x, t2y, divl )

        IF s1 == s2
            LOOP
        ENDIF

        IF line:backsector == NIL
            RETURN .F.
        ENDIF

        IF ( line:flags & ML_TWOSIDED ) == 0
            RETURN .F.
        ENDIF

        front := seg:frontsector
        back  := seg:backsector

        IF front:floorheight == back:floorheight ;
           .AND. front:ceilingheight == back:ceilingheight
            LOOP
        ENDIF

        IF front:ceilingheight < back:ceilingheight
            opentop := front:ceilingheight
        ELSE
            opentop := back:ceilingheight
        ENDIF

        IF front:floorheight > back:floorheight
            openbottom := front:floorheight
        ELSE
            openbottom := back:floorheight
        ENDIF

        IF openbottom >= opentop
            RETURN .F.
        ENDIF

        frac := P_InterceptVector2( strace, divl )

        IF front:floorheight != back:floorheight
            slope := FixedDiv( openbottom - sightzstart, frac )
            IF slope > bottomslope
                bottomslope := slope
            ENDIF
        ENDIF

        IF front:ceilingheight != back:ceilingheight
            slope := FixedDiv( opentop - sightzstart, frac )
            IF slope < topslope
                topslope := slope
            ENDIF
        ENDIF

        IF topslope <= bottomslope
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

FUNCTION P_CrossBSPNode( bspnum )
    LOCAL bsp
    LOCAL side
    LOCAL other
    MEMVAR nodes

    IF ( bspnum & NF_SUBSECTOR ) != 0
        IF bspnum == -1
            RETURN P_CrossSubsector( 0 )
        ELSE
            RETURN P_CrossSubsector( ( bspnum & 0x7FFF ) )
        ENDIF
    ENDIF

    bsp := nodes[ bspnum + 1 ]

    side := P_DivlineSide( strace:x, strace:y, bsp )
    IF side == 2
        side := 0
    ENDIF

    IF ! P_CrossBSPNode( bsp:children[ side + 1 ] )
        RETURN .F.
    ENDIF

    other := P_DivlineSide( t2x, t2y, bsp )
    IF side == other
        RETURN .T.
    ENDIF

RETURN P_CrossBSPNode( bsp:children[ ( side ^^ 1 ) + 1 ] )

FUNCTION P_CheckSight( t1, t2 )
    LOCAL s1
    LOCAL s2
    LOCAL pnum
    LOCAL bytenum
    LOCAL bitnum
    MEMVAR bottomslope
    MEMVAR numnodes
    MEMVAR numsectors
    MEMVAR rejectmatrix
    MEMVAR topslope
    MEMVAR validcount

    s1 := t1:subsector:sector:iSector
    s2 := t2:subsector:sector:iSector
    pnum := s1 * numsectors + s2
    bytenum := UShr( pnum, 3 )
    bitnum := Int( 1 * ( 2 ^ ( pnum & 7 ) ) )

    IF ( Asc( SubStr( rejectmatrix, bytenum + 1, 1 ) ) & bitnum ) != 0
        sightcounts[ 1 ] += 1
        RETURN .F.
    ENDIF

    sightcounts[ 2 ] += 1

    validcount += 1

    sightzstart := t1:z + t1:height - Shar( t1:height, 2 )
    topslope := ( t2:z + t2:height ) - sightzstart
    bottomslope := t2:z - sightzstart

    strace:x := t1:x
    strace:y := t1:y
    t2x := t2:x
    t2y := t2:y
    strace:dx := t2:x - t1:x
    strace:dy := t2:y - t1:y

RETURN P_CrossBSPNode( numnodes - 1 )
