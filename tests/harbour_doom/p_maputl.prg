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

STATIC intercepts_overrun := {}
STATIC intercepts
STATIC intercept_p
STATIC earlyout
STATIC ptflags

#include "p_maputl.ch"

CLASS intercepts_overrun_t
    DATA len
    DATA name
    DATA int16_array
    METHOD New()
ENDCLASS


METHOD New() CLASS intercepts_overrun_t
    ::len         := 0
    ::name        := NIL
    ::int16_array := .F.
RETURN Self

INIT PROCEDURE init_p_maputl
    LOCAL i

    PUBLIC trace
    PUBLIC opentop
    PUBLIC openbottom
    PUBLIC openrange
    PUBLIC lowfloor

    intercepts := {}
    FOR i := 1 TO MAXINTERCEPTS
        AAdd( intercepts, intercept_t():New() )
    NEXT
    intercept_p := 0
    trace := divline_t():New()
    opentop := 0
    openbottom := 0
    openrange := 0
    lowfloor := 0
    earlyout := .F.
    ptflags := 0

    intercepts_overrun := {}
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, "lowfloor", .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, "openbottom", .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, "opentop", .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, "openrange", .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 120, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 8, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, "bulletslope", .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 40, "playerstarts", .T. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, "bmapwidth", .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, "bmaporgx", .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, "bmaporgy", .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, NIL, .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 4, "bmapheight", .F. ) )
    AAdd( intercepts_overrun, OverrunEnt( 0, NIL, .F. ) )
RETURN

STATIC FUNCTION OverrunEnt( nLen, cName, lArr )
    LOCAL o := intercepts_overrun_t():New()

    o:len := nLen
    o:name := cName
    o:int16_array := lArr
RETURN o

STATIC FUNCTION AsInt32( n )
    n := ( n & 0xFFFFFFFF )
    IF n >= 2147483648
        RETURN n - 4294967296
    ENDIF
RETURN n

STATIC FUNCTION AsInt16( n )
    n := ( n & 0xFFFF )
    IF n >= 32768
        RETURN n - 65536
    ENDIF
RETURN n

FUNCTION P_AproxDistance( dx, dy )
    dx := Abs( dx )
    dy := Abs( dy )
    IF dx < dy
        RETURN dx + dy - Shar( dx, 1 )
    ENDIF
RETURN dx + dy - Shar( dy, 1 )

FUNCTION P_PointOnLineSide( x, y, line )
    LOCAL dx
    LOCAL dy
    LOCAL left
    LOCAL right

    IF line:dx == 0
        IF x <= line:v1:x
            RETURN iif( line:dy > 0, 1, 0 )
        ENDIF
        RETURN iif( line:dy < 0, 1, 0 )
    ENDIF
    IF line:dy == 0
        IF y <= line:v1:y
            RETURN iif( line:dx < 0, 1, 0 )
        ENDIF
        RETURN iif( line:dx > 0, 1, 0 )
    ENDIF

    dx := x - line:v1:x
    dy := y - line:v1:y

    left := FixedMul( Shar( line:dy, FRACBITS ), dx )
    right := FixedMul( dy, Shar( line:dx, FRACBITS ) )

    IF right < left
        RETURN 0
    ENDIF
RETURN 1

FUNCTION P_BoxOnLineSide( tmbox, ld )
    LOCAL p1 := 0
    LOCAL p2 := 0

    SWITCH ld:slopetype
    CASE ST_HORIZONTAL
        p1 := iif( tmbox[ BOXTOP + 1 ] > ld:v1:y, 1, 0 )
        p2 := iif( tmbox[ BOXBOTTOM + 1 ] > ld:v1:y, 1, 0 )
        IF ld:dx < 0
            p1 := ( p1 ^^ 1 )
            p2 := ( p2 ^^ 1 )
        ENDIF
        EXIT

    CASE ST_VERTICAL
        p1 := iif( tmbox[ BOXRIGHT + 1 ] < ld:v1:x, 1, 0 )
        p2 := iif( tmbox[ BOXLEFT + 1 ] < ld:v1:x, 1, 0 )
        IF ld:dy < 0
            p1 := ( p1 ^^ 1 )
            p2 := ( p2 ^^ 1 )
        ENDIF
        EXIT

    CASE ST_POSITIVE
        p1 := P_PointOnLineSide( tmbox[ BOXLEFT + 1 ], tmbox[ BOXTOP + 1 ], ld )
        p2 := P_PointOnLineSide( tmbox[ BOXRIGHT + 1 ], tmbox[ BOXBOTTOM + 1 ], ld )
        EXIT

    CASE ST_NEGATIVE
        p1 := P_PointOnLineSide( tmbox[ BOXRIGHT + 1 ], tmbox[ BOXTOP + 1 ], ld )
        p2 := P_PointOnLineSide( tmbox[ BOXLEFT + 1 ], tmbox[ BOXBOTTOM + 1 ], ld )
        EXIT
    ENDSWITCH

    IF p1 == p2
        RETURN p1
    ENDIF
RETURN -1

FUNCTION P_PointOnDivlineSide( x, y, line )
    LOCAL dx
    LOCAL dy
    LOCAL left
    LOCAL right

    IF line:dx == 0
        IF x <= line:x
            RETURN iif( line:dy > 0, 1, 0 )
        ENDIF
        RETURN iif( line:dy < 0, 1, 0 )
    ENDIF
    IF line:dy == 0
        IF y <= line:y
            RETURN iif( line:dx < 0, 1, 0 )
        ENDIF
        RETURN iif( line:dx > 0, 1, 0 )
    ENDIF

    dx := x - line:x
    dy := y - line:y

    IF ( ( ( ( line:dy ^^ line:dx ) ^^ dx ) ^^ dy ) & 0x80000000 ) != 0
        IF ( ( line:dy ^^ dx ) & 0x80000000 ) != 0
            RETURN 1
        ENDIF
        RETURN 0
    ENDIF

    left := FixedMul( Shar( line:dy, 8 ), Shar( dx, 8 ) )
    right := FixedMul( Shar( dy, 8 ), Shar( line:dx, 8 ) )

    IF right < left
        RETURN 0
    ENDIF
RETURN 1

FUNCTION P_MakeDivline( li, dl )
    dl:x := li:v1:x
    dl:y := li:v1:y
    dl:dx := li:dx
    dl:dy := li:dy
RETURN NIL

FUNCTION P_InterceptVector( v2, v1 )
    LOCAL frac
    LOCAL num
    LOCAL den

    den := FixedMul( Shar( v1:dy, 8 ), v2:dx ) - FixedMul( Shar( v1:dx, 8 ), v2:dy )

    IF den == 0
        RETURN 0
    ENDIF

    num := FixedMul( Shar( v1:x - v2:x, 8 ), v1:dy ) ;
         + FixedMul( Shar( v2:y - v1:y, 8 ), v1:dx )

    frac := FixedDiv( num, den )
RETURN frac

FUNCTION P_LineOpening( linedef )
    LOCAL front
    LOCAL back
    MEMVAR lowfloor
    MEMVAR openbottom
    MEMVAR openrange
    MEMVAR opentop

    IF linedef:sidenum[ 2 ] == -1
        openrange := 0
        RETURN NIL
    ENDIF

    front := linedef:frontsector
    back := linedef:backsector

    IF front:ceilingheight < back:ceilingheight
        opentop := front:ceilingheight
    ELSE
        opentop := back:ceilingheight
    ENDIF

    IF front:floorheight > back:floorheight
        openbottom := front:floorheight
        lowfloor := back:floorheight
    ELSE
        openbottom := back:floorheight
        lowfloor := front:floorheight
    ENDIF

    openrange := opentop - openbottom
RETURN NIL

FUNCTION P_UnsetThingPosition( thing )
    LOCAL blockx
    LOCAL blocky
    MEMVAR blocklinks
    MEMVAR bmapheight
    MEMVAR bmaporgx
    MEMVAR bmaporgy
    MEMVAR bmapwidth

    IF ( thing:flags & MF_NOSECTOR ) == 0
        IF !( thing:snext == NIL )
            thing:snext:sprev := thing:sprev
        ENDIF

        IF !( thing:sprev == NIL )
            thing:sprev:snext := thing:snext
        ELSE
            thing:subsector:sector:thinglist := thing:snext
        ENDIF
    ENDIF

    IF ( thing:flags & MF_NOBLOCKMAP ) == 0
        IF !( thing:bnext == NIL )
            thing:bnext:bprev := thing:bprev
        ENDIF

        IF !( thing:bprev == NIL )
            thing:bprev:bnext := thing:bnext
        ELSE
            blockx := Shar( thing:x - bmaporgx, MAPBLOCKSHIFT )
            blocky := Shar( thing:y - bmaporgy, MAPBLOCKSHIFT )

            IF blockx >= 0 .AND. blockx < bmapwidth ;
               .AND. blocky >= 0 .AND. blocky < bmapheight
                blocklinks[ blocky * bmapwidth + blockx + 1 ] := thing:bnext
            ENDIF
        ENDIF
    ENDIF
RETURN NIL

FUNCTION P_SetThingPosition( thing )
    LOCAL ss
    LOCAL sec
    LOCAL blockx
    LOCAL blocky
    LOCAL nLink
    MEMVAR blocklinks
    MEMVAR bmapheight
    MEMVAR bmaporgx
    MEMVAR bmaporgy
    MEMVAR bmapwidth

    ss := R_PointInSubsector( thing:x, thing:y )
    thing:subsector := ss

    IF ( thing:flags & MF_NOSECTOR ) == 0
        sec := ss:sector

        thing:sprev := NIL
        thing:snext := sec:thinglist

        IF !( sec:thinglist == NIL )
            sec:thinglist:sprev := thing
        ENDIF

        sec:thinglist := thing
    ENDIF

    IF ( thing:flags & MF_NOBLOCKMAP ) == 0
        blockx := Shar( thing:x - bmaporgx, MAPBLOCKSHIFT )
        blocky := Shar( thing:y - bmaporgy, MAPBLOCKSHIFT )

        IF blockx >= 0 ;
           .AND. blockx < bmapwidth ;
           .AND. blocky >= 0 ;
           .AND. blocky < bmapheight
            nLink := blocky * bmapwidth + blockx + 1
            thing:bprev := NIL
            thing:bnext := blocklinks[ nLink ]
            IF !( blocklinks[ nLink ] == NIL )
                blocklinks[ nLink ]:bprev := thing
            ENDIF
            blocklinks[ nLink ] := thing
        ELSE
            thing:bnext := NIL
            thing:bprev := NIL
        ENDIF
    ENDIF
RETURN NIL

FUNCTION P_BlockLinesIterator( x, y, func )
    LOCAL nList
    LOCAL n
    LOCAL ld
    MEMVAR blockmap
    MEMVAR blockmaplump
    MEMVAR bmapheight
    MEMVAR bmapwidth
    MEMVAR lines
    MEMVAR validcount

    IF x < 0 .OR. y < 0 .OR. x >= bmapwidth .OR. y >= bmapheight
        RETURN .T.
    ENDIF

    nList := blockmap[ y * bmapwidth + x + 1 ]

    DO WHILE blockmaplump[ nList + 1 ] != -1
        n := blockmaplump[ nList + 1 ]
        ld := lines[ n + 1 ]

        IF ld:validcount == validcount
            nList += 1
            LOOP
        ENDIF

        ld:validcount := validcount

        IF ! Eval( func, ld )
            RETURN .F.
        ENDIF

        nList += 1
    ENDDO
RETURN .T.

FUNCTION P_BlockThingsIterator( x, y, func )
    LOCAL mobj
    MEMVAR blocklinks
    MEMVAR bmapheight
    MEMVAR bmapwidth

    IF x < 0 .OR. y < 0 .OR. x >= bmapwidth .OR. y >= bmapheight
        RETURN .T.
    ENDIF

    mobj := blocklinks[ y * bmapwidth + x + 1 ]
    DO WHILE !( mobj == NIL )
        IF ! Eval( func, mobj )
            RETURN .F.
        ENDIF
        mobj := mobj:bnext
    ENDDO
RETURN .T.

FUNCTION PIT_AddLineIntercepts( ld )
    LOCAL s1
    LOCAL s2
    LOCAL frac
    LOCAL dl
    MEMVAR trace

    IF trace:dx > FRACUNIT * 16 ;
       .OR. trace:dy > FRACUNIT * 16 ;
       .OR. trace:dx < -FRACUNIT * 16 ;
       .OR. trace:dy < -FRACUNIT * 16
        s1 := P_PointOnDivlineSide( ld:v1:x, ld:v1:y, trace )
        s2 := P_PointOnDivlineSide( ld:v2:x, ld:v2:y, trace )
    ELSE
        s1 := P_PointOnLineSide( trace:x, trace:y, ld )
        s2 := P_PointOnLineSide( trace:x + trace:dx, trace:y + trace:dy, ld )
    ENDIF

    IF s1 == s2
        RETURN .T.
    ENDIF

    dl := divline_t():New()
    P_MakeDivline( ld, dl )
    frac := P_InterceptVector( trace, dl )

    IF frac < 0
        RETURN .T.
    ENDIF

    IF earlyout .AND. frac < FRACUNIT .AND. ld:backsector == NIL
        RETURN .F.
    ENDIF

    intercept_p += 1
    IF intercept_p > Len( intercepts )
        AAdd( intercepts, intercept_t():New() )
    ENDIF
    intercepts[ intercept_p ]:frac := frac
    intercepts[ intercept_p ]:isaline := .T.
    intercepts[ intercept_p ]:d:line := ld
    intercepts[ intercept_p ]:d:thing := NIL
    InterceptsOverrun( intercept_p - 1, intercepts[ intercept_p ] )
RETURN .T.

FUNCTION PIT_AddThingIntercepts( thing )
    LOCAL x1
    LOCAL y1
    LOCAL x2
    LOCAL y2
    LOCAL s1
    LOCAL s2
    LOCAL tracepositive
    LOCAL dl
    LOCAL frac
    MEMVAR trace

    tracepositive := AsInt32( ( trace:dx ^^ trace:dy ) ) > 0

    IF tracepositive
        x1 := thing:x - thing:radius
        y1 := thing:y + thing:radius
        x2 := thing:x + thing:radius
        y2 := thing:y - thing:radius
    ELSE
        x1 := thing:x - thing:radius
        y1 := thing:y - thing:radius
        x2 := thing:x + thing:radius
        y2 := thing:y + thing:radius
    ENDIF

    s1 := P_PointOnDivlineSide( x1, y1, trace )
    s2 := P_PointOnDivlineSide( x2, y2, trace )

    IF s1 == s2
        RETURN .T.
    ENDIF

    dl := divline_t():New()
    dl:x := x1
    dl:y := y1
    dl:dx := x2 - x1
    dl:dy := y2 - y1

    frac := P_InterceptVector( trace, dl )

    IF frac < 0
        RETURN .T.
    ENDIF

    intercept_p += 1
    IF intercept_p > Len( intercepts )
        AAdd( intercepts, intercept_t():New() )
    ENDIF
    intercepts[ intercept_p ]:frac := frac
    intercepts[ intercept_p ]:isaline := .F.
    intercepts[ intercept_p ]:d:thing := thing
    intercepts[ intercept_p ]:d:line := NIL
    InterceptsOverrun( intercept_p - 1, intercepts[ intercept_p ] )
RETURN .T.

FUNCTION P_TraverseIntercepts( func, maxfrac )
    LOCAL count
    LOCAL dist
    LOCAL scan
    LOCAL oIn

    count := intercept_p
    oIn := NIL

    DO WHILE count > 0
        count -= 1
        dist := INT_MAX
        FOR scan := 1 TO intercept_p
            IF intercepts[ scan ]:frac < dist
                dist := intercepts[ scan ]:frac
                oIn := intercepts[ scan ]
            ENDIF
        NEXT

        IF dist > maxfrac
            RETURN .T.
        ENDIF

        IF ! Eval( func, oIn )
            RETURN .F.
        ENDIF

        oIn:frac := INT_MAX
    ENDDO
RETURN .T.

STATIC PROCEDURE OverrunSetInt( cName, nVal )
    MEMVAR bmapheight
    MEMVAR bmaporgx
    MEMVAR bmaporgy
    MEMVAR bmapwidth
    MEMVAR bulletslope
    MEMVAR lowfloor
    MEMVAR openbottom
    MEMVAR openrange
    MEMVAR opentop
    SWITCH cName
    CASE "lowfloor"
        lowfloor := nVal
        EXIT
    CASE "openbottom"
        openbottom := nVal
        EXIT
    CASE "opentop"
        opentop := nVal
        EXIT
    CASE "openrange"
        openrange := nVal
        EXIT
    CASE "bulletslope"
        bulletslope := nVal
        EXIT
    CASE "bmapwidth"
        bmapwidth := nVal
        EXIT
    CASE "bmaporgx"
        bmaporgx := nVal
        EXIT
    CASE "bmaporgy"
        bmaporgy := nVal
        EXIT
    CASE "bmapheight"
        bmapheight := nVal
        EXIT
    ENDSWITCH
RETURN

STATIC PROCEDURE OverrunSetPlayerStartShort( nIndex, nVal )
    LOCAL nThing
    LOCAL nField
    LOCAL o
    MEMVAR playerstarts

    IF playerstarts == NIL
        RETURN
    ENDIF

    nThing := Int( nIndex / 5 )
    nField := nIndex % 5
    IF nThing < 0 .OR. nThing + 1 > Len( playerstarts )
        RETURN
    ENDIF

    o := playerstarts[ nThing + 1 ]
    IF o == NIL
        RETURN
    ENDIF

    nVal := AsInt16( nVal )
    SWITCH nField
    CASE 0
        o:x := nVal
        EXIT
    CASE 1
        o:y := nVal
        EXIT
    CASE 2
        o:angle := nVal
        EXIT
    CASE 3
        o:type := nVal
        EXIT
    CASE 4
        o:options := nVal
        EXIT
    ENDSWITCH
RETURN

STATIC PROCEDURE InterceptsMemoryOverrun( location, value )
    LOCAL i
    LOCAL offset
    LOCAL index

    i := 1
    offset := 0

    DO WHILE i <= Len( intercepts_overrun ) .AND. intercepts_overrun[ i ]:len != 0
        IF offset + intercepts_overrun[ i ]:len > location
            IF intercepts_overrun[ i ]:name != NIL
                IF intercepts_overrun[ i ]:int16_array
                    index := Int( ( location - offset ) / 2 )
                    OverrunSetPlayerStartShort( index, ( value & 0xFFFF ) )
                    OverrunSetPlayerStartShort( index + 1, ( UShr( value, 16 ) & 0xFFFF ) )
                ELSE
                    OverrunSetInt( intercepts_overrun[ i ]:name, value )
                ENDIF
            ENDIF
            EXIT
        ENDIF

        offset += intercepts_overrun[ i ]:len
        i += 1
    ENDDO
RETURN

STATIC PROCEDURE InterceptsOverrun( num_intercepts, intercept )
    LOCAL location

    IF num_intercepts <= MAXINTERCEPTS_ORIGINAL
        RETURN
    ENDIF

    location := ( num_intercepts - MAXINTERCEPTS_ORIGINAL - 1 ) * 12

    InterceptsMemoryOverrun( location, intercept:frac )
    InterceptsMemoryOverrun( location + 4, iif( intercept:isaline, 1, 0 ) )
    InterceptsMemoryOverrun( location + 8, 0 )
RETURN

FUNCTION P_PathTraverse( x1, y1, x2, y2, flags, trav )
    LOCAL xt1
    LOCAL yt1
    LOCAL xt2
    LOCAL yt2
    LOCAL xstep
    LOCAL ystep
    LOCAL partial
    LOCAL xintercept
    LOCAL yintercept
    LOCAL mapx
    LOCAL mapy
    LOCAL mapxstep
    LOCAL mapystep
    LOCAL count
    MEMVAR bmaporgx
    MEMVAR bmaporgy
    MEMVAR trace
    MEMVAR validcount

    earlyout := ( flags & PT_EARLYOUT ) != 0

    validcount += 1
    intercept_p := 0

    IF ( ( ( x1 - bmaporgx ) & ( MAPBLOCKSIZE - 1 ) ) ) == 0
        x1 += FRACUNIT
    ENDIF

    IF ( ( ( y1 - bmaporgy ) & ( MAPBLOCKSIZE - 1 ) ) ) == 0
        y1 += FRACUNIT
    ENDIF

    trace:x := x1
    trace:y := y1
    trace:dx := x2 - x1
    trace:dy := y2 - y1

    x1 -= bmaporgx
    y1 -= bmaporgy
    xt1 := Shar( x1, MAPBLOCKSHIFT )
    yt1 := Shar( y1, MAPBLOCKSHIFT )

    x2 -= bmaporgx
    y2 -= bmaporgy
    xt2 := Shar( x2, MAPBLOCKSHIFT )
    yt2 := Shar( y2, MAPBLOCKSHIFT )

    IF xt2 > xt1
        mapxstep := 1
        partial := FRACUNIT - ( ( Shar( x1, MAPBTOFRAC ) & ( FRACUNIT - 1 ) ) )
        ystep := FixedDiv( y2 - y1, Abs( x2 - x1 ) )
    ELSEIF xt2 < xt1
        mapxstep := -1
        partial := ( Shar( x1, MAPBTOFRAC ) & ( FRACUNIT - 1 ) )
        ystep := FixedDiv( y2 - y1, Abs( x2 - x1 ) )
    ELSE
        mapxstep := 0
        partial := FRACUNIT
        ystep := 256 * FRACUNIT
    ENDIF

    yintercept := Shar( y1, MAPBTOFRAC ) + FixedMul( partial, ystep )

    IF yt2 > yt1
        mapystep := 1
        partial := FRACUNIT - ( ( Shar( y1, MAPBTOFRAC ) & ( FRACUNIT - 1 ) ) )
        xstep := FixedDiv( x2 - x1, Abs( y2 - y1 ) )
    ELSEIF yt2 < yt1
        mapystep := -1
        partial := ( Shar( y1, MAPBTOFRAC ) & ( FRACUNIT - 1 ) )
        xstep := FixedDiv( x2 - x1, Abs( y2 - y1 ) )
    ELSE
        mapystep := 0
        partial := FRACUNIT
        xstep := 256 * FRACUNIT
    ENDIF
    xintercept := Shar( x1, MAPBTOFRAC ) + FixedMul( partial, xstep )

    mapx := xt1
    mapy := yt1

    FOR count := 0 TO 63
        IF ( flags & PT_ADDLINES ) != 0
            IF ! P_BlockLinesIterator( mapx, mapy, {| ld | PIT_AddLineIntercepts( ld ) } )
                RETURN .F.
            ENDIF
        ENDIF

        IF ( flags & PT_ADDTHINGS ) != 0
            IF ! P_BlockThingsIterator( mapx, mapy, {| th | PIT_AddThingIntercepts( th ) } )
                RETURN .F.
            ENDIF
        ENDIF

        IF mapx == xt2 .AND. mapy == yt2
            EXIT
        ENDIF

        IF Shar( yintercept, FRACBITS ) == mapy
            yintercept += ystep
            mapx += mapxstep
        ELSEIF Shar( xintercept, FRACBITS ) == mapx
            xintercept += xstep
            mapy += mapystep
        ENDIF
    NEXT
RETURN P_TraverseIntercepts( trav, FRACUNIT )
