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

STATIC nSpechitBase := 0
STATIC bestslidefrac
STATIC secondslidefrac
STATIC bestslideline
STATIC secondslideline
STATIC slidemo
STATIC tmxmove
STATIC tmymove
STATIC shootthing
STATIC shootz
STATIC la_damage
STATIC aimslope
STATIC usething
STATIC bombsource
STATIC bombspot
STATIC bombdamage
STATIC tmbbox
STATIC tmthing
STATIC tmflags
STATIC tmx
STATIC tmy
STATIC tmceilingz
STATIC tmdropoffz
STATIC crushchange
STATIC nofit

#include "p_map.ch"
#include "deh_misc.ch"

MEMVAR deh_species_infighting


INIT PROCEDURE init_p_map
    LOCAL i

    PUBLIC floatok
    PUBLIC tmfloorz
    PUBLIC ceilingline
    PUBLIC spechit
    PUBLIC numspechit
    PUBLIC linetarget
    PUBLIC bulletslope
    PUBLIC attackrange

    tmbbox := {}
    FOR i := 1 TO 4
        AAdd( tmbbox, 0 )
    NEXT
    tmthing := NIL
    tmflags := 0
    tmx := 0
    tmy := 0
    floatok := .F.
    tmfloorz := 0
    tmceilingz := 0
    tmdropoffz := 0
    ceilingline := NIL
    spechit := {}
    FOR i := 1 TO MAXSPECIALCROSS
        AAdd( spechit, NIL )
    NEXT
    numspechit := 0
    linetarget := NIL
    crushchange := .F.
    nofit := .F.
    bulletslope := 0

    bestslidefrac := 0
    secondslidefrac := 0
    bestslideline := NIL
    secondslideline := NIL
    slidemo := NIL
    tmxmove := 0
    tmymove := 0
    shootthing := NIL
    shootz := 0
    la_damage := 0
    attackrange := 0
    aimslope := 0
    usething := NIL
    bombsource := NIL
    bombspot := NIL
    bombdamage := 0
    nSpechitBase := 0
RETURN

FUNCTION PIT_StompThing( thing )
    LOCAL blockdist
    MEMVAR gamemap

    IF ( thing:flags & MF_SHOOTABLE ) == 0
        RETURN .T.
    ENDIF

    blockdist := thing:radius + tmthing:radius

    IF Abs( thing:x - tmx ) >= blockdist ;
       .OR. Abs( thing:y - tmy ) >= blockdist
        RETURN .T.
    ENDIF

    IF thing == tmthing
        RETURN .T.
    ENDIF

    IF tmthing:player == NIL .AND. gamemap != 30
        RETURN .F.
    ENDIF

    P_DamageMobj( thing, tmthing, tmthing, 10000 )
RETURN .T.

FUNCTION P_TeleportMove( thing, x, y )
    LOCAL xl
    LOCAL xh
    LOCAL yl
    LOCAL yh
    LOCAL bx
    LOCAL by
    LOCAL newsubsec
    MEMVAR bmaporgx
    MEMVAR bmaporgy
    MEMVAR ceilingline
    MEMVAR numspechit
    MEMVAR tmfloorz
    MEMVAR validcount

    tmthing := thing
    tmflags := thing:flags

    tmx := x
    tmy := y

    tmbbox[ BOXTOP + 1 ] := y + tmthing:radius
    tmbbox[ BOXBOTTOM + 1 ] := y - tmthing:radius
    tmbbox[ BOXRIGHT + 1 ] := x + tmthing:radius
    tmbbox[ BOXLEFT + 1 ] := x - tmthing:radius

    newsubsec := R_PointInSubsector( x, y )
    ceilingline := NIL

    tmfloorz := newsubsec:sector:floorheight
    tmdropoffz := tmfloorz
    tmceilingz := newsubsec:sector:ceilingheight

    validcount += 1
    numspechit := 0

    xl := Shar( tmbbox[ BOXLEFT + 1 ] - bmaporgx - MAXRADIUS, MAPBLOCKSHIFT )
    xh := Shar( tmbbox[ BOXRIGHT + 1 ] - bmaporgx + MAXRADIUS, MAPBLOCKSHIFT )
    yl := Shar( tmbbox[ BOXBOTTOM + 1 ] - bmaporgy - MAXRADIUS, MAPBLOCKSHIFT )
    yh := Shar( tmbbox[ BOXTOP + 1 ] - bmaporgy + MAXRADIUS, MAPBLOCKSHIFT )

    FOR bx := xl TO xh
        FOR by := yl TO yh
            IF ! P_BlockThingsIterator( bx, by, {| th | PIT_StompThing( th ) } )
                RETURN .F.
            ENDIF
        NEXT
    NEXT

    P_UnsetThingPosition( thing )

    thing:floorz := tmfloorz
    thing:ceilingz := tmceilingz
    thing:x := x
    thing:y := y

    P_SetThingPosition( thing )
RETURN .T.

STATIC PROCEDURE SpechitOverrun( ld )
    LOCAL nAddr
    LOCAL p
    MEMVAR myargv
    MEMVAR numspechit

    IF nSpechitBase == 0
        p := M_CheckParmWithArgs( "-spechit", 1 )
        IF p > 0
            M_StrToInt( myargv[ p + 1 + 1 ], @nSpechitBase )
        ELSE
            nSpechitBase := DEFAULT_SPECHIT_MAGIC
        ENDIF
    ENDIF

    nAddr := nSpechitBase + ld:iLine * 0x3E

    SWITCH numspechit
    CASE 9
        tmbbox[ numspechit - 9 + 1 ] := nAddr
        EXIT
    CASE 10
        tmbbox[ numspechit - 9 + 1 ] := nAddr
        EXIT
    CASE 11
        tmbbox[ numspechit - 9 + 1 ] := nAddr
        EXIT
    CASE 12
        tmbbox[ numspechit - 9 + 1 ] := nAddr
        EXIT
    CASE 13
        crushchange := nAddr
        EXIT
    CASE 14
        nofit := nAddr
        EXIT
    OTHERWISE
        OutErr( "SpechitOverrun: Warning: unable to emulate an overrun where numspechit=" + ;
                LTrim( Str( numspechit ) ) + hb_eol() )
        EXIT
    ENDSWITCH
RETURN

FUNCTION PIT_CheckLine( ld )
    MEMVAR ceilingline
    MEMVAR lowfloor
    MEMVAR numspechit
    MEMVAR openbottom
    MEMVAR opentop
    MEMVAR spechit
    MEMVAR tmfloorz
    IF tmbbox[ BOXRIGHT + 1 ] <= ld:bbox[ BOXLEFT + 1 ] ;
       .OR. tmbbox[ BOXLEFT + 1 ] >= ld:bbox[ BOXRIGHT + 1 ] ;
       .OR. tmbbox[ BOXTOP + 1 ] <= ld:bbox[ BOXBOTTOM + 1 ] ;
       .OR. tmbbox[ BOXBOTTOM + 1 ] >= ld:bbox[ BOXTOP + 1 ]
        RETURN .T.
    ENDIF

    IF P_BoxOnLineSide( tmbbox, ld ) != -1
        RETURN .T.
    ENDIF

    IF ld:backsector == NIL
        RETURN .F.
    ENDIF

    IF ( tmthing:flags & MF_MISSILE ) == 0
        IF ( ld:flags & ML_BLOCKING ) != 0
            RETURN .F.
        ENDIF

        IF tmthing:player == NIL .AND. ( ld:flags & ML_BLOCKMONSTERS ) != 0
            RETURN .F.
        ENDIF
    ENDIF

    P_LineOpening( ld )

    IF opentop < tmceilingz
        tmceilingz := opentop
        ceilingline := ld
    ENDIF

    IF openbottom > tmfloorz
        tmfloorz := openbottom
    ENDIF

    IF lowfloor < tmdropoffz
        tmdropoffz := lowfloor
    ENDIF

    IF ld:special != 0
        IF numspechit + 1 > Len( spechit )
            AAdd( spechit, NIL )
        ENDIF
        spechit[ numspechit + 1 ] := ld
        numspechit += 1

        IF numspechit > MAXSPECIALCROSS_ORIGINAL
            SpechitOverrun( ld )
        ENDIF
    ENDIF
RETURN .T.

FUNCTION PIT_CheckThing( thing )
    LOCAL blockdist
    LOCAL solid
    LOCAL damage

    IF ( thing:flags & ( ( MF_SOLID | MF_SPECIAL ) | MF_SHOOTABLE ) ) == 0
        RETURN .T.
    ENDIF

    blockdist := thing:radius + tmthing:radius

    IF Abs( thing:x - tmx ) >= blockdist ;
       .OR. Abs( thing:y - tmy ) >= blockdist
        RETURN .T.
    ENDIF

    IF thing == tmthing
        RETURN .T.
    ENDIF

    IF ( tmthing:flags & MF_SKULLFLY ) != 0
        damage := ( ( P_Random() % 8 ) + 1 ) * tmthing:info:damage

        P_DamageMobj( thing, tmthing, tmthing, damage )

        tmthing:flags := ( tmthing:flags & ( MF_SKULLFLY ^^ 0xFFFFFFFF ) )
        tmthing:momx := 0
        tmthing:momy := 0
        tmthing:momz := 0

        P_SetMobjState( tmthing, tmthing:info:spawnstate )
        RETURN .F.
    ENDIF

    IF ( tmthing:flags & MF_MISSILE ) != 0
        IF tmthing:z > thing:z + thing:height
            RETURN .T.
        ENDIF
        IF tmthing:z + tmthing:height < thing:z
            RETURN .T.
        ENDIF

        IF tmthing:target != NIL ;
           .AND. ( tmthing:target:type == thing:type ;
                   .OR. ( tmthing:target:type == MT_KNIGHT .AND. thing:type == MT_BRUISER ) ;
                   .OR. ( tmthing:target:type == MT_BRUISER .AND. thing:type == MT_KNIGHT ) )
            IF thing == tmthing:target
                RETURN .T.
            ENDIF

            IF thing:type != MT_PLAYER .AND. deh_species_infighting == 0
                RETURN .F.
            ENDIF
        ENDIF

        IF ( thing:flags & MF_SHOOTABLE ) == 0
            RETURN ( thing:flags & MF_SOLID ) == 0
        ENDIF

        damage := ( ( P_Random() % 8 ) + 1 ) * tmthing:info:damage
        P_DamageMobj( thing, tmthing, tmthing:target, damage )
        RETURN .F.
    ENDIF

    IF ( thing:flags & MF_SPECIAL ) != 0
        solid := ( thing:flags & MF_SOLID )
        IF ( tmflags & MF_PICKUP ) != 0
            P_TouchSpecialThing( thing, tmthing )
        ENDIF
        RETURN solid == 0
    ENDIF
RETURN ( thing:flags & MF_SOLID ) == 0

FUNCTION P_CheckPosition( thing, x, y )
    LOCAL xl
    LOCAL xh
    LOCAL yl
    LOCAL yh
    LOCAL bx
    LOCAL by
    LOCAL newsubsec
    MEMVAR bmaporgx
    MEMVAR bmaporgy
    MEMVAR ceilingline
    MEMVAR numspechit
    MEMVAR tmfloorz
    MEMVAR validcount

    tmthing := thing
    tmflags := thing:flags

    tmx := x
    tmy := y

    tmbbox[ BOXTOP + 1 ] := y + tmthing:radius
    tmbbox[ BOXBOTTOM + 1 ] := y - tmthing:radius
    tmbbox[ BOXRIGHT + 1 ] := x + tmthing:radius
    tmbbox[ BOXLEFT + 1 ] := x - tmthing:radius

    newsubsec := R_PointInSubsector( x, y )
    ceilingline := NIL

    tmfloorz := newsubsec:sector:floorheight
    tmdropoffz := tmfloorz
    tmceilingz := newsubsec:sector:ceilingheight

    validcount += 1
    numspechit := 0

    IF ( tmflags & MF_NOCLIP ) != 0
        RETURN .T.
    ENDIF

    xl := Shar( tmbbox[ BOXLEFT + 1 ] - bmaporgx - MAXRADIUS, MAPBLOCKSHIFT )
    xh := Shar( tmbbox[ BOXRIGHT + 1 ] - bmaporgx + MAXRADIUS, MAPBLOCKSHIFT )
    yl := Shar( tmbbox[ BOXBOTTOM + 1 ] - bmaporgy - MAXRADIUS, MAPBLOCKSHIFT )
    yh := Shar( tmbbox[ BOXTOP + 1 ] - bmaporgy + MAXRADIUS, MAPBLOCKSHIFT )

    FOR bx := xl TO xh
        FOR by := yl TO yh
            IF ! P_BlockThingsIterator( bx, by, {| th | PIT_CheckThing( th ) } )
                RETURN .F.
            ENDIF
        NEXT
    NEXT

    xl := Shar( tmbbox[ BOXLEFT + 1 ] - bmaporgx, MAPBLOCKSHIFT )
    xh := Shar( tmbbox[ BOXRIGHT + 1 ] - bmaporgx, MAPBLOCKSHIFT )
    yl := Shar( tmbbox[ BOXBOTTOM + 1 ] - bmaporgy, MAPBLOCKSHIFT )
    yh := Shar( tmbbox[ BOXTOP + 1 ] - bmaporgy, MAPBLOCKSHIFT )

    FOR bx := xl TO xh
        FOR by := yl TO yh
            IF ! P_BlockLinesIterator( bx, by, {| ln | PIT_CheckLine( ln ) } )
                RETURN .F.
            ENDIF
        NEXT
    NEXT
RETURN .T.

FUNCTION P_TryMove( thing, x, y )
    LOCAL oldx
    LOCAL oldy
    LOCAL side
    LOCAL oldside
    LOCAL ld
    MEMVAR floatok
    MEMVAR numspechit
    MEMVAR spechit
    MEMVAR tmfloorz

    floatok := .F.
    IF ! P_CheckPosition( thing, x, y )
        RETURN .F.
    ENDIF

    IF ( thing:flags & MF_NOCLIP ) == 0
        IF tmceilingz - tmfloorz < thing:height
            RETURN .F.
        ENDIF

        floatok := .T.

        IF ( thing:flags & MF_TELEPORT ) == 0 ;
           .AND. tmceilingz - thing:z < thing:height
            RETURN .F.
        ENDIF

        IF ( thing:flags & MF_TELEPORT ) == 0 ;
           .AND. tmfloorz - thing:z > 24 * FRACUNIT
            RETURN .F.
        ENDIF

        IF ( thing:flags & ( MF_DROPOFF | MF_FLOAT ) ) == 0 ;
           .AND. tmfloorz - tmdropoffz > 24 * FRACUNIT
            RETURN .F.
        ENDIF
    ENDIF

    P_UnsetThingPosition( thing )

    oldx := thing:x
    oldy := thing:y
    thing:floorz := tmfloorz
    thing:ceilingz := tmceilingz
    thing:x := x
    thing:y := y

    P_SetThingPosition( thing )

    IF ( thing:flags & ( MF_TELEPORT | MF_NOCLIP ) ) == 0
        DO WHILE numspechit > 0
            numspechit -= 1
            ld := spechit[ numspechit + 1 ]
            side := P_PointOnLineSide( thing:x, thing:y, ld )
            oldside := P_PointOnLineSide( oldx, oldy, ld )
            IF side != oldside
                IF ld:special != 0
                    P_CrossSpecialLine( ld:iLine, oldside, thing )
                ENDIF
            ENDIF
        ENDDO
    ENDIF
RETURN .T.

FUNCTION P_ThingHeightClip( thing )
    LOCAL onfloor
    MEMVAR tmfloorz

    onfloor := ( thing:z == thing:floorz )

    P_CheckPosition( thing, thing:x, thing:y )

    thing:floorz := tmfloorz
    thing:ceilingz := tmceilingz

    IF onfloor
        thing:z := thing:floorz
    ELSE
        IF thing:z + thing:height > thing:ceilingz
            thing:z := thing:ceilingz - thing:height
        ENDIF
    ENDIF

    IF thing:ceilingz - thing:floorz < thing:height
        RETURN .F.
    ENDIF
RETURN .T.

FUNCTION P_HitSlideLine( ld )
    LOCAL side
    LOCAL lineangle
    LOCAL moveangle
    LOCAL deltaangle
    LOCAL movelen
    LOCAL newlen
    MEMVAR finecosine
    MEMVAR finesine

    IF ld:slopetype == ST_HORIZONTAL
        tmymove := 0
        RETURN NIL
    ENDIF

    IF ld:slopetype == ST_VERTICAL
        tmxmove := 0
        RETURN NIL
    ENDIF

    side := P_PointOnLineSide( slidemo:x, slidemo:y, ld )

    lineangle := R_PointToAngle2( 0, 0, ld:dx, ld:dy )

    IF side == 1
        lineangle := ( ( lineangle + ANG180 ) & 0xFFFFFFFF )
    ENDIF

    moveangle := R_PointToAngle2( 0, 0, tmxmove, tmymove )
    deltaangle := ( ( moveangle - lineangle ) & 0xFFFFFFFF )

    IF deltaangle > ANG180
        deltaangle := ( ( deltaangle + ANG180 ) & 0xFFFFFFFF )
    ENDIF

    lineangle := UShr( lineangle, ANGLETOFINESHIFT )
    deltaangle := UShr( deltaangle, ANGLETOFINESHIFT )

    movelen := P_AproxDistance( tmxmove, tmymove )
    newlen := FixedMul( movelen, finecosine[ deltaangle + 1 ] )

    tmxmove := FixedMul( newlen, finecosine[ lineangle + 1 ] )
    tmymove := FixedMul( newlen, finesine[ lineangle + 1 ] )
RETURN NIL

FUNCTION PTR_SlideTraverse( in )
    LOCAL li
    LOCAL lBlocking
    MEMVAR openbottom
    MEMVAR openrange
    MEMVAR opentop

    IF ! in:isaline
        I_Error( "PTR_SlideTraverse: not a line?" )
    ENDIF

    li := in:d:line
    lBlocking := .F.

    IF ( li:flags & ML_TWOSIDED ) == 0
        IF P_PointOnLineSide( slidemo:x, slidemo:y, li ) != 0
            RETURN .T.
        ENDIF
        lBlocking := .T.
    ELSE
        P_LineOpening( li )

        IF openrange < slidemo:height
            lBlocking := .T.
        ELSEIF opentop - slidemo:z < slidemo:height
            lBlocking := .T.
        ELSEIF openbottom - slidemo:z > 24 * FRACUNIT
            lBlocking := .T.
        ENDIF
    ENDIF

    IF ! lBlocking
        RETURN .T.
    ENDIF

    IF in:frac < bestslidefrac
        secondslidefrac := bestslidefrac
        secondslideline := bestslideline
        bestslidefrac := in:frac
        bestslideline := li
    ENDIF
RETURN .F.

FUNCTION P_SlideMove( mo )
    LOCAL leadx
    LOCAL leady
    LOCAL trailx
    LOCAL traily
    LOCAL newx
    LOCAL newy
    LOCAL hitcount
    LOCAL lStairstep

    slidemo := mo
    hitcount := 0

    DO WHILE .T.
        hitcount += 1
        lStairstep := .F.

        IF hitcount == 3
            lStairstep := .T.
        ELSE
            IF mo:momx > 0
                leadx := mo:x + mo:radius
                trailx := mo:x - mo:radius
            ELSE
                leadx := mo:x - mo:radius
                trailx := mo:x + mo:radius
            ENDIF

            IF mo:momy > 0
                leady := mo:y + mo:radius
                traily := mo:y - mo:radius
            ELSE
                leady := mo:y - mo:radius
                traily := mo:y + mo:radius
            ENDIF

            bestslidefrac := FRACUNIT + 1

            P_PathTraverse( leadx, leady, leadx + mo:momx, leady + mo:momy, ;
                            PT_ADDLINES, {| in | PTR_SlideTraverse( in ) } )
            P_PathTraverse( trailx, leady, trailx + mo:momx, leady + mo:momy, ;
                            PT_ADDLINES, {| in | PTR_SlideTraverse( in ) } )
            P_PathTraverse( leadx, traily, leadx + mo:momx, traily + mo:momy, ;
                            PT_ADDLINES, {| in | PTR_SlideTraverse( in ) } )

            IF bestslidefrac == FRACUNIT + 1
                lStairstep := .T.
            ENDIF
        ENDIF

        IF lStairstep
            IF ! P_TryMove( mo, mo:x, mo:y + mo:momy )
                P_TryMove( mo, mo:x + mo:momx, mo:y )
            ENDIF
            RETURN NIL
        ENDIF

        bestslidefrac -= 0x800
        IF bestslidefrac > 0
            newx := FixedMul( mo:momx, bestslidefrac )
            newy := FixedMul( mo:momy, bestslidefrac )

            IF ! P_TryMove( mo, mo:x + newx, mo:y + newy )
                IF ! P_TryMove( mo, mo:x, mo:y + mo:momy )
                    P_TryMove( mo, mo:x + mo:momx, mo:y )
                ENDIF
                RETURN NIL
            ENDIF
        ENDIF

        bestslidefrac := FRACUNIT - ( bestslidefrac + 0x800 )

        IF bestslidefrac > FRACUNIT
            bestslidefrac := FRACUNIT
        ENDIF

        IF bestslidefrac <= 0
            RETURN NIL
        ENDIF

        tmxmove := FixedMul( mo:momx, bestslidefrac )
        tmymove := FixedMul( mo:momy, bestslidefrac )

        P_HitSlideLine( bestslideline )

        mo:momx := tmxmove
        mo:momy := tmymove

        IF P_TryMove( mo, mo:x + tmxmove, mo:y + tmymove )
            EXIT
        ENDIF
    ENDDO
RETURN NIL

FUNCTION PTR_AimTraverse( in )
    LOCAL li
    LOCAL th
    LOCAL slope
    LOCAL thingtopslope
    LOCAL thingbottomslope
    LOCAL dist
    MEMVAR attackrange
    MEMVAR bottomslope
    MEMVAR linetarget
    MEMVAR openbottom
    MEMVAR opentop
    MEMVAR topslope

    IF in:isaline
        li := in:d:line

        IF ( li:flags & ML_TWOSIDED ) == 0
            RETURN .F.
        ENDIF

        P_LineOpening( li )

        IF openbottom >= opentop
            RETURN .F.
        ENDIF

        dist := FixedMul( attackrange, in:frac )

        IF li:backsector == NIL ;
           .OR. li:frontsector:floorheight != li:backsector:floorheight
            slope := FixedDiv( openbottom - shootz, dist )
            IF slope > bottomslope
                bottomslope := slope
            ENDIF
        ENDIF

        IF li:backsector == NIL ;
           .OR. li:frontsector:ceilingheight != li:backsector:ceilingheight
            slope := FixedDiv( opentop - shootz, dist )
            IF slope < topslope
                topslope := slope
            ENDIF
        ENDIF

        IF topslope <= bottomslope
            RETURN .F.
        ENDIF
        RETURN .T.
    ENDIF

    th := in:d:thing
    IF th == shootthing
        RETURN .T.
    ENDIF

    IF ( th:flags & MF_SHOOTABLE ) == 0
        RETURN .T.
    ENDIF

    dist := FixedMul( attackrange, in:frac )
    thingtopslope := FixedDiv( th:z + th:height - shootz, dist )

    IF thingtopslope < bottomslope
        RETURN .T.
    ENDIF

    thingbottomslope := FixedDiv( th:z - shootz, dist )

    IF thingbottomslope > topslope
        RETURN .T.
    ENDIF

    IF thingtopslope > topslope
        thingtopslope := topslope
    ENDIF

    IF thingbottomslope < bottomslope
        thingbottomslope := bottomslope
    ENDIF

    aimslope := Int( ( thingtopslope + thingbottomslope ) / 2 )
    linetarget := th
RETURN .F.

FUNCTION PTR_ShootTraverse( in )
    LOCAL x
    LOCAL y
    LOCAL z
    LOCAL frac
    LOCAL li
    LOCAL th
    LOCAL slope
    LOCAL dist
    LOCAL thingtopslope
    LOCAL thingbottomslope
    LOCAL lHitLine
    MEMVAR attackrange
    MEMVAR openbottom
    MEMVAR opentop
    MEMVAR skyflatnum
    MEMVAR trace

    IF in:isaline
        li := in:d:line
        lHitLine := .F.

        IF li:special != 0
            P_ShootSpecialLine( shootthing, li )
        ENDIF

        IF ( li:flags & ML_TWOSIDED ) == 0
            lHitLine := .T.
        ELSE
            P_LineOpening( li )

            dist := FixedMul( attackrange, in:frac )

            IF li:backsector == NIL
                slope := FixedDiv( openbottom - shootz, dist )
                IF slope > aimslope
                    lHitLine := .T.
                ELSE
                    slope := FixedDiv( opentop - shootz, dist )
                    IF slope < aimslope
                        lHitLine := .T.
                    ENDIF
                ENDIF
            ELSE
                IF li:frontsector:floorheight != li:backsector:floorheight
                    slope := FixedDiv( openbottom - shootz, dist )
                    IF slope > aimslope
                        lHitLine := .T.
                    ENDIF
                ENDIF

                IF ! lHitLine .AND. li:frontsector:ceilingheight != li:backsector:ceilingheight
                    slope := FixedDiv( opentop - shootz, dist )
                    IF slope < aimslope
                        lHitLine := .T.
                    ENDIF
                ENDIF
            ENDIF
        ENDIF

        IF ! lHitLine
            RETURN .T.
        ENDIF

        frac := in:frac - FixedDiv( 4 * FRACUNIT, attackrange )
        x := trace:x + FixedMul( trace:dx, frac )
        y := trace:y + FixedMul( trace:dy, frac )
        z := shootz + FixedMul( aimslope, FixedMul( frac, attackrange ) )

        IF li:frontsector:ceilingpic == skyflatnum
            IF z > li:frontsector:ceilingheight
                RETURN .F.
            ENDIF

            IF li:backsector != NIL .AND. li:backsector:ceilingpic == skyflatnum
                RETURN .F.
            ENDIF
        ENDIF

        P_SpawnPuff( x, y, z )
        RETURN .F.
    ENDIF

    th := in:d:thing
    IF th == shootthing
        RETURN .T.
    ENDIF

    IF ( th:flags & MF_SHOOTABLE ) == 0
        RETURN .T.
    ENDIF

    dist := FixedMul( attackrange, in:frac )
    thingtopslope := FixedDiv( th:z + th:height - shootz, dist )

    IF thingtopslope < aimslope
        RETURN .T.
    ENDIF

    thingbottomslope := FixedDiv( th:z - shootz, dist )

    IF thingbottomslope > aimslope
        RETURN .T.
    ENDIF

    frac := in:frac - FixedDiv( 10 * FRACUNIT, attackrange )

    x := trace:x + FixedMul( trace:dx, frac )
    y := trace:y + FixedMul( trace:dy, frac )
    z := shootz + FixedMul( aimslope, FixedMul( frac, attackrange ) )

    IF ( in:d:thing:flags & MF_NOBLOOD ) != 0
        P_SpawnPuff( x, y, z )
    ELSE
        P_SpawnBlood( x, y, z, la_damage )
    ENDIF

    IF la_damage != 0
        P_DamageMobj( th, shootthing, shootthing, la_damage )
    ENDIF
RETURN .F.

FUNCTION P_AimLineAttack( t1, angle, distance )
    LOCAL x2
    LOCAL y2
    MEMVAR attackrange
    MEMVAR bottomslope
    MEMVAR finecosine
    MEMVAR finesine
    MEMVAR linetarget
    MEMVAR topslope

    t1 := P_SubstNullMobj( t1 )

    angle := UShr( angle, ANGLETOFINESHIFT )
    shootthing := t1

    x2 := t1:x + Shar( distance, FRACBITS ) * finecosine[ angle + 1 ]
    y2 := t1:y + Shar( distance, FRACBITS ) * finesine[ angle + 1 ]
    shootz := t1:z + Shar( t1:height, 1 ) + 8 * FRACUNIT

    topslope := Int( 100 * FRACUNIT / 160 )
    bottomslope := Int( -100 * FRACUNIT / 160 )

    attackrange := distance
    linetarget := NIL

    P_PathTraverse( t1:x, t1:y, ;
                    x2, y2, ;
                    ( PT_ADDLINES | PT_ADDTHINGS ), ;
                    {| in | PTR_AimTraverse( in ) } )

    IF linetarget != NIL
        RETURN aimslope
    ENDIF
RETURN 0

FUNCTION P_LineAttack( t1, angle, distance, slope, damage )
    LOCAL x2
    LOCAL y2
    MEMVAR attackrange
    MEMVAR finecosine
    MEMVAR finesine

    angle := UShr( angle, ANGLETOFINESHIFT )
    shootthing := t1
    la_damage := damage
    x2 := t1:x + Shar( distance, FRACBITS ) * finecosine[ angle + 1 ]
    y2 := t1:y + Shar( distance, FRACBITS ) * finesine[ angle + 1 ]
    shootz := t1:z + Shar( t1:height, 1 ) + 8 * FRACUNIT
    attackrange := distance
    aimslope := slope

    P_PathTraverse( t1:x, t1:y, ;
                    x2, y2, ;
                    ( PT_ADDLINES | PT_ADDTHINGS ), ;
                    {| in | PTR_ShootTraverse( in ) } )
RETURN NIL

FUNCTION PTR_UseTraverse( in )
    LOCAL side
    MEMVAR openrange

    IF in:d:line:special == 0
        P_LineOpening( in:d:line )
        IF openrange <= 0
            S_StartSound( usething, sfx_noway )
            RETURN .F.
        ENDIF
        RETURN .T.
    ENDIF

    side := 0
    IF P_PointOnLineSide( usething:x, usething:y, in:d:line ) == 1
        side := 1
    ENDIF

    P_UseSpecialLine( usething, in:d:line, side )
RETURN .F.

FUNCTION P_UseLines( player )
    LOCAL angle
    LOCAL x1
    LOCAL y1
    LOCAL x2
    LOCAL y2
    MEMVAR finecosine
    MEMVAR finesine

    usething := player:mo

    angle := UShr( player:mo:angle, ANGLETOFINESHIFT )

    x1 := player:mo:x
    y1 := player:mo:y
    x2 := x1 + Shar( USERANGE, FRACBITS ) * finecosine[ angle + 1 ]
    y2 := y1 + Shar( USERANGE, FRACBITS ) * finesine[ angle + 1 ]

    P_PathTraverse( x1, y1, x2, y2, PT_ADDLINES, {| in | PTR_UseTraverse( in ) } )
RETURN NIL

FUNCTION PIT_RadiusAttack( thing )
    LOCAL dx
    LOCAL dy
    LOCAL dist

    IF ( thing:flags & MF_SHOOTABLE ) == 0
        RETURN .T.
    ENDIF

    IF thing:type == MT_CYBORG .OR. thing:type == MT_SPIDER
        RETURN .T.
    ENDIF

    dx := Abs( thing:x - bombspot:x )
    dy := Abs( thing:y - bombspot:y )

    dist := iif( dx > dy, dx, dy )
    dist := Shar( dist - thing:radius, FRACBITS )

    IF dist < 0
        dist := 0
    ENDIF

    IF dist >= bombdamage
        RETURN .T.
    ENDIF

    IF P_CheckSight( thing, bombspot )
        P_DamageMobj( thing, bombspot, bombsource, bombdamage - dist )
    ENDIF
RETURN .T.

FUNCTION P_RadiusAttack( spot, source, damage )
    LOCAL x
    LOCAL y
    LOCAL xl
    LOCAL xh
    LOCAL yl
    LOCAL yh
    LOCAL dist
    MEMVAR bmaporgx
    MEMVAR bmaporgy

    dist := damage * FRACUNIT + MAXRADIUS
    yh := Shar( spot:y + dist - bmaporgy, MAPBLOCKSHIFT )
    yl := Shar( spot:y - dist - bmaporgy, MAPBLOCKSHIFT )
    xh := Shar( spot:x + dist - bmaporgx, MAPBLOCKSHIFT )
    xl := Shar( spot:x - dist - bmaporgx, MAPBLOCKSHIFT )
    bombspot := spot
    bombsource := source
    bombdamage := damage

    FOR y := yl TO yh
        FOR x := xl TO xh
            P_BlockThingsIterator( x, y, {| th | PIT_RadiusAttack( th ) } )
        NEXT
    NEXT
RETURN NIL

FUNCTION PIT_ChangeSector( thing )
    LOCAL mo
    MEMVAR leveltime

    IF P_ThingHeightClip( thing )
        RETURN .T.
    ENDIF

    IF thing:health <= 0
        P_SetMobjState( thing, S_GIBS )

        thing:flags := ( thing:flags & ( MF_SOLID ^^ 0xFFFFFFFF ) )
        thing:height := 0
        thing:radius := 0
        RETURN .T.
    ENDIF

    IF ( thing:flags & MF_DROPPED ) != 0
        P_RemoveMobj( thing )
        RETURN .T.
    ENDIF

    IF ( thing:flags & MF_SHOOTABLE ) == 0
        RETURN .T.
    ENDIF

    nofit := .T.

    IF crushchange .AND. ( leveltime & 3 ) == 0
        P_DamageMobj( thing, NIL, NIL, 10 )

        mo := P_SpawnMobj( thing:x, ;
                           thing:y, ;
                           thing:z + Int( thing:height / 2 ), MT_BLOOD )

        mo:momx := ( P_Random() - P_Random() ) * 4096
        mo:momy := ( P_Random() - P_Random() ) * 4096
    ENDIF
RETURN .T.

FUNCTION P_ChangeSector( sector, crunch )
    LOCAL x
    LOCAL y

    nofit := .F.
    crushchange := crunch

    FOR x := sector:blockbox[ BOXLEFT + 1 ] TO sector:blockbox[ BOXRIGHT + 1 ]
        FOR y := sector:blockbox[ BOXBOTTOM + 1 ] TO sector:blockbox[ BOXTOP + 1 ]
            P_BlockThingsIterator( x, y, {| th | PIT_ChangeSector( th ) } )
        NEXT
    NEXT
RETURN nofit
