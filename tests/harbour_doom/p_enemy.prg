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

STATIC opposite := {}
STATIC diags := {}
STATIC xspeed := {}
STATIC yspeed := {}
STATIC soundtarget
STATIC corpsehit
STATIC vileobj
STATIC viletryx
STATIC viletryy
STATIC braintargets := {}
STATIC numbraintargets
STATIC braintargeton
STATIC easy
STATIC nTraceAngle

#include "p_enemy.ch"

CLASS p_tagline_t
    DATA tag
    DATA special
    METHOD New()
ENDCLASS


METHOD New() CLASS p_tagline_t
    ::tag     := 0
    ::special := 0
RETURN Self

INIT PROCEDURE init_p_enemy
    LOCAL i

    opposite := { DI_WEST, DI_SOUTHWEST, DI_SOUTH, DI_SOUTHEAST, ;
                  DI_EAST, DI_NORTHEAST, DI_NORTH, DI_NORTHWEST, DI_NODIR }
    diags := { DI_NORTHWEST, DI_NORTHEAST, DI_SOUTHWEST, DI_SOUTHEAST }
    xspeed := { FRACUNIT, 47000, 0, -47000, -FRACUNIT, -47000, 0, 47000 }
    yspeed := { 0, 47000, FRACUNIT, 47000, 0, -47000, -FRACUNIT, -47000 }

    soundtarget := NIL
    corpsehit := NIL
    vileobj := NIL
    viletryx := 0
    viletryy := 0

    braintargets := {}
    FOR i := 1 TO 32
        AAdd( braintargets, NIL )
    NEXT
    numbraintargets := 0
    braintargeton := 0
    easy := 0
    nTraceAngle := TRACEANGLE
RETURN

STATIC FUNCTION AsInt32( n )
    n := ( n & 0xFFFFFFFF )
    IF n >= 2147483648
        RETURN n - 4294967296
    ENDIF
RETURN n

STATIC FUNCTION FineCos( nExact )
    MEMVAR finecosine
RETURN finecosine[ nExact + 1 ]

STATIC FUNCTION FineSin( nExact )
    MEMVAR finesine
RETURN finesine[ nExact + 1 ]

STATIC FUNCTION ThinkName( o )
    LOCAL x

    IF o == NIL
        RETURN ""
    ENDIF
    IF __objHasMsg( o, "THINKFN" ) .AND. ValType( o:thinkfn ) == "C" .AND. ! Empty( o:thinkfn )
        RETURN o:thinkfn
    ENDIF
    IF __objHasMsg( o, "THINKER" ) .AND. o:thinker != NIL
        IF __objHasMsg( o:thinker, "THINKFN" ) .AND. ValType( o:thinker:thinkfn ) == "C" .AND. ! Empty( o:thinker:thinkfn )
            RETURN o:thinker:thinkfn
        ENDIF
        IF o:thinker:function != NIL
            x := o:thinker:function:acp1
            IF ValType( x ) == "C"
                RETURN x
            ENDIF
        ENDIF
    ENDIF
    IF __objHasMsg( o, "FUNCTION" ) .AND. o:function != NIL .AND. ValType( o:function ) == "O"
        x := o:function:acp1
        IF ValType( x ) == "C"
            RETURN x
        ENDIF
    ENDIF
RETURN ""

STATIC FUNCTION ThinkerToMobj( th )
    LOCAL o

    IF th == NIL
        RETURN NIL
    ENDIF
    IF __objHasMsg( th, "TYPE" ) .AND. __objHasMsg( th, "HEALTH" )
        IF ValType( th:type ) == "N" .AND. ValType( th:health ) == "N"
            RETURN th
        ENDIF
    ENDIF
    IF __objHasMsg( th, "OWNER" ) .AND. th:owner != NIL
        o := th:owner
        IF __objHasMsg( o, "TYPE" ) .AND. __objHasMsg( o, "HEALTH" )
            IF ValType( o:type ) == "N" .AND. ValType( o:health ) == "N"
                RETURN o
            ENDIF
        ENDIF
    ENDIF
RETURN NIL

STATIC FUNCTION IsMobjThinker( th )
    IF ThinkName( th ) == "P_MobjThinker"
        RETURN .T.
    ENDIF
    IF ThinkerToMobj( th ) != NIL
        RETURN .T.
    ENDIF
RETURN .F.

FUNCTION P_RecursiveSound( sec, soundblocks )
    LOCAL i
    LOCAL check
    LOCAL other
    MEMVAR openrange
    MEMVAR sides
    MEMVAR validcount

    IF sec:validcount == validcount .AND. sec:soundtraversed <= soundblocks + 1
        RETURN NIL
    ENDIF

    sec:validcount := validcount
    sec:soundtraversed := soundblocks + 1
    sec:soundtarget := soundtarget

    FOR i := 0 TO sec:linecount - 1
        check := sec:lines[ i + 1 ]
        IF ( check:flags & ML_TWOSIDED ) == 0
            LOOP
        ENDIF

        P_LineOpening( check )

        IF openrange <= 0
            LOOP
        ENDIF

        IF sides[ check:sidenum[ 1 ] + 1 ]:sector == sec
            other := sides[ check:sidenum[ 2 ] + 1 ]:sector
        ELSE
            other := sides[ check:sidenum[ 1 ] + 1 ]:sector
        ENDIF

        IF ( check:flags & ML_SOUNDBLOCK ) != 0
            IF soundblocks == 0
                P_RecursiveSound( other, 1 )
            ENDIF
        ELSE
            P_RecursiveSound( other, soundblocks )
        ENDIF
    NEXT
RETURN NIL

FUNCTION P_NoiseAlert( target, emmiter )
    MEMVAR validcount
    soundtarget := target
    validcount := validcount + 1
    P_RecursiveSound( emmiter:subsector:sector, 0 )
RETURN NIL

FUNCTION P_CheckMeleeRange( actor )
    LOCAL pl
    LOCAL dist

    IF actor:target == NIL
        RETURN .F.
    ENDIF

    pl := actor:target
    dist := P_AproxDistance( pl:x - actor:x, pl:y - actor:y )

    IF dist >= MELEERANGE - 20 * FRACUNIT + pl:info:radius
        RETURN .F.
    ENDIF

    IF ! P_CheckSight( actor, actor:target )
        RETURN .F.
    ENDIF
RETURN .T.

FUNCTION P_CheckMissileRange( actor )
    LOCAL dist

    IF ! P_CheckSight( actor, actor:target )
        RETURN .F.
    ENDIF

    IF ( actor:flags & MF_JUSTHIT ) != 0
        actor:flags := ( actor:flags & ( MF_JUSTHIT ^^ 0xFFFFFFFF ) )
        RETURN .T.
    ENDIF

    IF actor:reactiontime != 0
        RETURN .F.
    ENDIF

    dist := P_AproxDistance( actor:x - actor:target:x, ;
                             actor:y - actor:target:y ) - 64 * FRACUNIT

    IF actor:info:meleestate == 0
        dist -= 128 * FRACUNIT
    ENDIF

    dist := Shar( dist, 16 )

    IF actor:type == MT_VILE
        IF dist > 14 * 64
            RETURN .F.
        ENDIF
    ENDIF

    IF actor:type == MT_UNDEAD
        IF dist < 196
            RETURN .F.
        ENDIF
        dist := Shar( dist, 1 )
    ENDIF

    IF actor:type == MT_CYBORG .OR. actor:type == MT_SPIDER .OR. actor:type == MT_SKULL
        dist := Shar( dist, 1 )
    ENDIF

    IF dist > 200
        dist := 200
    ENDIF

    IF actor:type == MT_CYBORG .AND. dist > 160
        dist := 160
    ENDIF

    IF P_Random() < dist
        RETURN .F.
    ENDIF
RETURN .T.

FUNCTION P_Move( actor )
    LOCAL tryx
    LOCAL tryy
    LOCAL ld
    LOCAL try_ok
    LOCAL good
    MEMVAR floatok
    MEMVAR numspechit
    MEMVAR spechit
    MEMVAR tmfloorz

    IF actor:movedir == DI_NODIR
        RETURN .F.
    ENDIF

    IF actor:movedir < 0 .OR. actor:movedir >= 8
        I_Error( "Weird actor->movedir!" )
    ENDIF

    tryx := actor:x + actor:info:speed * xspeed[ actor:movedir + 1 ]
    tryy := actor:y + actor:info:speed * yspeed[ actor:movedir + 1 ]

    try_ok := P_TryMove( actor, tryx, tryy )

    IF ! try_ok
        IF ( actor:flags & MF_FLOAT ) != 0 .AND. floatok
            IF actor:z < tmfloorz
                actor:z += FLOATSPEED
            ELSE
                actor:z -= FLOATSPEED
            ENDIF
            actor:flags := ( actor:flags | MF_INFLOAT )
            RETURN .T.
        ENDIF

        IF numspechit == 0
            RETURN .F.
        ENDIF

        actor:movedir := DI_NODIR
        good := .F.
        DO WHILE numspechit > 0
            numspechit := numspechit - 1
            ld := spechit[ numspechit + 1 ]
            IF P_UseSpecialLine( actor, ld, 0 )
                good := .T.
            ENDIF
        ENDDO
        RETURN good
    ELSE
        actor:flags := ( actor:flags & ( MF_INFLOAT ^^ 0xFFFFFFFF ) )
    ENDIF

    IF ( actor:flags & MF_FLOAT ) == 0
        actor:z := actor:floorz
    ENDIF
RETURN .T.

FUNCTION P_TryWalk( actor )
    IF ! P_Move( actor )
        RETURN .F.
    ENDIF
    actor:movecount := ( P_Random() & 15 )
RETURN .T.

FUNCTION P_NewChaseDir( actor )
    LOCAL deltax
    LOCAL deltay
    LOCAL d
    LOCAL tdir
    LOCAL olddir
    LOCAL turnaround
    LOCAL nDiag

    IF actor:target == NIL
        I_Error( "P_NewChaseDir: called with no target" )
    ENDIF

    olddir := actor:movedir
    turnaround := opposite[ olddir + 1 ]

    deltax := actor:target:x - actor:x
    deltay := actor:target:y - actor:y

    d := Array( 3 )

    IF deltax > 10 * FRACUNIT
        d[ 2 ] := DI_EAST
    ELSEIF deltax < -10 * FRACUNIT
        d[ 2 ] := DI_WEST
    ELSE
        d[ 2 ] := DI_NODIR
    ENDIF

    IF deltay < -10 * FRACUNIT
        d[ 3 ] := DI_SOUTH
    ELSEIF deltay > 10 * FRACUNIT
        d[ 3 ] := DI_NORTH
    ELSE
        d[ 3 ] := DI_NODIR
    ENDIF

    IF d[ 2 ] != DI_NODIR .AND. d[ 3 ] != DI_NODIR
        nDiag := iif( deltay < 0, 2, 0 ) + iif( deltax > 0, 1, 0 )
        actor:movedir := diags[ nDiag + 1 ]
        IF actor:movedir != turnaround .AND. P_TryWalk( actor )
            RETURN NIL
        ENDIF
    ENDIF

    IF P_Random() > 200 .OR. Abs( deltay ) > Abs( deltax )
        tdir := d[ 2 ]
        d[ 2 ] := d[ 3 ]
        d[ 3 ] := tdir
    ENDIF

    IF d[ 2 ] == turnaround
        d[ 2 ] := DI_NODIR
    ENDIF
    IF d[ 3 ] == turnaround
        d[ 3 ] := DI_NODIR
    ENDIF

    IF d[ 2 ] != DI_NODIR
        actor:movedir := d[ 2 ]
        IF P_TryWalk( actor )
            RETURN NIL
        ENDIF
    ENDIF

    IF d[ 3 ] != DI_NODIR
        actor:movedir := d[ 3 ]
        IF P_TryWalk( actor )
            RETURN NIL
        ENDIF
    ENDIF

    IF olddir != DI_NODIR
        actor:movedir := olddir
        IF P_TryWalk( actor )
            RETURN NIL
        ENDIF
    ENDIF

    IF ( P_Random() & 1 ) != 0
        FOR tdir := DI_EAST TO DI_SOUTHEAST
            IF tdir != turnaround
                actor:movedir := tdir
                IF P_TryWalk( actor )
                    RETURN NIL
                ENDIF
            ENDIF
        NEXT
    ELSE
        tdir := DI_SOUTHEAST
        DO WHILE tdir >= DI_EAST
            IF tdir != turnaround
                actor:movedir := tdir
                IF P_TryWalk( actor )
                    RETURN NIL
                ENDIF
            ENDIF
            tdir := tdir - 1
        ENDDO
    ENDIF

    IF turnaround != DI_NODIR
        actor:movedir := turnaround
        IF P_TryWalk( actor )
            RETURN NIL
        ENDIF
    ENDIF

    actor:movedir := DI_NODIR
RETURN NIL

FUNCTION P_LookForPlayers( actor, allaround )
    LOCAL c
    LOCAL stop
    LOCAL player
    LOCAL an
    LOCAL dist
    MEMVAR playeringame
    MEMVAR players

    c := 0
    stop := ( ( actor:lastlook - 1 ) & 3 )

    DO WHILE .T.
        IF ! playeringame[ actor:lastlook + 1 ]
            actor:lastlook := ( ( actor:lastlook + 1 ) & 3 )
            LOOP
        ENDIF

        IF c == 2 .OR. actor:lastlook == stop
            RETURN .F.
        ENDIF
        c := c + 1

        player := players[ actor:lastlook + 1 ]

        IF player:health <= 0
            actor:lastlook := ( ( actor:lastlook + 1 ) & 3 )
            LOOP
        ENDIF

        IF ! P_CheckSight( actor, player:mo )
            actor:lastlook := ( ( actor:lastlook + 1 ) & 3 )
            LOOP
        ENDIF

        IF ! allaround
            an := ( ( R_PointToAngle2( actor:x, actor:y, player:mo:x, player:mo:y ) - actor:angle ) & 0xFFFFFFFF )
            IF an > ANG90 .AND. an < ANG270
                dist := P_AproxDistance( player:mo:x - actor:x, player:mo:y - actor:y )
                IF dist > MELEERANGE
                    actor:lastlook := ( ( actor:lastlook + 1 ) & 3 )
                    LOOP
                ENDIF
            ENDIF
        ENDIF

        actor:target := player:mo
        RETURN .T.
    ENDDO
RETURN .F.

FUNCTION A_KeenDie( mo )
    LOCAL th
    LOCAL mo2
    LOCAL junk
    MEMVAR thinkercap

    A_Fall( mo )

    IF thinkercap != NIL
        th := thinkercap:next
        DO WHILE th != NIL .AND. !( th == thinkercap )
            IF IsMobjThinker( th )
                mo2 := ThinkerToMobj( th )
                IF mo2 != NIL .AND. !( mo2 == mo ) .AND. mo2:type == mo:type .AND. mo2:health > 0
                    RETURN NIL
                ENDIF
            ENDIF
            th := th:next
        ENDDO
    ENDIF

    junk := p_tagline_t():New()
    junk:tag := 666
    EV_DoDoor( junk, vld_open )
RETURN NIL

FUNCTION A_Look( actor )
    LOCAL targ
    LOCAL lSeeYou
    LOCAL sound

    actor:threshold := 0
    targ := actor:subsector:sector:soundtarget
    lSeeYou := .F.

    IF targ != NIL .AND. ( targ:flags & MF_SHOOTABLE ) != 0
        actor:target := targ
        IF ( actor:flags & MF_AMBUSH ) != 0
            IF P_CheckSight( actor, actor:target )
                lSeeYou := .T.
            ENDIF
        ELSE
            lSeeYou := .T.
        ENDIF
    ENDIF

    IF ! lSeeYou
        IF ! P_LookForPlayers( actor, .F. )
            RETURN NIL
        ENDIF
    ENDIF

    IF actor:info:seesound != 0
        SWITCH actor:info:seesound
        CASE sfx_posit1
            sound := sfx_posit1 + ( P_Random() % 3 )
            EXIT
        CASE sfx_posit2
            sound := sfx_posit1 + ( P_Random() % 3 )
            EXIT
        CASE sfx_posit3
            sound := sfx_posit1 + ( P_Random() % 3 )
            EXIT
        CASE sfx_bgsit1
            sound := sfx_bgsit1 + ( P_Random() % 2 )
            EXIT
        CASE sfx_bgsit2
            sound := sfx_bgsit1 + ( P_Random() % 2 )
            EXIT
        OTHERWISE
            sound := actor:info:seesound
            EXIT
        ENDSWITCH

        IF actor:type == MT_SPIDER .OR. actor:type == MT_CYBORG
            S_StartSound( NIL, sound )
        ELSE
            S_StartSound( actor, sound )
        ENDIF
    ENDIF

    P_SetMobjState( actor, actor:info:seestate )
RETURN NIL

FUNCTION A_Chase( actor )
    LOCAL delta
    LOCAL lNomissile
    MEMVAR fastparm
    MEMVAR gameskill
    MEMVAR netgame
    MEMVAR leveltime

    IF actor:reactiontime != 0
        actor:reactiontime := actor:reactiontime - 1
    ENDIF

    IF actor:threshold != 0
        IF actor:target == NIL .OR. actor:target:health <= 0
            actor:threshold := 0
        ELSE
            actor:threshold := actor:threshold - 1
        ENDIF
    ENDIF

    IF actor:movedir < 8
        actor:angle := ( actor:angle & 3758096384 )
        delta := AsInt32( actor:angle - ( actor:movedir * 536870912 ) )
        IF delta > 0
            actor:angle -= Int( ANG90 / 2 )
        ELSEIF delta < 0
            actor:angle += Int( ANG90 / 2 )
        ENDIF
    ENDIF

    IF actor:target == NIL .OR. ( actor:target:flags & MF_SHOOTABLE ) == 0
        IF P_LookForPlayers( actor, .T. )
            RETURN NIL
        ENDIF
        P_SetMobjState( actor, actor:info:spawnstate )
        RETURN NIL
    ENDIF

    IF ( actor:flags & MF_JUSTATTACKED ) != 0
        actor:flags := ( actor:flags & ( MF_JUSTATTACKED ^^ 0xFFFFFFFF ) )
        IF gameskill != sk_nightmare .AND. ! fastparm
            P_NewChaseDir( actor )
        ENDIF
        RETURN NIL
    ENDIF

    IF actor:info:meleestate != 0 .AND. P_CheckMeleeRange( actor )
        IF actor:info:attacksound != 0
            S_StartSound( actor, actor:info:attacksound )
        ENDIF
        P_SetMobjState( actor, actor:info:meleestate )
        RETURN NIL
    ENDIF

    lNomissile := .T.
    IF actor:info:missilestate != 0
        lNomissile := .F.
        IF gameskill < sk_nightmare .AND. ! fastparm .AND. actor:movecount != 0
            lNomissile := .T.
        ELSE
            IF ! P_CheckMissileRange( actor )
                lNomissile := .T.
            ELSE
                P_SetMobjState( actor, actor:info:missilestate )
                actor:flags := ( actor:flags | MF_JUSTATTACKED )
                RETURN NIL
            ENDIF
        ENDIF
    ENDIF

    HB_SYMBOL_UNUSED( lNomissile )

    IF netgame .AND. actor:threshold == 0 .AND. ! P_CheckSight( actor, actor:target )
        IF P_LookForPlayers( actor, .T. )
            RETURN NIL
        ENDIF
    ENDIF

    actor:movecount := actor:movecount - 1
    IF actor:movecount < 0 .OR. ! P_Move( actor )
        P_NewChaseDir( actor )
    ENDIF

    IF actor:info:activesound != 0 .AND. P_Random() < 3
        S_StartSound( actor, actor:info:activesound )
    ENDIF
RETURN NIL

FUNCTION A_FaceTarget( actor )
    IF actor:target == NIL
        RETURN NIL
    ENDIF

    actor:flags := ( actor:flags & ( MF_AMBUSH ^^ 0xFFFFFFFF ) )

    actor:angle := R_PointToAngle2( actor:x, actor:y, actor:target:x, actor:target:y )

    IF ( actor:target:flags & MF_SHADOW ) != 0
        actor:angle += ( P_Random() - P_Random() ) * 2097152
    ENDIF
RETURN NIL

FUNCTION A_PosAttack( actor )
    LOCAL angle
    LOCAL damage
    LOCAL slope

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    A_FaceTarget( actor )
    angle := actor:angle
    slope := P_AimLineAttack( actor, angle, MISSILERANGE )

    S_StartSound( actor, sfx_pistol )
    angle += ( P_Random() - P_Random() ) * 1048576
    damage := ( ( P_Random() % 5 ) + 1 ) * 3
    P_LineAttack( actor, angle, MISSILERANGE, slope, damage )
RETURN NIL

FUNCTION A_SPosAttack( actor )
    LOCAL i
    LOCAL angle
    LOCAL bangle
    LOCAL damage
    LOCAL slope

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    S_StartSound( actor, sfx_shotgn )
    A_FaceTarget( actor )
    bangle := actor:angle
    slope := P_AimLineAttack( actor, bangle, MISSILERANGE )

    FOR i := 0 TO 2
        angle := bangle + ( ( P_Random() - P_Random() ) * 1048576 )
        damage := ( ( P_Random() % 5 ) + 1 ) * 3
        P_LineAttack( actor, angle, MISSILERANGE, slope, damage )
    NEXT
RETURN NIL

FUNCTION A_CPosAttack( actor )
    LOCAL angle
    LOCAL bangle
    LOCAL damage
    LOCAL slope

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    S_StartSound( actor, sfx_shotgn )
    A_FaceTarget( actor )
    bangle := actor:angle
    slope := P_AimLineAttack( actor, bangle, MISSILERANGE )

    angle := bangle + ( ( P_Random() - P_Random() ) * 1048576 )
    damage := ( ( P_Random() % 5 ) + 1 ) * 3
    P_LineAttack( actor, angle, MISSILERANGE, slope, damage )
RETURN NIL

FUNCTION A_CPosRefire( actor )
    A_FaceTarget( actor )

    IF P_Random() < 40
        RETURN NIL
    ENDIF

    IF actor:target == NIL .OR. actor:target:health <= 0 .OR. ! P_CheckSight( actor, actor:target )
        P_SetMobjState( actor, actor:info:seestate )
    ENDIF
RETURN NIL

FUNCTION A_SpidRefire( actor )
    A_FaceTarget( actor )

    IF P_Random() < 10
        RETURN NIL
    ENDIF

    IF actor:target == NIL .OR. actor:target:health <= 0 .OR. ! P_CheckSight( actor, actor:target )
        P_SetMobjState( actor, actor:info:seestate )
    ENDIF
RETURN NIL

FUNCTION A_BspiAttack( actor )
    IF actor:target == NIL
        RETURN NIL
    ENDIF
    A_FaceTarget( actor )
    P_SpawnMissile( actor, actor:target, MT_ARACHPLAZ )
RETURN NIL

FUNCTION A_TroopAttack( actor )
    LOCAL damage

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    A_FaceTarget( actor )
    IF P_CheckMeleeRange( actor )
        S_StartSound( actor, sfx_claw )
        damage := ( ( P_Random() % 8 ) + 1 ) * 3
        P_DamageMobj( actor:target, actor, actor, damage )
        RETURN NIL
    ENDIF

    P_SpawnMissile( actor, actor:target, MT_TROOPSHOT )
RETURN NIL

FUNCTION A_SargAttack( actor )
    LOCAL damage

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    A_FaceTarget( actor )
    IF P_CheckMeleeRange( actor )
        damage := ( ( P_Random() % 10 ) + 1 ) * 4
        P_DamageMobj( actor:target, actor, actor, damage )
    ENDIF
RETURN NIL

FUNCTION A_HeadAttack( actor )
    LOCAL damage

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    A_FaceTarget( actor )
    IF P_CheckMeleeRange( actor )
        damage := ( ( P_Random() % 6 ) + 1 ) * 10
        P_DamageMobj( actor:target, actor, actor, damage )
        RETURN NIL
    ENDIF

    P_SpawnMissile( actor, actor:target, MT_HEADSHOT )
RETURN NIL

FUNCTION A_CyberAttack( actor )
    IF actor:target == NIL
        RETURN NIL
    ENDIF
    A_FaceTarget( actor )
    P_SpawnMissile( actor, actor:target, MT_ROCKET )
RETURN NIL

FUNCTION A_BruisAttack( actor )
    LOCAL damage

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    IF P_CheckMeleeRange( actor )
        S_StartSound( actor, sfx_claw )
        damage := ( ( P_Random() % 8 ) + 1 ) * 10
        P_DamageMobj( actor:target, actor, actor, damage )
        RETURN NIL
    ENDIF

    P_SpawnMissile( actor, actor:target, MT_BRUISERSHOT )
RETURN NIL

FUNCTION A_SkelMissile( actor )
    LOCAL mo

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    A_FaceTarget( actor )
    actor:z += 16 * FRACUNIT
    mo := P_SpawnMissile( actor, actor:target, MT_TRACER )
    actor:z -= 16 * FRACUNIT

    mo:x += mo:momx
    mo:y += mo:momy
    mo:tracer := actor:target
RETURN NIL

FUNCTION A_Tracer( actor )
    LOCAL exact
    LOCAL dist
    LOCAL slope
    LOCAL dest
    LOCAL th
    LOCAL nDiff
    MEMVAR gametic

    IF ( gametic & 3 ) != 0
        RETURN NIL
    ENDIF

    P_SpawnPuff( actor:x, actor:y, actor:z )

    th := P_SpawnMobj( actor:x - actor:momx, actor:y - actor:momy, actor:z, MT_SMOKE )

    th:momz := FRACUNIT
    th:tics -= ( P_Random() & 3 )
    IF th:tics < 1
        th:tics := 1
    ENDIF

    dest := actor:tracer

    IF dest == NIL .OR. dest:health <= 0
        RETURN NIL
    ENDIF

    exact := R_PointToAngle2( actor:x, actor:y, dest:x, dest:y )

    IF exact != actor:angle
        nDiff := ( ( exact - actor:angle ) & 0xFFFFFFFF )
        IF nDiff > 0x80000000
            actor:angle -= nTraceAngle
            nDiff := ( ( exact - actor:angle ) & 0xFFFFFFFF )
            IF nDiff < 0x80000000
                actor:angle := exact
            ENDIF
        ELSE
            actor:angle += nTraceAngle
            nDiff := ( ( exact - actor:angle ) & 0xFFFFFFFF )
            IF nDiff > 0x80000000
                actor:angle := exact
            ENDIF
        ENDIF
    ENDIF

    exact := UShr( actor:angle, ANGLETOFINESHIFT )
    actor:momx := FixedMul( actor:info:speed, FineCos( exact ) )
    actor:momy := FixedMul( actor:info:speed, FineSin( exact ) )

    dist := P_AproxDistance( dest:x - actor:x, dest:y - actor:y )
    dist := Int( dist / actor:info:speed )
    IF dist < 1
        dist := 1
    ENDIF
    slope := Int( ( dest:z + 40 * FRACUNIT - actor:z ) / dist )

    IF slope < actor:momz
        actor:momz -= Int( FRACUNIT / 8 )
    ELSE
        actor:momz += Int( FRACUNIT / 8 )
    ENDIF
RETURN NIL

FUNCTION A_SkelWhoosh( actor )
    IF actor:target == NIL
        RETURN NIL
    ENDIF
    A_FaceTarget( actor )
    S_StartSound( actor, sfx_skeswg )
RETURN NIL

FUNCTION A_SkelFist( actor )
    LOCAL damage

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    A_FaceTarget( actor )

    IF P_CheckMeleeRange( actor )
        damage := ( ( P_Random() % 10 ) + 1 ) * 6
        S_StartSound( actor, sfx_skepch )
        P_DamageMobj( actor:target, actor, actor, damage )
    ENDIF
RETURN NIL

FUNCTION PIT_VileCheck( thing )
    LOCAL maxdist
    LOCAL check
    MEMVAR mobjinfo

    IF ( thing:flags & MF_CORPSE ) == 0
        RETURN .T.
    ENDIF

    IF thing:tics != -1
        RETURN .T.
    ENDIF

    IF thing:info:raisestate == S_NULL
        RETURN .T.
    ENDIF

    maxdist := thing:info:radius + mobjinfo[ MT_VILE + 1 ]:radius

    IF Abs( thing:x - viletryx ) > maxdist .OR. Abs( thing:y - viletryy ) > maxdist
        RETURN .T.
    ENDIF

    corpsehit := thing
    corpsehit:momx := 0
    corpsehit:momy := 0
    corpsehit:height := corpsehit:height * 4
    check := P_CheckPosition( corpsehit, corpsehit:x, corpsehit:y )
    corpsehit:height := Int( corpsehit:height / 4 )

    IF ! check
        RETURN .T.
    ENDIF
RETURN .F.

FUNCTION A_VileChase( actor )
    LOCAL xl
    LOCAL xh
    LOCAL yl
    LOCAL yh
    LOCAL bx
    LOCAL by
    LOCAL info
    LOCAL temp
    MEMVAR bmaporgx
    MEMVAR bmaporgy

    IF actor:movedir != DI_NODIR
        viletryx := actor:x + actor:info:speed * xspeed[ actor:movedir + 1 ]
        viletryy := actor:y + actor:info:speed * yspeed[ actor:movedir + 1 ]

        xl := Shar( viletryx - bmaporgx - MAXRADIUS * 2, MAPBLOCKSHIFT )
        xh := Shar( viletryx - bmaporgx + MAXRADIUS * 2, MAPBLOCKSHIFT )
        yl := Shar( viletryy - bmaporgy - MAXRADIUS * 2, MAPBLOCKSHIFT )
        yh := Shar( viletryy - bmaporgy + MAXRADIUS * 2, MAPBLOCKSHIFT )

        vileobj := actor
        FOR bx := xl TO xh
            FOR by := yl TO yh
                IF ! P_BlockThingsIterator( bx, by, {| th | PIT_VileCheck( th ) } )
                    temp := actor:target
                    actor:target := corpsehit
                    A_FaceTarget( actor )
                    actor:target := temp

                    P_SetMobjState( actor, S_VILE_HEAL1 )
                    S_StartSound( corpsehit, sfx_slop )
                    info := corpsehit:info

                    P_SetMobjState( corpsehit, info:raisestate )
                    corpsehit:height := corpsehit:height * 4
                    corpsehit:flags := info:flags
                    corpsehit:health := info:spawnhealth
                    corpsehit:target := NIL
                    RETURN NIL
                ENDIF
            NEXT
        NEXT
    ENDIF

    A_Chase( actor )
RETURN NIL

FUNCTION A_VileStart( actor )
    S_StartSound( actor, sfx_vilatk )
RETURN NIL

FUNCTION A_StartFire( actor )
    S_StartSound( actor, sfx_flamst )
    A_Fire( actor )
RETURN NIL

FUNCTION A_FireCrackle( actor )
    S_StartSound( actor, sfx_flame )
    A_Fire( actor )
RETURN NIL

FUNCTION A_Fire( actor )
    LOCAL dest
    LOCAL target
    LOCAL an

    dest := actor:tracer
    IF dest == NIL
        RETURN NIL
    ENDIF

    target := P_SubstNullMobj( actor:target )

    IF ! P_CheckSight( target, dest )
        RETURN NIL
    ENDIF

    an := UShr( dest:angle, ANGLETOFINESHIFT )

    P_UnsetThingPosition( actor )
    actor:x := dest:x + FixedMul( 24 * FRACUNIT, FineCos( an ) )
    actor:y := dest:y + FixedMul( 24 * FRACUNIT, FineSin( an ) )
    actor:z := dest:z
    P_SetThingPosition( actor )
RETURN NIL

FUNCTION A_VileTarget( actor )
    LOCAL fog

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    A_FaceTarget( actor )

    fog := P_SpawnMobj( actor:target:x, actor:target:x, actor:target:z, MT_FIRE )

    actor:tracer := fog
    fog:target := actor
    fog:tracer := actor:target
    A_Fire( fog )
RETURN NIL

FUNCTION A_VileAttack( actor )
    LOCAL fire
    LOCAL an

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    A_FaceTarget( actor )

    IF ! P_CheckSight( actor, actor:target )
        RETURN NIL
    ENDIF

    S_StartSound( actor, sfx_barexp )
    P_DamageMobj( actor:target, actor, actor, 20 )
    actor:target:momz := Int( 1000 * FRACUNIT / actor:target:info:mass )

    an := UShr( actor:angle, ANGLETOFINESHIFT )

    fire := actor:tracer
    IF fire == NIL
        RETURN NIL
    ENDIF

    fire:x := actor:target:x - FixedMul( 24 * FRACUNIT, FineCos( an ) )
    fire:y := actor:target:y - FixedMul( 24 * FRACUNIT, FineSin( an ) )
    P_RadiusAttack( fire, actor, 70 )
RETURN NIL

FUNCTION A_FatRaise( actor )
    A_FaceTarget( actor )
    S_StartSound( actor, sfx_manatk )
RETURN NIL

FUNCTION A_FatAttack1( actor )
    LOCAL mo
    LOCAL target
    LOCAL an

    A_FaceTarget( actor )

    actor:angle += FATSPREAD
    target := P_SubstNullMobj( actor:target )
    P_SpawnMissile( actor, target, MT_FATSHOT )

    mo := P_SpawnMissile( actor, target, MT_FATSHOT )
    mo:angle += FATSPREAD
    an := UShr( mo:angle, ANGLETOFINESHIFT )
    mo:momx := FixedMul( mo:info:speed, FineCos( an ) )
    mo:momy := FixedMul( mo:info:speed, FineSin( an ) )
RETURN NIL

FUNCTION A_FatAttack2( actor )
    LOCAL mo
    LOCAL target
    LOCAL an

    A_FaceTarget( actor )

    actor:angle -= FATSPREAD
    target := P_SubstNullMobj( actor:target )
    P_SpawnMissile( actor, target, MT_FATSHOT )

    mo := P_SpawnMissile( actor, target, MT_FATSHOT )
    mo:angle -= FATSPREAD * 2
    an := UShr( mo:angle, ANGLETOFINESHIFT )
    mo:momx := FixedMul( mo:info:speed, FineCos( an ) )
    mo:momy := FixedMul( mo:info:speed, FineSin( an ) )
RETURN NIL

FUNCTION A_FatAttack3( actor )
    LOCAL mo
    LOCAL target
    LOCAL an

    A_FaceTarget( actor )

    target := P_SubstNullMobj( actor:target )

    mo := P_SpawnMissile( actor, target, MT_FATSHOT )
    mo:angle -= Int( FATSPREAD / 2 )
    an := UShr( mo:angle, ANGLETOFINESHIFT )
    mo:momx := FixedMul( mo:info:speed, FineCos( an ) )
    mo:momy := FixedMul( mo:info:speed, FineSin( an ) )

    mo := P_SpawnMissile( actor, target, MT_FATSHOT )
    mo:angle += Int( FATSPREAD / 2 )
    an := UShr( mo:angle, ANGLETOFINESHIFT )
    mo:momx := FixedMul( mo:info:speed, FineCos( an ) )
    mo:momy := FixedMul( mo:info:speed, FineSin( an ) )
RETURN NIL

FUNCTION A_SkullAttack( actor )
    LOCAL dest
    LOCAL an
    LOCAL dist

    IF actor:target == NIL
        RETURN NIL
    ENDIF

    dest := actor:target
    actor:flags := ( actor:flags | MF_SKULLFLY )

    S_StartSound( actor, actor:info:attacksound )
    A_FaceTarget( actor )
    an := UShr( actor:angle, ANGLETOFINESHIFT )
    actor:momx := FixedMul( SKULLSPEED, FineCos( an ) )
    actor:momy := FixedMul( SKULLSPEED, FineSin( an ) )
    dist := P_AproxDistance( dest:x - actor:x, dest:y - actor:y )
    dist := Int( dist / SKULLSPEED )
    IF dist < 1
        dist := 1
    ENDIF
    actor:momz := Int( ( dest:z + Shar( dest:height, 1 ) - actor:z ) / dist )
RETURN NIL

FUNCTION A_PainShootSkull( actor, angle )
    LOCAL x
    LOCAL y
    LOCAL z
    LOCAL newmobj
    LOCAL an
    LOCAL prestep
    LOCAL count
    LOCAL currentthinker
    LOCAL mo
    MEMVAR mobjinfo
    MEMVAR thinkercap

    count := 0

    IF thinkercap != NIL
        currentthinker := thinkercap:next
        DO WHILE currentthinker != NIL .AND. !( currentthinker == thinkercap )
            IF IsMobjThinker( currentthinker )
                mo := ThinkerToMobj( currentthinker )
                IF mo != NIL .AND. mo:type == MT_SKULL
                    count := count + 1
                ENDIF
            ENDIF
            currentthinker := currentthinker:next
        ENDDO
    ENDIF

    IF count > 20
        RETURN NIL
    ENDIF

    an := UShr( angle, ANGLETOFINESHIFT )

    prestep := 4 * FRACUNIT + Int( 3 * ( actor:info:radius + mobjinfo[ MT_SKULL + 1 ]:radius ) / 2 )

    x := actor:x + FixedMul( prestep, FineCos( an ) )
    y := actor:y + FixedMul( prestep, FineSin( an ) )
    z := actor:z + 8 * FRACUNIT

    newmobj := P_SpawnMobj( x, y, z, MT_SKULL )

    IF ! P_TryMove( newmobj, newmobj:x, newmobj:y )
        P_DamageMobj( newmobj, actor, actor, 10000 )
        RETURN NIL
    ENDIF

    newmobj:target := actor:target
    A_SkullAttack( newmobj )
RETURN NIL

FUNCTION A_PainAttack( actor )
    IF actor:target == NIL
        RETURN NIL
    ENDIF
    A_FaceTarget( actor )
    A_PainShootSkull( actor, actor:angle )
RETURN NIL

FUNCTION A_PainDie( actor )
    A_Fall( actor )
    A_PainShootSkull( actor, actor:angle + ANG90 )
    A_PainShootSkull( actor, actor:angle + ANG180 )
    A_PainShootSkull( actor, actor:angle + ANG270 )
RETURN NIL

FUNCTION A_Scream( actor )
    LOCAL sound

    SWITCH actor:info:deathsound
    CASE 0
        RETURN NIL

    CASE sfx_podth1
        sound := sfx_podth1 + ( P_Random() % 3 )
        EXIT
    CASE sfx_podth2
        sound := sfx_podth1 + ( P_Random() % 3 )
        EXIT
    CASE sfx_podth3
        sound := sfx_podth1 + ( P_Random() % 3 )
        EXIT

    CASE sfx_bgdth1
        sound := sfx_bgdth1 + ( P_Random() % 2 )
        EXIT
    CASE sfx_bgdth2
        sound := sfx_bgdth1 + ( P_Random() % 2 )
        EXIT

    OTHERWISE
        sound := actor:info:deathsound
        EXIT
    ENDSWITCH

    IF actor:type == MT_SPIDER .OR. actor:type == MT_CYBORG
        S_StartSound( NIL, sound )
    ELSE
        S_StartSound( actor, sound )
    ENDIF
RETURN NIL

FUNCTION A_XScream( actor )
    S_StartSound( actor, sfx_slop )
RETURN NIL

FUNCTION A_Pain( actor )
    IF actor:info:painsound != 0
        S_StartSound( actor, actor:info:painsound )
    ENDIF
RETURN NIL

FUNCTION A_Fall( actor )
    actor:flags := ( actor:flags & ( MF_SOLID ^^ 0xFFFFFFFF ) )
RETURN NIL

FUNCTION A_Explode( thingy )
    P_RadiusAttack( thingy, thingy:target, 128 )
RETURN NIL

STATIC FUNCTION CheckBossEnd( motype )
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gameversion
    IF gameversion < exe_ultimate
        IF gamemap != 8
            RETURN .F.
        ENDIF
        IF motype == MT_BRUISER .AND. gameepisode != 1
            RETURN .F.
        ENDIF
        RETURN .T.
    ENDIF

    SWITCH gameepisode
    CASE 1
        RETURN gamemap == 8 .AND. motype == MT_BRUISER
    CASE 2
        RETURN gamemap == 8 .AND. motype == MT_CYBORG
    CASE 3
        RETURN gamemap == 8 .AND. motype == MT_SPIDER
    CASE 4
        RETURN ( gamemap == 6 .AND. motype == MT_CYBORG ) .OR. ( gamemap == 8 .AND. motype == MT_SPIDER )
    OTHERWISE
        RETURN gamemap == 8
    ENDSWITCH
RETURN .F.

FUNCTION A_BossDeath( mo )
    LOCAL th
    LOCAL mo2
    LOCAL junk
    LOCAL i
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gamemode
    MEMVAR playeringame
    MEMVAR players
    MEMVAR thinkercap

    IF gamemode == commercial
        IF gamemap != 7
            RETURN NIL
        ENDIF
        IF mo:type != MT_FATSO .AND. mo:type != MT_BABY
            RETURN NIL
        ENDIF
    ELSE
        IF ! CheckBossEnd( mo:type )
            RETURN NIL
        ENDIF
    ENDIF

    i := 0
    DO WHILE i < MAXPLAYERS
        IF playeringame[ i + 1 ] .AND. players[ i + 1 ]:health > 0
            EXIT
        ENDIF
        i := i + 1
    ENDDO

    IF i == MAXPLAYERS
        RETURN NIL
    ENDIF

    IF thinkercap != NIL
        th := thinkercap:next
        DO WHILE th != NIL .AND. !( th == thinkercap )
            IF IsMobjThinker( th )
                mo2 := ThinkerToMobj( th )
                IF mo2 != NIL .AND. !( mo2 == mo ) .AND. mo2:type == mo:type .AND. mo2:health > 0
                    RETURN NIL
                ENDIF
            ENDIF
            th := th:next
        ENDDO
    ENDIF

    junk := p_tagline_t():New()

    IF gamemode == commercial
        IF gamemap == 7
            IF mo:type == MT_FATSO
                junk:tag := 666
                EV_DoFloor( junk, lowerFloorToLowest )
                RETURN NIL
            ENDIF
            IF mo:type == MT_BABY
                junk:tag := 667
                EV_DoFloor( junk, raiseToTexture )
                RETURN NIL
            ENDIF
        ENDIF
    ELSE
        SWITCH gameepisode
        CASE 1
            junk:tag := 666
            EV_DoFloor( junk, lowerFloorToLowest )
            RETURN NIL
        CASE 4
            SWITCH gamemap
            CASE 6
                junk:tag := 666
                EV_DoDoor( junk, vld_blazeOpen )
                RETURN NIL
            CASE 8
                junk:tag := 666
                EV_DoFloor( junk, lowerFloorToLowest )
                RETURN NIL
            ENDSWITCH
            EXIT
        ENDSWITCH
    ENDIF

    G_ExitLevel()
RETURN NIL

FUNCTION A_Hoof( mo )
    S_StartSound( mo, sfx_hoof )
    A_Chase( mo )
RETURN NIL

FUNCTION A_Metal( mo )
    S_StartSound( mo, sfx_metal )
    A_Chase( mo )
RETURN NIL

FUNCTION A_BabyMetal( mo )
    S_StartSound( mo, sfx_bspwlk )
    A_Chase( mo )
RETURN NIL

FUNCTION A_OpenShotgun2( player, psp )
    HB_SYMBOL_UNUSED( psp )
    S_StartSound( player:mo, sfx_dbopn )
RETURN NIL

FUNCTION A_LoadShotgun2( player, psp )
    HB_SYMBOL_UNUSED( psp )
    S_StartSound( player:mo, sfx_dbload )
RETURN NIL

FUNCTION A_CloseShotgun2( player, psp )
    S_StartSound( player:mo, sfx_dbcls )
    A_ReFire( player, psp )
RETURN NIL

FUNCTION A_BrainAwake( mo )
    LOCAL thinker
    LOCAL m
    MEMVAR thinkercap

    HB_SYMBOL_UNUSED( mo )

    numbraintargets := 0
    braintargeton := 0

    IF thinkercap != NIL
        thinker := thinkercap:next
        DO WHILE thinker != NIL .AND. !( thinker == thinkercap )
            IF IsMobjThinker( thinker )
                m := ThinkerToMobj( thinker )
                IF m != NIL .AND. m:type == MT_BOSSTARGET
                    braintargets[ numbraintargets + 1 ] := m
                    numbraintargets := numbraintargets + 1
                ENDIF
            ENDIF
            thinker := thinker:next
        ENDDO
    ENDIF

    S_StartSound( NIL, sfx_bossit )
RETURN NIL

FUNCTION A_BrainPain( mo )
    HB_SYMBOL_UNUSED( mo )
    S_StartSound( NIL, sfx_bospn )
RETURN NIL

FUNCTION A_BrainScream( mo )
    LOCAL x
    LOCAL y
    LOCAL z
    LOCAL th

    x := mo:x - 196 * FRACUNIT
    DO WHILE x < mo:x + 320 * FRACUNIT
        y := mo:y - 320 * FRACUNIT
        z := 128 + P_Random() * 2 * FRACUNIT
        th := P_SpawnMobj( x, y, z, MT_ROCKET )
        th:momz := P_Random() * 512
        P_SetMobjState( th, S_BRAINEXPLODE1 )
        th:tics -= ( P_Random() & 7 )
        IF th:tics < 1
            th:tics := 1
        ENDIF
        x += FRACUNIT * 8
    ENDDO

    S_StartSound( NIL, sfx_bosdth )
RETURN NIL

FUNCTION A_BrainExplode( mo )
    LOCAL x
    LOCAL y
    LOCAL z
    LOCAL th

    x := mo:x + ( P_Random() - P_Random() ) * 2048
    y := mo:y
    z := 128 + P_Random() * 2 * FRACUNIT
    th := P_SpawnMobj( x, y, z, MT_ROCKET )
    th:momz := P_Random() * 512
    P_SetMobjState( th, S_BRAINEXPLODE1 )
    th:tics -= ( P_Random() & 7 )
    IF th:tics < 1
        th:tics := 1
    ENDIF
RETURN NIL

FUNCTION A_BrainDie( mo )
    HB_SYMBOL_UNUSED( mo )
    G_ExitLevel()
RETURN NIL

FUNCTION A_BrainSpit( mo )
    LOCAL targ
    LOCAL newmobj
    MEMVAR gameskill

    easy := ( easy ^^ 1 )
    IF gameskill <= sk_easy .AND. easy == 0
        RETURN NIL
    ENDIF

    targ := braintargets[ braintargeton + 1 ]
    braintargeton := ( braintargeton + 1 ) % numbraintargets

    newmobj := P_SpawnMissile( mo, targ, MT_SPAWNSHOT )
    newmobj:target := targ
    newmobj:reactiontime := Int( Int( ( targ:y - mo:y ) / newmobj:momy ) / newmobj:state:tics )

    S_StartSound( NIL, sfx_bospit )
RETURN NIL

FUNCTION A_SpawnSound( mo )
    S_StartSound( mo, sfx_boscub )
    A_SpawnFly( mo )
RETURN NIL

FUNCTION A_SpawnFly( mo )
    LOCAL newmobj
    LOCAL fog
    LOCAL targ
    LOCAL r
    LOCAL type

    mo:reactiontime := mo:reactiontime - 1
    IF mo:reactiontime != 0
        RETURN NIL
    ENDIF

    targ := P_SubstNullMobj( mo:target )

    fog := P_SpawnMobj( targ:x, targ:y, targ:z, MT_SPAWNFIRE )
    S_StartSound( fog, sfx_telept )

    r := P_Random()

    IF r < 50
        type := MT_TROOP
    ELSEIF r < 90
        type := MT_SERGEANT
    ELSEIF r < 120
        type := MT_SHADOWS
    ELSEIF r < 130
        type := MT_PAIN
    ELSEIF r < 160
        type := MT_HEAD
    ELSEIF r < 162
        type := MT_VILE
    ELSEIF r < 172
        type := MT_UNDEAD
    ELSEIF r < 192
        type := MT_BABY
    ELSEIF r < 222
        type := MT_FATSO
    ELSEIF r < 246
        type := MT_KNIGHT
    ELSE
        type := MT_BRUISER
    ENDIF

    newmobj := P_SpawnMobj( targ:x, targ:y, targ:z, type )
    IF P_LookForPlayers( newmobj, .T. )
        P_SetMobjState( newmobj, newmobj:info:seestate )
    ENDIF

    P_TeleportMove( newmobj, newmobj:x, newmobj:y )
    P_RemoveMobj( mo )
RETURN NIL

FUNCTION A_PlayerScream( mo )
    LOCAL sound
    MEMVAR gamemode

    sound := sfx_pldeth

    IF gamemode == commercial .AND. mo:health < -50
        sound := sfx_pdiehi
    ENDIF

    S_StartSound( mo, sound )
RETURN NIL
