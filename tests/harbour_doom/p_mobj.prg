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

STATIC dummy_mobj
STATIC itemrespawnque
STATIC itemrespawntime

#include "p_mobj.ch"

CLASS mobj_t
    DATA thinker
    DATA thinkfn
    DATA x
    DATA y
    DATA z
    DATA snext
    DATA sprev
    DATA angle
    DATA sprite
    DATA frame
    DATA bnext
    DATA bprev
    DATA subsector
    DATA floorz
    DATA ceilingz
    DATA radius
    DATA height
    DATA momx
    DATA momy
    DATA momz
    DATA validcount
    DATA type
    DATA info
    DATA tics
    DATA state
    DATA iState
    DATA flags
    DATA health
    DATA movedir
    DATA movecount
    DATA target
    DATA reactiontime
    DATA threshold
    DATA player
    DATA lastlook
    DATA spawnpoint
    DATA tracer
    METHOD New()
ENDCLASS
#include "p_local.ch"
#include "p_pspr.ch"
#include "doomstat.ch"
#include "doomdef.ch"
#include "info.ch"


METHOD New() CLASS mobj_t
    ::thinker      := thinker_t():New()
    ::thinkfn      := ""
    ::x            := 0
    ::y            := 0
    ::z            := 0
    ::snext        := NIL
    ::sprev        := NIL
    ::angle        := 0
    ::sprite       := 0
    ::frame        := 0
    ::bnext        := NIL
    ::bprev        := NIL
    ::subsector    := NIL
    ::floorz       := 0
    ::ceilingz     := 0
    ::radius       := 0
    ::height       := 0
    ::momx         := 0
    ::momy         := 0
    ::momz         := 0
    ::validcount   := 0
    ::type         := 0
    ::info         := NIL
    ::tics         := 0
    ::state        := NIL
    ::iState       := 0
    ::flags        := 0
    ::health       := 0
    ::movedir      := 0
    ::movecount    := 0
    ::target       := NIL
    ::reactiontime := 0
    ::threshold    := 0
    ::player       := NIL
    ::lastlook     := 0
    ::spawnpoint   := mapthing_t():New()
    ::tracer       := NIL
RETURN Self

INIT PROCEDURE init_p_mobj
    LOCAL i

    PUBLIC iquehead
    PUBLIC iquetail

    itemrespawnque := {}
    itemrespawntime := {}
    FOR i := 1 TO ITEMQUESIZE
        AAdd( itemrespawnque, mapthing_t():New() )
        AAdd( itemrespawntime, 0 )
    NEXT

    iquehead := 0
    iquetail := 0
    dummy_mobj := mobj_t():New()
RETURN

STATIC FUNCTION IfaceCall( xFun, x1, x2 )
    LOCAL nArgs := PCount() - 1

    IF xFun == NIL
        RETURN NIL
    ENDIF

    IF ValType( xFun ) == "B"
        IF nArgs <= 0
            RETURN Eval( xFun )
        ELSEIF nArgs == 1
            RETURN Eval( xFun, x1 )
        ENDIF
        RETURN Eval( xFun, x1, x2 )
    ENDIF

    IF ValType( xFun ) == "C"
        IF nArgs <= 0
            RETURN &( xFun )()
        ELSEIF nArgs == 1
            RETURN &( xFun )( x1 )
        ENDIF
        RETURN &( xFun )( x1, x2 )
    ENDIF
RETURN NIL

STATIC PROCEDURE BindPMobjThinker( mobj )
    LOCAL o := mobj

    mobj:thinkfn := "P_MobjThinker"
    mobj:thinker:thinkfn := "P_MobjThinker"
    mobj:thinker:owner := mobj
    mobj:thinker:function:acp1 := {|| P_MobjThinker( o ) }
RETURN

STATIC PROCEDURE AssignMapThing( dst, src )
    IF dst == NIL .OR. src == NIL
        RETURN
    ENDIF
    dst:x       := src:x
    dst:y       := src:y
    dst:angle   := src:angle
    dst:type    := src:type
    dst:options := src:options
RETURN

STATIC FUNCTION ThinkerRemoved( mobj )
    IF mobj != NIL .AND. mobj:thinker != NIL .AND. mobj:thinker:function != NIL
        IF ValType( mobj:thinker:function:acv ) == "N" .AND. mobj:thinker:function:acv == -1
            RETURN .T.
        ENDIF
    ENDIF
RETURN .F.

FUNCTION P_SetMobjState( mobj, state )
    LOCAL st
    LOCAL xAct
    MEMVAR states

    DO WHILE .T.
        IF state == S_NULL
            mobj:state := NIL
            mobj:iState := S_NULL
            P_RemoveMobj( mobj )
            RETURN .F.
        ENDIF

        st := states[ state + 1 ]
        mobj:state := st
        mobj:iState := state
        mobj:tics := st:tics
        mobj:sprite := st:sprite
        mobj:frame := st:frame

        IF st:action != NIL
            xAct := st:action:acp1
            IF xAct == NIL
                xAct := st:action:acv
            ENDIF
            IfaceCall( xAct, mobj )
        ENDIF

        state := st:nextstate
        IF mobj:tics != 0
            EXIT
        ENDIF
    ENDDO
RETURN .T.

FUNCTION P_ExplodeMissile( mo )
    MEMVAR mobjinfo
    mo:momx := 0
    mo:momy := 0
    mo:momz := 0

    P_SetMobjState( mo, mobjinfo[ mo:type + 1 ]:deathstate )

    mo:tics -= ( P_Random() & 3 )

    IF mo:tics < 1
        mo:tics := 1
    ENDIF

    mo:flags := ( mo:flags & ( MF_MISSILE ^^ 0xFFFFFFFF ) )

    IF mo:info != NIL .AND. mo:info:deathsound != 0
        S_StartSound( mo, mo:info:deathsound )
    ENDIF
RETURN NIL

FUNCTION P_XYMovement( mo )
    LOCAL ptryx
    LOCAL ptryy
    LOCAL player
    LOCAL xmove
    LOCAL ymove
    LOCAL n
    MEMVAR ceilingline
    MEMVAR skyflatnum

    IF mo:momx == 0 .AND. mo:momy == 0
        IF ( mo:flags & MF_SKULLFLY ) != 0
            mo:flags := ( mo:flags & ( MF_SKULLFLY ^^ 0xFFFFFFFF ) )
            mo:momx := 0
            mo:momy := 0
            mo:momz := 0
            P_SetMobjState( mo, mo:info:spawnstate )
        ENDIF
        RETURN NIL
    ENDIF

    player := mo:player

    IF mo:momx > MAXMOVE
        mo:momx := MAXMOVE
    ELSEIF mo:momx < -MAXMOVE
        mo:momx := -MAXMOVE
    ENDIF

    IF mo:momy > MAXMOVE
        mo:momy := MAXMOVE
    ELSEIF mo:momy < -MAXMOVE
        mo:momy := -MAXMOVE
    ENDIF

    xmove := mo:momx
    ymove := mo:momy

    DO WHILE xmove != 0 .OR. ymove != 0
        IF xmove > Int( MAXMOVE / 2 ) .OR. ymove > Int( MAXMOVE / 2 )
            ptryx := mo:x + Int( xmove / 2 )
            ptryy := mo:y + Int( ymove / 2 )
            xmove := Int( xmove / 2 )
            ymove := Int( ymove / 2 )
        ELSE
            ptryx := mo:x + xmove
            ptryy := mo:y + ymove
            xmove := 0
            ymove := 0
        ENDIF

        IF ! P_TryMove( mo, ptryx, ptryy )
            IF mo:player != NIL
                P_SlideMove( mo )
            ELSEIF ( mo:flags & MF_MISSILE ) != 0
                IF ceilingline != NIL ;
                   .AND. ceilingline:backsector != NIL ;
                   .AND. ceilingline:backsector:ceilingpic == skyflatnum
                    P_RemoveMobj( mo )
                    RETURN NIL
                ENDIF
                P_ExplodeMissile( mo )
            ELSE
                mo:momx := 0
                mo:momy := 0
            ENDIF
        ENDIF
    ENDDO

    IF player != NIL .AND. ( player:cheats & CF_NOMOMENTUM ) != 0
        mo:momx := 0
        mo:momy := 0
        RETURN NIL
    ENDIF

    IF ( mo:flags & ( MF_MISSILE | MF_SKULLFLY ) ) != 0
        RETURN NIL
    ENDIF

    IF mo:z > mo:floorz
        RETURN NIL
    ENDIF

    IF ( mo:flags & MF_CORPSE ) != 0
        IF mo:momx > Int( FRACUNIT / 4 ) ;
           .OR. mo:momx < -Int( FRACUNIT / 4 ) ;
           .OR. mo:momy > Int( FRACUNIT / 4 ) ;
           .OR. mo:momy < -Int( FRACUNIT / 4 )
            IF mo:floorz != mo:subsector:sector:floorheight
                RETURN NIL
            ENDIF
        ENDIF
    ENDIF

    IF mo:momx > -STOPSPEED ;
       .AND. mo:momx < STOPSPEED ;
       .AND. mo:momy > -STOPSPEED ;
       .AND. mo:momy < STOPSPEED ;
       .AND. ( player == NIL ;
               .OR. ( player:cmd:forwardmove == 0 .AND. player:cmd:sidemove == 0 ) )
        IF player != NIL
            n := player:mo:iState - S_PLAY_RUN1
            IF n >= 0 .AND. n < 4
                P_SetMobjState( player:mo, S_PLAY )
            ENDIF
        ENDIF
        mo:momx := 0
        mo:momy := 0
    ELSE
        mo:momx := FixedMul( mo:momx, FRICTION )
        mo:momy := FixedMul( mo:momy, FRICTION )
    ENDIF
RETURN NIL

FUNCTION P_ZMovement( mo )
    LOCAL dist
    LOCAL delta
    LOCAL correct_lost_soul_bounce
    MEMVAR gameversion

    IF mo:player != NIL .AND. mo:z < mo:floorz
        mo:player:viewheight -= ( mo:floorz - mo:z )
        mo:player:deltaviewheight := Shar( VIEWHEIGHT - mo:player:viewheight, 3 )
    ENDIF

    mo:z += mo:momz

    IF ( mo:flags & MF_FLOAT ) != 0 .AND. mo:target != NIL
        IF ( mo:flags & MF_SKULLFLY ) == 0 .AND. ( mo:flags & MF_INFLOAT ) == 0
            dist := P_AproxDistance( mo:x - mo:target:x, mo:y - mo:target:y )
            delta := ( mo:target:z + Shar( mo:height, 1 ) ) - mo:z

            IF delta < 0 .AND. dist < -( delta * 3 )
                mo:z -= FLOATSPEED
            ELSEIF delta > 0 .AND. dist < ( delta * 3 )
                mo:z += FLOATSPEED
            ENDIF
        ENDIF
    ENDIF

    IF mo:z <= mo:floorz
        correct_lost_soul_bounce := ( gameversion >= exe_ultimate )

        IF correct_lost_soul_bounce .AND. ( mo:flags & MF_SKULLFLY ) != 0
            mo:momz := -mo:momz
        ENDIF

        IF mo:momz < 0
            IF mo:player != NIL .AND. mo:momz < -GRAVITY * 8
                mo:player:deltaviewheight := Shar( mo:momz, 3 )
                S_StartSound( mo, sfx_oof )
            ENDIF
            mo:momz := 0
        ENDIF
        mo:z := mo:floorz

        IF ! correct_lost_soul_bounce .AND. ( mo:flags & MF_SKULLFLY ) != 0
            mo:momz := -mo:momz
        ENDIF

        IF ( mo:flags & MF_MISSILE ) != 0 .AND. ( mo:flags & MF_NOCLIP ) == 0
            P_ExplodeMissile( mo )
            RETURN NIL
        ENDIF
    ELSEIF ( mo:flags & MF_NOGRAVITY ) == 0
        IF mo:momz == 0
            mo:momz := -GRAVITY * 2
        ELSE
            mo:momz -= GRAVITY
        ENDIF
    ENDIF

    IF mo:z + mo:height > mo:ceilingz
        IF mo:momz > 0
            mo:momz := 0
        ENDIF
        mo:z := mo:ceilingz - mo:height

        IF ( mo:flags & MF_SKULLFLY ) != 0
            mo:momz := -mo:momz
        ENDIF

        IF ( mo:flags & MF_MISSILE ) != 0 .AND. ( mo:flags & MF_NOCLIP ) == 0
            P_ExplodeMissile( mo )
            RETURN NIL
        ENDIF
    ENDIF
RETURN NIL

FUNCTION P_NightmareRespawn( mobj )
    LOCAL x
    LOCAL y
    LOCAL z
    LOCAL ss
    LOCAL mo
    LOCAL mthing

    x := mobj:spawnpoint:x * FRACUNIT
    y := mobj:spawnpoint:y * FRACUNIT

    IF ! P_CheckPosition( mobj, x, y )
        RETURN NIL
    ENDIF

    mo := P_SpawnMobj( mobj:x, mobj:y, mobj:subsector:sector:floorheight, MT_TFOG )
    S_StartSound( mo, sfx_telept )

    ss := R_PointInSubsector( x, y )
    mo := P_SpawnMobj( x, y, ss:sector:floorheight, MT_TFOG )
    S_StartSound( mo, sfx_telept )

    mthing := mobj:spawnpoint

    IF ( mobj:info:flags & MF_SPAWNCEILING ) != 0
        z := ONCEILINGZ
    ELSE
        z := ONFLOORZ
    ENDIF

    mo := P_SpawnMobj( x, y, z, mobj:type )
    AssignMapThing( mo:spawnpoint, mobj:spawnpoint )
    mo:angle := ANG45 * Int( mthing:angle / 45 )

    IF ( mthing:options & MTF_AMBUSH ) != 0
        mo:flags := ( mo:flags | MF_AMBUSH )
    ENDIF

    mo:reactiontime := 18
    P_RemoveMobj( mobj )
RETURN NIL

FUNCTION P_MobjThinker( mobj )
    MEMVAR leveltime
    MEMVAR respawnmonsters
    IF mobj:momx != 0 .OR. mobj:momy != 0 .OR. ( mobj:flags & MF_SKULLFLY ) != 0
        P_XYMovement( mobj )
        IF ThinkerRemoved( mobj )
            RETURN NIL
        ENDIF
    ENDIF

    IF mobj:z != mobj:floorz .OR. mobj:momz != 0
        P_ZMovement( mobj )
        IF ThinkerRemoved( mobj )
            RETURN NIL
        ENDIF
    ENDIF

    IF mobj:tics != -1
        mobj:tics--
        IF mobj:tics == 0
            IF mobj:state == NIL
                RETURN NIL
            ENDIF
            IF ! P_SetMobjState( mobj, mobj:state:nextstate )
                RETURN NIL
            ENDIF
        ENDIF
    ELSE
        IF ( mobj:flags & MF_COUNTKILL ) == 0
            RETURN NIL
        ENDIF
        IF ! respawnmonsters
            RETURN NIL
        ENDIF

        mobj:movecount++

        IF mobj:movecount < 12 * TICRATE
            RETURN NIL
        ENDIF

        IF ( leveltime & 31 ) != 0
            RETURN NIL
        ENDIF

        IF P_Random() > 4
            RETURN NIL
        ENDIF

        P_NightmareRespawn( mobj )
    ENDIF
RETURN NIL

FUNCTION P_SpawnMobj( x, y, z, type )
    LOCAL mobj
    LOCAL st
    LOCAL info
    MEMVAR gameskill
    MEMVAR mobjinfo
    MEMVAR states

    mobj := mobj_t():New()
    info := mobjinfo[ type + 1 ]

    mobj:type := type
    mobj:info := info
    mobj:x := x
    mobj:y := y
    mobj:radius := info:radius
    mobj:height := info:height
    mobj:flags := info:flags
    mobj:health := info:spawnhealth

    IF gameskill != sk_nightmare
        mobj:reactiontime := info:reactiontime
    ENDIF

    mobj:lastlook := P_Random() % MAXPLAYERS

    st := states[ info:spawnstate + 1 ]
    mobj:state := st
    mobj:iState := info:spawnstate
    mobj:tics := st:tics
    mobj:sprite := st:sprite
    mobj:frame := st:frame

    P_SetThingPosition( mobj )

    mobj:floorz := mobj:subsector:sector:floorheight
    mobj:ceilingz := mobj:subsector:sector:ceilingheight

    IF z == ONFLOORZ
        mobj:z := mobj:floorz
    ELSEIF z == ONCEILINGZ
        mobj:z := mobj:ceilingz - mobj:info:height
    ELSE
        mobj:z := z
    ENDIF

    BindPMobjThinker( mobj )
    P_AddThinker( mobj:thinker )
RETURN mobj

FUNCTION P_RemoveMobj( mobj )
    MEMVAR iquehead
    MEMVAR iquetail
    MEMVAR leveltime
    IF ( mobj:flags & MF_SPECIAL ) != 0 ;
       .AND. ( mobj:flags & MF_DROPPED ) == 0 ;
       .AND. mobj:type != MT_INV ;
       .AND. mobj:type != MT_INS
        AssignMapThing( itemrespawnque[ iquehead + 1 ], mobj:spawnpoint )
        itemrespawntime[ iquehead + 1 ] := leveltime
        iquehead := ( ( iquehead + 1 ) & ( ITEMQUESIZE - 1 ) )

        IF iquehead == iquetail
            iquetail := ( ( iquetail + 1 ) & ( ITEMQUESIZE - 1 ) )
        ENDIF
    ENDIF

    P_UnsetThingPosition( mobj )
    S_StopSound( mobj )
    P_RemoveThinker( mobj:thinker )
    IF mobj:thinker != NIL .AND. mobj:thinker:function != NIL
        mobj:thinker:function:acv := -1
    ENDIF
RETURN NIL

FUNCTION P_RespawnSpecials()
    LOCAL x
    LOCAL y
    LOCAL z
    LOCAL ss
    LOCAL mo
    LOCAL mthing
    LOCAL i
    MEMVAR deathmatch
    MEMVAR iquehead
    MEMVAR iquetail
    MEMVAR leveltime
    MEMVAR mobjinfo

    IF deathmatch != 2
        RETURN NIL
    ENDIF

    IF iquehead == iquetail
        RETURN NIL
    ENDIF

    IF leveltime - itemrespawntime[ iquetail + 1 ] < 30 * TICRATE
        RETURN NIL
    ENDIF

    mthing := itemrespawnque[ iquetail + 1 ]

    x := mthing:x * FRACUNIT
    y := mthing:y * FRACUNIT

    ss := R_PointInSubsector( x, y )
    mo := P_SpawnMobj( x, y, ss:sector:floorheight, MT_IFOG )
    S_StartSound( mo, sfx_itmbk )

    FOR i := 0 TO NUMMOBJTYPES - 1
        IF mthing:type == mobjinfo[ i + 1 ]:doomednum
            EXIT
        ENDIF
    NEXT

    IF ( mobjinfo[ i + 1 ]:flags & MF_SPAWNCEILING ) != 0
        z := ONCEILINGZ
    ELSE
        z := ONFLOORZ
    ENDIF

    mo := P_SpawnMobj( x, y, z, i )
    AssignMapThing( mo:spawnpoint, mthing )
    mo:angle := ANG45 * Int( mthing:angle / 45 )

    iquetail := ( ( iquetail + 1 ) & ( ITEMQUESIZE - 1 ) )
RETURN NIL

FUNCTION P_SpawnPlayer( mthing )
    LOCAL p
    LOCAL x
    LOCAL y
    LOCAL z
    LOCAL mobj
    LOCAL i
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR playeringame
    MEMVAR players

    IF mthing:type == 0
        RETURN NIL
    ENDIF

    IF ! playeringame[ mthing:type ]
        RETURN NIL
    ENDIF

    p := players[ mthing:type ]

    IF p:playerstate == PST_REBORN
        G_PlayerReborn( mthing:type - 1 )
        p := players[ mthing:type ]
    ENDIF

    x := mthing:x * FRACUNIT
    y := mthing:y * FRACUNIT
    z := ONFLOORZ
    mobj := P_SpawnMobj( x, y, z, MT_PLAYER )

    IF mthing:type > 1
        mobj:flags := ( mobj:flags | ( ( mthing:type - 1 ) * 67108864 ) )
    ENDIF

    mobj:angle := ANG45 * Int( mthing:angle / 45 )
    mobj:player := p
    mobj:health := p:health

    p:mo := mobj
    p:playerstate := PST_LIVE
    p:refire := 0
    p:message := NIL
    p:damagecount := 0
    p:bonuscount := 0
    p:extralight := 0
    p:fixedcolormap := 0
    p:viewheight := VIEWHEIGHT

    P_SetupPsprites( p )

    IF deathmatch != 0
        FOR i := 0 TO NUMCARDS - 1
            p:cards[ i + 1 ] := .T.
        NEXT
    ENDIF

    IF mthing:type - 1 == consoleplayer
        ST_Start()
        HU_Start()
    ENDIF
RETURN NIL

FUNCTION P_SpawnMapThing( mthing )
    LOCAL i
    LOCAL bit
    LOCAL mobj
    LOCAL x
    LOCAL y
    LOCAL z
    MEMVAR deathmatch
    MEMVAR deathmatch_p
    MEMVAR deathmatchstarts
    MEMVAR gameskill
    MEMVAR mobjinfo
    MEMVAR netgame
    MEMVAR nomonsters
    MEMVAR playerstarts
    MEMVAR totalitems
    MEMVAR totalkills

    IF mthing:type == 11
        IF deathmatch_p < 10
            AssignMapThing( deathmatchstarts[ deathmatch_p + 1 ], mthing )
            deathmatch_p := deathmatch_p + 1
        ENDIF
        RETURN NIL
    ENDIF

    IF mthing:type <= 0
        RETURN NIL
    ENDIF

    IF mthing:type <= 4
        AssignMapThing( playerstarts[ mthing:type ], mthing )
        IF deathmatch == 0
            P_SpawnPlayer( mthing )
        ENDIF
        RETURN NIL
    ENDIF

    IF ! netgame .AND. ( mthing:options & 16 ) != 0
        RETURN NIL
    ENDIF

    IF gameskill == sk_baby
        bit := 1
    ELSEIF gameskill == sk_nightmare
        bit := 4
    ELSE
        bit := Int( 2 ^ ( gameskill - 1 ) )
    ENDIF

    IF ( mthing:options & bit ) == 0
        RETURN NIL
    ENDIF

    FOR i := 0 TO NUMMOBJTYPES - 1
        IF mthing:type == mobjinfo[ i + 1 ]:doomednum
            EXIT
        ENDIF
    NEXT

    IF i == NUMMOBJTYPES
        I_Error( "P_SpawnMapThing: Unknown type " + hb_ntos( mthing:type ) + ;
                 " at (" + hb_ntos( mthing:x ) + ", " + hb_ntos( mthing:y ) + ")" )
    ENDIF

    IF deathmatch != 0 .AND. ( mobjinfo[ i + 1 ]:flags & MF_NOTDMATCH ) != 0
        RETURN NIL
    ENDIF

    IF nomonsters ;
       .AND. ( i == MT_SKULL .OR. ( mobjinfo[ i + 1 ]:flags & MF_COUNTKILL ) != 0 )
        RETURN NIL
    ENDIF

    x := mthing:x * FRACUNIT
    y := mthing:y * FRACUNIT

    IF ( mobjinfo[ i + 1 ]:flags & MF_SPAWNCEILING ) != 0
        z := ONCEILINGZ
    ELSE
        z := ONFLOORZ
    ENDIF

    mobj := P_SpawnMobj( x, y, z, i )
    AssignMapThing( mobj:spawnpoint, mthing )

    IF mobj:tics > 0
        mobj:tics := 1 + ( P_Random() % mobj:tics )
    ENDIF
    IF ( mobj:flags & MF_COUNTKILL ) != 0
        totalkills++
    ENDIF
    IF ( mobj:flags & MF_COUNTITEM ) != 0
        totalitems++
    ENDIF

    mobj:angle := ANG45 * Int( mthing:angle / 45 )
    IF ( mthing:options & MTF_AMBUSH ) != 0
        mobj:flags := ( mobj:flags | MF_AMBUSH )
    ENDIF
RETURN NIL

FUNCTION P_SpawnPuff( x, y, z )
    LOCAL th
    MEMVAR attackrange

    z += ( P_Random() - P_Random() ) * 1024

    th := P_SpawnMobj( x, y, z, MT_PUFF )
    th:momz := FRACUNIT
    th:tics -= ( P_Random() & 3 )

    IF th:tics < 1
        th:tics := 1
    ENDIF

    IF attackrange == MELEERANGE
        P_SetMobjState( th, S_PUFF3 )
    ENDIF
RETURN NIL

FUNCTION P_SpawnBlood( x, y, z, damage )
    LOCAL th

    z += ( P_Random() - P_Random() ) * 1024
    th := P_SpawnMobj( x, y, z, MT_BLOOD )
    th:momz := FRACUNIT * 2
    th:tics -= ( P_Random() & 3 )

    IF th:tics < 1
        th:tics := 1
    ENDIF

    IF damage <= 12 .AND. damage >= 9
        P_SetMobjState( th, S_BLOOD2 )
    ELSEIF damage < 9
        P_SetMobjState( th, S_BLOOD3 )
    ENDIF
RETURN NIL

FUNCTION P_CheckMissileSpawn( th )
    th:tics -= ( P_Random() & 3 )
    IF th:tics < 1
        th:tics := 1
    ENDIF

    th:x += Int( th:momx / 2 )
    th:y += Int( th:momy / 2 )
    th:z += Int( th:momz / 2 )

    IF ! P_TryMove( th, th:x, th:y )
        P_ExplodeMissile( th )
    ENDIF
RETURN NIL

FUNCTION P_SubstNullMobj( mobj )
    IF mobj == NIL
        dummy_mobj:x := 0
        dummy_mobj:y := 0
        dummy_mobj:z := 0
        dummy_mobj:flags := 0
        mobj := dummy_mobj
    ENDIF
RETURN mobj

FUNCTION P_SpawnMissile( source, dest, type )
    LOCAL th
    LOCAL an
    LOCAL dist
    MEMVAR finecosine
    MEMVAR finesine

    th := P_SpawnMobj( source:x, source:y, source:z + 4 * 8 * FRACUNIT, type )

    IF th:info:seesound != 0
        S_StartSound( th, th:info:seesound )
    ENDIF

    th:target := source
    an := R_PointToAngle2( source:x, source:y, dest:x, dest:y )

    IF ( dest:flags & MF_SHADOW ) != 0
        an := ( ( an + ( P_Random() - P_Random() ) * 1048576 ) & 0xFFFFFFFF )
    ENDIF

    th:angle := an
    an := UShr( an, ANGLETOFINESHIFT )
    th:momx := FixedMul( th:info:speed, finecosine[ an + 1 ] )
    th:momy := FixedMul( th:info:speed, finesine[ an + 1 ] )

    dist := P_AproxDistance( dest:x - source:x, dest:y - source:y )
    dist := Int( dist / th:info:speed )

    IF dist < 1
        dist := 1
    ENDIF

    th:momz := Int( ( dest:z - source:z ) / dist )
    P_CheckMissileSpawn( th )
RETURN th

FUNCTION P_SpawnPlayerMissile( source, type )
    LOCAL th
    LOCAL an
    LOCAL x
    LOCAL y
    LOCAL z
    LOCAL slope
    MEMVAR finecosine
    MEMVAR finesine
    MEMVAR linetarget

    an := source:angle
    slope := P_AimLineAttack( source, an, 16 * 64 * FRACUNIT )

    IF linetarget == NIL
        an := ( ( an + 67108864 ) & 0xFFFFFFFF )
        slope := P_AimLineAttack( source, an, 16 * 64 * FRACUNIT )

        IF linetarget == NIL
            an := ( ( an - 134217728 ) & 0xFFFFFFFF )
            slope := P_AimLineAttack( source, an, 16 * 64 * FRACUNIT )
        ENDIF

        IF linetarget == NIL
            an := source:angle
            slope := 0
        ENDIF
    ENDIF

    x := source:x
    y := source:y
    z := source:z + 4 * 8 * FRACUNIT

    th := P_SpawnMobj( x, y, z, type )

    IF th:info:seesound != 0
        S_StartSound( th, th:info:seesound )
    ENDIF

    th:target := source
    th:angle := an
    th:momx := FixedMul( th:info:speed, finecosine[ UShr( an, ANGLETOFINESHIFT ) + 1 ] )
    th:momy := FixedMul( th:info:speed, finesine[ UShr( an, ANGLETOFINESHIFT ) + 1 ] )
    th:momz := FixedMul( th:info:speed, slope )

    P_CheckMissileSpawn( th )
RETURN NIL
