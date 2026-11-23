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

STATIC swingx
STATIC swingy

#include "p_pspr.ch"
#include "p_local.ch"
#include "info.ch"
#include "d_items.ch"
#include "d_event.ch"
#include "deh_misc.ch"
#include "doomstat.ch"

MEMVAR deh_bfg_cells_per_shot


INIT PROCEDURE init_p_pspr
    PUBLIC bulletslope

    bulletslope := 0
    swingx := 0
    swingy := 0
RETURN

STATIC FUNCTION AsInt32( n )
    n := ( n & 0xFFFFFFFF )
    IF n >= 2147483648
        RETURN n - 4294967296
    ENDIF
RETURN n

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

FUNCTION P_SetPsprite( player, position, stnum )
    LOCAL psp
    LOCAL state
    LOCAL xAct
    MEMVAR states

    psp := player:psprites[ position + 1 ]

    DO WHILE .T.
        IF stnum == 0
            psp:state := NIL
            psp:iState := 0
            EXIT
        ENDIF

        state := states[ stnum + 1 ]
        psp:state := state
        psp:iState := stnum
        psp:tics := state:tics

        IF state:misc1 != 0
            psp:sx := state:misc1 * FRACUNIT
            psp:sy := state:misc2 * FRACUNIT
        ENDIF

        IF state:action != NIL
            xAct := state:action:acp2
            IF xAct == NIL
                xAct := state:action:acv
            ENDIF
            IF xAct != NIL
                IfaceCall( xAct, player, psp )
                IF psp:state == NIL
                    EXIT
                ENDIF
            ENDIF
        ENDIF

        stnum := psp:state:nextstate
        IF psp:tics != 0
            EXIT
        ENDIF
    ENDDO
RETURN NIL

FUNCTION P_CalcSwing( player )
    LOCAL swing
    LOCAL angle
    MEMVAR finesine
    MEMVAR leveltime

    swing := player:bob

    angle := ( ( Int( FINEANGLES / 70 ) * leveltime ) & FINEMASK )
    swingx := FixedMul( swing, finesine[ angle + 1 ] )

    angle := ( ( Int( FINEANGLES / 70 ) * leveltime + Int( FINEANGLES / 2 ) ) & FINEMASK )
    swingy := -FixedMul( swingx, finesine[ angle + 1 ] )
RETURN NIL

FUNCTION P_BringUpWeapon( player )
    LOCAL newstate
    MEMVAR weaponinfo

    IF player:pendingweapon == wp_nochange
        player:pendingweapon := player:readyweapon
    ENDIF

    IF player:pendingweapon == wp_chainsaw
        S_StartSound( player:mo, sfx_sawup )
    ENDIF

    newstate := weaponinfo[ player:pendingweapon + 1 ]:upstate

    player:pendingweapon := wp_nochange
    player:psprites[ ps_weapon + 1 ]:sy := WEAPONBOTTOM

    P_SetPsprite( player, ps_weapon, newstate )
RETURN NIL

FUNCTION P_CheckAmmo( player )
    LOCAL ammo
    LOCAL count
    MEMVAR gamemode
    MEMVAR weaponinfo

    ammo := weaponinfo[ player:readyweapon + 1 ]:ammo

    IF player:readyweapon == wp_bfg
        count := deh_bfg_cells_per_shot
    ELSEIF player:readyweapon == wp_supershotgun
        count := 2
    ELSE
        count := 1
    ENDIF

    IF ammo == am_noammo .OR. player:ammo[ ammo + 1 ] >= count
        RETURN .T.
    ENDIF

    DO WHILE .T.
        IF player:weaponowned[ wp_plasma + 1 ] ;
           .AND. player:ammo[ am_cell + 1 ] != 0 ;
           .AND. gamemode != shareware
            player:pendingweapon := wp_plasma
        ELSEIF player:weaponowned[ wp_supershotgun + 1 ] ;
               .AND. player:ammo[ am_shell + 1 ] > 2 ;
               .AND. gamemode == commercial
            player:pendingweapon := wp_supershotgun
        ELSEIF player:weaponowned[ wp_chaingun + 1 ] ;
               .AND. player:ammo[ am_clip + 1 ] != 0
            player:pendingweapon := wp_chaingun
        ELSEIF player:weaponowned[ wp_shotgun + 1 ] ;
               .AND. player:ammo[ am_shell + 1 ] != 0
            player:pendingweapon := wp_shotgun
        ELSEIF player:ammo[ am_clip + 1 ] != 0
            player:pendingweapon := wp_pistol
        ELSEIF player:weaponowned[ wp_chainsaw + 1 ]
            player:pendingweapon := wp_chainsaw
        ELSEIF player:weaponowned[ wp_missile + 1 ] ;
               .AND. player:ammo[ am_misl + 1 ] != 0
            player:pendingweapon := wp_missile
        ELSEIF player:weaponowned[ wp_bfg + 1 ] ;
               .AND. player:ammo[ am_cell + 1 ] > 40 ;
               .AND. gamemode != shareware
            player:pendingweapon := wp_bfg
        ELSE
            player:pendingweapon := wp_fist
        ENDIF

        IF player:pendingweapon != wp_nochange
            EXIT
        ENDIF
    ENDDO

    P_SetPsprite( player, ps_weapon, weaponinfo[ player:readyweapon + 1 ]:downstate )
RETURN .F.

FUNCTION P_FireWeapon( player )
    LOCAL newstate
    MEMVAR weaponinfo

    IF ! P_CheckAmmo( player )
        RETURN NIL
    ENDIF

    P_SetMobjState( player:mo, S_PLAY_ATK1 )
    newstate := weaponinfo[ player:readyweapon + 1 ]:atkstate
    P_SetPsprite( player, ps_weapon, newstate )
    P_NoiseAlert( player:mo, player:mo )
RETURN NIL

FUNCTION P_DropWeapon( player )
    MEMVAR weaponinfo
    P_SetPsprite( player, ps_weapon, weaponinfo[ player:readyweapon + 1 ]:downstate )
RETURN NIL

FUNCTION A_WeaponReady( player, psp )
    LOCAL newstate
    LOCAL angle
    MEMVAR finecosine
    MEMVAR finesine
    MEMVAR leveltime
    MEMVAR states
    MEMVAR weaponinfo

    IF player:mo:iState == S_PLAY_ATK1 .OR. player:mo:iState == S_PLAY_ATK2
        P_SetMobjState( player:mo, S_PLAY )
    ENDIF

    IF player:readyweapon == wp_chainsaw ;
       .AND. ( psp:iState == S_SAW .OR. psp:state == states[ S_SAW + 1 ] )
        S_StartSound( player:mo, sfx_sawidl )
    ENDIF

    IF player:pendingweapon != wp_nochange .OR. player:health == 0
        newstate := weaponinfo[ player:readyweapon + 1 ]:downstate
        P_SetPsprite( player, ps_weapon, newstate )
        RETURN NIL
    ENDIF

    IF ( player:cmd:buttons & BT_ATTACK ) != 0
        IF ! player:attackdown ;
           .OR. ( player:readyweapon != wp_missile .AND. player:readyweapon != wp_bfg )
            player:attackdown := .T.
            P_FireWeapon( player )
            RETURN NIL
        ENDIF
    ELSE
        player:attackdown := .F.
    ENDIF

    angle := ( ( 128 * leveltime ) & FINEMASK )
    psp:sx := FRACUNIT + FixedMul( player:bob, finecosine[ angle + 1 ] )
    angle := ( angle & ( Int( FINEANGLES / 2 ) - 1 ) )
    psp:sy := WEAPONTOP + FixedMul( player:bob, finesine[ angle + 1 ] )
RETURN NIL

FUNCTION A_ReFire( player, psp )
    HB_SYMBOL_UNUSED( psp )

    IF ( player:cmd:buttons & BT_ATTACK ) != 0 ;
       .AND. player:pendingweapon == wp_nochange ;
       .AND. player:health != 0
        player:refire++
        P_FireWeapon( player )
    ELSE
        player:refire := 0
        P_CheckAmmo( player )
    ENDIF
RETURN NIL

FUNCTION A_CheckReload( player, psp )
    HB_SYMBOL_UNUSED( psp )
    P_CheckAmmo( player )
RETURN NIL

FUNCTION A_Lower( player, psp )
    psp:sy += LOWERSPEED

    IF psp:sy < WEAPONBOTTOM
        RETURN NIL
    ENDIF

    IF player:playerstate == PST_DEAD
        psp:sy := WEAPONBOTTOM
        RETURN NIL
    ENDIF

    IF player:health == 0
        P_SetPsprite( player, ps_weapon, S_NULL )
        RETURN NIL
    ENDIF

    player:readyweapon := player:pendingweapon
    P_BringUpWeapon( player )
RETURN NIL

FUNCTION A_Raise( player, psp )
    LOCAL newstate
    MEMVAR weaponinfo

    psp:sy -= RAISESPEED

    IF psp:sy > WEAPONTOP
        RETURN NIL
    ENDIF

    psp:sy := WEAPONTOP

    newstate := weaponinfo[ player:readyweapon + 1 ]:readystate
    P_SetPsprite( player, ps_weapon, newstate )
RETURN NIL

FUNCTION A_GunFlash( player, psp )
    MEMVAR weaponinfo
    HB_SYMBOL_UNUSED( psp )
    P_SetMobjState( player:mo, S_PLAY_ATK2 )
    P_SetPsprite( player, ps_flash, weaponinfo[ player:readyweapon + 1 ]:flashstate )
RETURN NIL

FUNCTION A_Punch( player, psp )
    LOCAL angle
    LOCAL damage
    LOCAL slope
    MEMVAR linetarget

    HB_SYMBOL_UNUSED( psp )

    damage := ( ( P_Random() % 10 ) + 1 ) * 2

    IF player:powers[ pw_strength + 1 ] != 0
        damage *= 10
    ENDIF

    angle := player:mo:angle
    angle := ( ( angle + ( P_Random() - P_Random() ) * 262144 ) & 0xFFFFFFFF )
    slope := P_AimLineAttack( player:mo, angle, MELEERANGE )
    P_LineAttack( player:mo, angle, MELEERANGE, slope, damage )

    IF linetarget != NIL
        S_StartSound( player:mo, sfx_punch )
        player:mo:angle := R_PointToAngle2( player:mo:x, player:mo:y, ;
                                            linetarget:x, linetarget:y )
    ENDIF
RETURN NIL

FUNCTION A_Saw( player, psp )
    LOCAL angle
    LOCAL damage
    LOCAL slope
    LOCAL nUDiff
    MEMVAR linetarget

    HB_SYMBOL_UNUSED( psp )

    damage := 2 * ( ( P_Random() % 10 ) + 1 )
    angle := player:mo:angle
    angle := ( ( angle + ( P_Random() - P_Random() ) * 262144 ) & 0xFFFFFFFF )

    slope := P_AimLineAttack( player:mo, angle, MELEERANGE + 1 )
    P_LineAttack( player:mo, angle, MELEERANGE + 1, slope, damage )

    IF linetarget == NIL
        S_StartSound( player:mo, sfx_sawful )
        RETURN NIL
    ENDIF
    S_StartSound( player:mo, sfx_sawhit )

    angle := R_PointToAngle2( player:mo:x, player:mo:y, linetarget:x, linetarget:y )
    nUDiff := ( ( angle - player:mo:angle ) & 0xFFFFFFFF )

    IF nUDiff > ANG180
        IF AsInt32( nUDiff ) < -Int( ANG90 / 20 )
            player:mo:angle := ( ( angle + Int( ANG90 / 21 ) ) & 0xFFFFFFFF )
        ELSE
            player:mo:angle := ( ( player:mo:angle - Int( ANG90 / 20 ) ) & 0xFFFFFFFF )
        ENDIF
    ELSE
        IF nUDiff > Int( ANG90 / 20 )
            player:mo:angle := ( ( angle - Int( ANG90 / 21 ) ) & 0xFFFFFFFF )
        ELSE
            player:mo:angle := ( ( player:mo:angle + Int( ANG90 / 20 ) ) & 0xFFFFFFFF )
        ENDIF
    ENDIF
    player:mo:flags := ( player:mo:flags | MF_JUSTATTACKED )
RETURN NIL

STATIC FUNCTION DecreaseAmmo( player, ammonum, amount )
    IF ammonum < NUMAMMO
        player:ammo[ ammonum + 1 ] -= amount
    ELSE
        player:maxammo[ ammonum - NUMAMMO + 1 ] -= amount
    ENDIF
RETURN NIL

FUNCTION A_FireMissile( player, psp )
    MEMVAR weaponinfo
    HB_SYMBOL_UNUSED( psp )
    DecreaseAmmo( player, weaponinfo[ player:readyweapon + 1 ]:ammo, 1 )
    P_SpawnPlayerMissile( player:mo, MT_ROCKET )
RETURN NIL

FUNCTION A_FireBFG( player, psp )
    MEMVAR weaponinfo
    HB_SYMBOL_UNUSED( psp )
    DecreaseAmmo( player, weaponinfo[ player:readyweapon + 1 ]:ammo, deh_bfg_cells_per_shot )
    P_SpawnPlayerMissile( player:mo, MT_BFG )
RETURN NIL

FUNCTION A_FirePlasma( player, psp )
    MEMVAR weaponinfo
    HB_SYMBOL_UNUSED( psp )
    DecreaseAmmo( player, weaponinfo[ player:readyweapon + 1 ]:ammo, 1 )

    P_SetPsprite( player, ps_flash, ;
                  weaponinfo[ player:readyweapon + 1 ]:flashstate + ( P_Random() & 1 ) )

    P_SpawnPlayerMissile( player:mo, MT_PLASMA )
RETURN NIL

FUNCTION P_BulletSlope( mo )
    LOCAL an
    MEMVAR bulletslope
    MEMVAR linetarget

    an := mo:angle
    bulletslope := P_AimLineAttack( mo, an, 16 * 64 * FRACUNIT )

    IF linetarget == NIL
        an := ( ( an + 67108864 ) & 0xFFFFFFFF )
        bulletslope := P_AimLineAttack( mo, an, 16 * 64 * FRACUNIT )
        IF linetarget == NIL
            an := ( ( an - 134217728 ) & 0xFFFFFFFF )
            bulletslope := P_AimLineAttack( mo, an, 16 * 64 * FRACUNIT )
        ENDIF
    ENDIF
RETURN NIL

FUNCTION P_GunShot( mo, accurate )
    LOCAL angle
    LOCAL damage
    MEMVAR bulletslope

    damage := 5 * ( ( P_Random() % 3 ) + 1 )
    angle := mo:angle

    IF ! accurate
        angle := ( ( angle + ( P_Random() - P_Random() ) * 262144 ) & 0xFFFFFFFF )
    ENDIF

    P_LineAttack( mo, angle, MISSILERANGE, bulletslope, damage )
RETURN NIL

FUNCTION A_FirePistol( player, psp )
    MEMVAR weaponinfo
    HB_SYMBOL_UNUSED( psp )
    S_StartSound( player:mo, sfx_pistol )

    P_SetMobjState( player:mo, S_PLAY_ATK2 )
    DecreaseAmmo( player, weaponinfo[ player:readyweapon + 1 ]:ammo, 1 )

    P_SetPsprite( player, ps_flash, weaponinfo[ player:readyweapon + 1 ]:flashstate )

    P_BulletSlope( player:mo )
    P_GunShot( player:mo, player:refire == 0 )
RETURN NIL

FUNCTION A_FireShotgun( player, psp )
    LOCAL i
    MEMVAR weaponinfo

    HB_SYMBOL_UNUSED( psp )
    S_StartSound( player:mo, sfx_shotgn )
    P_SetMobjState( player:mo, S_PLAY_ATK2 )

    DecreaseAmmo( player, weaponinfo[ player:readyweapon + 1 ]:ammo, 1 )

    P_SetPsprite( player, ps_flash, weaponinfo[ player:readyweapon + 1 ]:flashstate )

    P_BulletSlope( player:mo )

    FOR i := 0 TO 6
        P_GunShot( player:mo, .F. )
    NEXT
RETURN NIL

FUNCTION A_FireShotgun2( player, psp )
    LOCAL i
    LOCAL angle
    LOCAL damage
    MEMVAR bulletslope
    MEMVAR weaponinfo

    HB_SYMBOL_UNUSED( psp )
    S_StartSound( player:mo, sfx_dshtgn )
    P_SetMobjState( player:mo, S_PLAY_ATK2 )

    DecreaseAmmo( player, weaponinfo[ player:readyweapon + 1 ]:ammo, 2 )

    P_SetPsprite( player, ps_flash, weaponinfo[ player:readyweapon + 1 ]:flashstate )

    P_BulletSlope( player:mo )

    FOR i := 0 TO 19
        damage := 5 * ( ( P_Random() % 3 ) + 1 )
        angle := player:mo:angle
        angle := ( ( angle + ( P_Random() - P_Random() ) * 524288 ) & 0xFFFFFFFF )
        P_LineAttack( player:mo, ;
                      angle, ;
                      MISSILERANGE, ;
                      bulletslope + ( ( P_Random() - P_Random() ) * 32 ), ;
                      damage )
    NEXT
RETURN NIL

FUNCTION A_FireCGun( player, psp )
    MEMVAR weaponinfo
    S_StartSound( player:mo, sfx_pistol )

    IF player:ammo[ weaponinfo[ player:readyweapon + 1 ]:ammo + 1 ] == 0
        RETURN NIL
    ENDIF

    P_SetMobjState( player:mo, S_PLAY_ATK2 )
    DecreaseAmmo( player, weaponinfo[ player:readyweapon + 1 ]:ammo, 1 )

    P_SetPsprite( player, ps_flash, ;
                  weaponinfo[ player:readyweapon + 1 ]:flashstate + psp:iState - S_CHAIN1 )

    P_BulletSlope( player:mo )
    P_GunShot( player:mo, player:refire == 0 )
RETURN NIL

FUNCTION A_Light0( player, psp )
    HB_SYMBOL_UNUSED( psp )
    player:extralight := 0
RETURN NIL

FUNCTION A_Light1( player, psp )
    HB_SYMBOL_UNUSED( psp )
    player:extralight := 1
RETURN NIL

FUNCTION A_Light2( player, psp )
    HB_SYMBOL_UNUSED( psp )
    player:extralight := 2
RETURN NIL

FUNCTION A_BFGSpray( mo )
    LOCAL i
    LOCAL j
    LOCAL damage
    LOCAL an
    MEMVAR linetarget

    FOR i := 0 TO 39
        an := mo:angle - Int( ANG90 / 2 ) + Int( ANG90 / 40 ) * i

        P_AimLineAttack( mo:target, an, 16 * 64 * FRACUNIT )

        IF linetarget == NIL
            LOOP
        ENDIF

        P_SpawnMobj( linetarget:x, ;
                     linetarget:y, ;
                     linetarget:z + Shar( linetarget:height, 2 ), ;
                     MT_EXTRABFG )

        damage := 0
        FOR j := 0 TO 14
            damage += ( P_Random() & 7 ) + 1
        NEXT

        P_DamageMobj( linetarget, mo:target, mo:target, damage )
    NEXT
RETURN NIL

FUNCTION A_BFGsound( player, psp )
    HB_SYMBOL_UNUSED( psp )
    S_StartSound( player:mo, sfx_bfg )
RETURN NIL

FUNCTION P_SetupPsprites( player )
    LOCAL i

    FOR i := 0 TO NUMPSPRITES - 1
        player:psprites[ i + 1 ]:state := NIL
        player:psprites[ i + 1 ]:iState := 0
    NEXT

    player:pendingweapon := player:readyweapon
    P_BringUpWeapon( player )
RETURN NIL

FUNCTION P_MovePsprites( player )
    LOCAL i
    LOCAL psp

    FOR i := 0 TO NUMPSPRITES - 1
        psp := player:psprites[ i + 1 ]
        IF psp:state != NIL
            IF psp:tics != -1
                psp:tics--
                IF psp:tics == 0
                    P_SetPsprite( player, i, psp:state:nextstate )
                ENDIF
            ENDIF
        ENDIF
    NEXT

    player:psprites[ ps_flash + 1 ]:sx := player:psprites[ ps_weapon + 1 ]:sx
    player:psprites[ ps_flash + 1 ]:sy := player:psprites[ ps_weapon + 1 ]:sy
RETURN NIL
