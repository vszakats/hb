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

#include "p_inter.ch"
#include "p_local.ch"
#include "p_mobj.ch"
#include "dstrings.ch"
#include "deh_str.ch"
#include "deh_misc.ch"
#include "doomstat.ch"

MEMVAR deh_green_armor_class
MEMVAR deh_blue_armor_class
MEMVAR deh_max_health
MEMVAR deh_max_armor
MEMVAR deh_soulsphere_health
MEMVAR deh_max_soulsphere
MEMVAR deh_megasphere_health
MEMVAR clipammo
#include "info.ch"
#include "d_items.ch"
#include "doomdef.ch"


#ifndef sfx_itemup
#define sfx_itemup 32
#define sfx_wpnup  33
#define sfx_noway  81
#define sfx_getpow 93
#endif


INIT PROCEDURE init_p_inter
    PUBLIC maxammo
    PUBLIC clipammo
    maxammo  := { 200, 50, 300, 50 }
    clipammo := { 10, 4, 20, 1 }
RETURN

STATIC FUNCTION PlayerIndex( p )
    LOCAL i
    MEMVAR players

    FOR i := 0 TO MAXPLAYERS - 1
        IF players[ i + 1 ] == p
            RETURN i
        ENDIF
    NEXT
RETURN 0

FUNCTION P_GiveAmmo( player, ammo, num )
    LOCAL oldammo
    MEMVAR gameskill

    IF ammo == am_noammo
        RETURN .F.
    ENDIF

    IF ammo > NUMAMMO
        I_Error( "P_GiveAmmo: bad type " + LTrim( Str( ammo ) ) )
    ENDIF

    IF player:ammo[ ammo + 1 ] == player:maxammo[ ammo + 1 ]
        RETURN .F.
    ENDIF

    IF num != 0
        num := num * clipammo[ ammo + 1 ]
    ELSE
        num := Int( clipammo[ ammo + 1 ] / 2 )
    ENDIF

    IF gameskill == sk_baby .OR. gameskill == sk_nightmare
        num := num * 2
    ENDIF

    oldammo := player:ammo[ ammo + 1 ]
    player:ammo[ ammo + 1 ] += num

    IF player:ammo[ ammo + 1 ] > player:maxammo[ ammo + 1 ]
        player:ammo[ ammo + 1 ] := player:maxammo[ ammo + 1 ]
    ENDIF

    IF oldammo != 0
        RETURN .T.
    ENDIF

    SWITCH ammo
    CASE am_clip
        IF player:readyweapon == wp_fist
            IF player:weaponowned[ wp_chaingun + 1 ]
                player:pendingweapon := wp_chaingun
            ELSE
                player:pendingweapon := wp_pistol
            ENDIF
        ENDIF
        EXIT

    CASE am_shell
        IF player:readyweapon == wp_fist ;
           .OR. player:readyweapon == wp_pistol
            IF player:weaponowned[ wp_shotgun + 1 ]
                player:pendingweapon := wp_shotgun
            ENDIF
        ENDIF
        EXIT

    CASE am_cell
        IF player:readyweapon == wp_fist ;
           .OR. player:readyweapon == wp_pistol
            IF player:weaponowned[ wp_plasma + 1 ]
                player:pendingweapon := wp_plasma
            ENDIF
        ENDIF
        EXIT

    CASE am_misl
        IF player:readyweapon == wp_fist
            IF player:weaponowned[ wp_missile + 1 ]
                player:pendingweapon := wp_missile
            ENDIF
        ENDIF
        EXIT

    OTHERWISE
        EXIT
    ENDSWITCH

RETURN .T.

FUNCTION P_GiveWeapon( player, weapon, dropped )
    LOCAL gaveammo
    LOCAL gaveweapon
    MEMVAR consoleplayer
    MEMVAR deathmatch
    MEMVAR netgame
    MEMVAR players
    MEMVAR weaponinfo

    IF netgame .AND. deathmatch != 2 .AND. ! dropped
        IF player:weaponowned[ weapon + 1 ]
            RETURN .F.
        ENDIF

        player:bonuscount += BONUSADD
        player:weaponowned[ weapon + 1 ] := .T.

        IF deathmatch != 0
            P_GiveAmmo( player, weaponinfo[ weapon + 1 ]:ammo, 5 )
        ELSE
            P_GiveAmmo( player, weaponinfo[ weapon + 1 ]:ammo, 2 )
        ENDIF
        player:pendingweapon := weapon

        IF player == players[ consoleplayer + 1 ]
            S_StartSound( NIL, sfx_wpnup )
        ENDIF
        RETURN .F.
    ENDIF

    IF weaponinfo[ weapon + 1 ]:ammo != am_noammo
        IF dropped
            gaveammo := P_GiveAmmo( player, weaponinfo[ weapon + 1 ]:ammo, 1 )
        ELSE
            gaveammo := P_GiveAmmo( player, weaponinfo[ weapon + 1 ]:ammo, 2 )
        ENDIF
    ELSE
        gaveammo := .F.
    ENDIF

    IF player:weaponowned[ weapon + 1 ]
        gaveweapon := .F.
    ELSE
        gaveweapon := .T.
        player:weaponowned[ weapon + 1 ] := .T.
        player:pendingweapon := weapon
    ENDIF

RETURN gaveweapon .OR. gaveammo

FUNCTION P_GiveBody( player, num )
    IF player:health >= MAXHEALTH
        RETURN .F.
    ENDIF

    player:health += num
    IF player:health > MAXHEALTH
        player:health := MAXHEALTH
    ENDIF
    player:mo:health := player:health

RETURN .T.

FUNCTION P_GiveArmor( player, armortype )
    LOCAL hits

    hits := armortype * 100
    IF player:armorpoints >= hits
        RETURN .F.
    ENDIF

    player:armortype := armortype
    player:armorpoints := hits

RETURN .T.

FUNCTION P_GiveCard( player, card )
    IF player:cards[ card + 1 ]
        RETURN NIL
    ENDIF

    player:bonuscount := BONUSADD
    player:cards[ card + 1 ] := .T.
RETURN NIL

FUNCTION P_GivePower( player, power )
    IF power == pw_invulnerability
        player:powers[ power + 1 ] := INVULNTICS
        RETURN .T.
    ENDIF

    IF power == pw_invisibility
        player:powers[ power + 1 ] := INVISTICS
        player:mo:flags := ( player:mo:flags | MF_SHADOW )
        RETURN .T.
    ENDIF

    IF power == pw_infrared
        player:powers[ power + 1 ] := INFRATICS
        RETURN .T.
    ENDIF

    IF power == pw_ironfeet
        player:powers[ power + 1 ] := IRONTICS
        RETURN .T.
    ENDIF

    IF power == pw_strength
        P_GiveBody( player, 100 )
        player:powers[ power + 1 ] := 1
        RETURN .T.
    ENDIF

    IF player:powers[ power + 1 ] != 0
        RETURN .F.
    ENDIF

    player:powers[ power + 1 ] := 1
RETURN .T.

FUNCTION P_TouchSpecialThing( special, toucher )
    LOCAL player
    LOCAL i
    LOCAL delta
    LOCAL sound
    MEMVAR consoleplayer
    MEMVAR gamemode
    MEMVAR netgame
    MEMVAR players

    delta := special:z - toucher:z

    IF delta > toucher:height .OR. delta < -8 * FRACUNIT
        RETURN NIL
    ENDIF

    sound := sfx_itemup
    player := toucher:player

    IF toucher:health <= 0
        RETURN NIL
    ENDIF

    SWITCH special:sprite
    CASE SPR_ARM1
        IF ! P_GiveArmor( player, deh_green_armor_class )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTARMOR )
        EXIT

    CASE SPR_ARM2
        IF ! P_GiveArmor( player, deh_blue_armor_class )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTMEGA )
        EXIT

    CASE SPR_BON1
        player:health += 1
        IF player:health > deh_max_health
            player:health := deh_max_health
        ENDIF
        player:mo:health := player:health
        player:message := DEH_String( GOTHTHBONUS )
        EXIT

    CASE SPR_BON2
        player:armorpoints += 1
        IF player:armorpoints > deh_max_armor
            player:armorpoints := deh_max_armor
        ENDIF
        IF player:armortype == 0
            player:armortype := 1
        ENDIF
        player:message := DEH_String( GOTARMBONUS )
        EXIT

    CASE SPR_SOUL
        player:health += deh_soulsphere_health
        IF player:health > deh_max_soulsphere
            player:health := deh_max_soulsphere
        ENDIF
        player:mo:health := player:health
        player:message := DEH_String( GOTSUPER )
        sound := sfx_getpow
        EXIT

    CASE SPR_MEGA
        IF gamemode != commercial
            RETURN NIL
        ENDIF
        player:health := deh_megasphere_health
        player:mo:health := player:health
        P_GiveArmor( player, 2 )
        player:message := DEH_String( GOTMSPHERE )
        sound := sfx_getpow
        EXIT

    CASE SPR_BKEY
        IF ! player:cards[ it_bluecard + 1 ]
            player:message := DEH_String( GOTBLUECARD )
        ENDIF
        P_GiveCard( player, it_bluecard )
        IF ! netgame
            EXIT
        ENDIF
        RETURN NIL

    CASE SPR_YKEY
        IF ! player:cards[ it_yellowcard + 1 ]
            player:message := DEH_String( GOTYELWCARD )
        ENDIF
        P_GiveCard( player, it_yellowcard )
        IF ! netgame
            EXIT
        ENDIF
        RETURN NIL

    CASE SPR_RKEY
        IF ! player:cards[ it_redcard + 1 ]
            player:message := DEH_String( GOTREDCARD )
        ENDIF
        P_GiveCard( player, it_redcard )
        IF ! netgame
            EXIT
        ENDIF
        RETURN NIL

    CASE SPR_BSKU
        IF ! player:cards[ it_blueskull + 1 ]
            player:message := DEH_String( GOTBLUESKUL )
        ENDIF
        P_GiveCard( player, it_blueskull )
        IF ! netgame
            EXIT
        ENDIF
        RETURN NIL

    CASE SPR_YSKU
        IF ! player:cards[ it_yellowskull + 1 ]
            player:message := DEH_String( GOTYELWSKUL )
        ENDIF
        P_GiveCard( player, it_yellowskull )
        IF ! netgame
            EXIT
        ENDIF
        RETURN NIL

    CASE SPR_RSKU
        IF ! player:cards[ it_redskull + 1 ]
            player:message := DEH_String( GOTREDSKULL )
        ENDIF
        P_GiveCard( player, it_redskull )
        IF ! netgame
            EXIT
        ENDIF
        RETURN NIL

    CASE SPR_STIM
        IF ! P_GiveBody( player, 10 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTSTIM )
        EXIT

    CASE SPR_MEDI
        IF ! P_GiveBody( player, 25 )
            RETURN NIL
        ENDIF
        IF player:health < 25
            player:message := DEH_String( GOTMEDINEED )
        ELSE
            player:message := DEH_String( GOTMEDIKIT )
        ENDIF
        EXIT

    CASE SPR_PINV
        IF ! P_GivePower( player, pw_invulnerability )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTINVUL )
        sound := sfx_getpow
        EXIT

    CASE SPR_PSTR
        IF ! P_GivePower( player, pw_strength )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTBERSERK )
        IF player:readyweapon != wp_fist
            player:pendingweapon := wp_fist
        ENDIF
        sound := sfx_getpow
        EXIT

    CASE SPR_PINS
        IF ! P_GivePower( player, pw_invisibility )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTINVIS )
        sound := sfx_getpow
        EXIT

    CASE SPR_SUIT
        IF ! P_GivePower( player, pw_ironfeet )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTSUIT )
        sound := sfx_getpow
        EXIT

    CASE SPR_PMAP
        IF ! P_GivePower( player, pw_allmap )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTMAP )
        sound := sfx_getpow
        EXIT

    CASE SPR_PVIS
        IF ! P_GivePower( player, pw_infrared )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTVISOR )
        sound := sfx_getpow
        EXIT

    CASE SPR_CLIP
        IF ( special:flags & MF_DROPPED ) != 0
            IF ! P_GiveAmmo( player, am_clip, 0 )
                RETURN NIL
            ENDIF
        ELSE
            IF ! P_GiveAmmo( player, am_clip, 1 )
                RETURN NIL
            ENDIF
        ENDIF
        player:message := DEH_String( GOTCLIP )
        EXIT

    CASE SPR_AMMO
        IF ! P_GiveAmmo( player, am_clip, 5 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTCLIPBOX )
        EXIT

    CASE SPR_ROCK
        IF ! P_GiveAmmo( player, am_misl, 1 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTROCKET )
        EXIT

    CASE SPR_BROK
        IF ! P_GiveAmmo( player, am_misl, 5 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTROCKBOX )
        EXIT

    CASE SPR_CELL
        IF ! P_GiveAmmo( player, am_cell, 1 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTCELL )
        EXIT

    CASE SPR_CELP
        IF ! P_GiveAmmo( player, am_cell, 5 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTCELLBOX )
        EXIT

    CASE SPR_SHEL
        IF ! P_GiveAmmo( player, am_shell, 1 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTSHELLS )
        EXIT

    CASE SPR_SBOX
        IF ! P_GiveAmmo( player, am_shell, 5 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTSHELLBOX )
        EXIT

    CASE SPR_BPAK
        IF ! player:backpack
            FOR i := 0 TO NUMAMMO - 1
                player:maxammo[ i + 1 ] := player:maxammo[ i + 1 ] * 2
            NEXT
            player:backpack := .T.
        ENDIF
        FOR i := 0 TO NUMAMMO - 1
            P_GiveAmmo( player, i, 1 )
        NEXT
        player:message := DEH_String( GOTBACKPACK )
        EXIT

    CASE SPR_BFUG
        IF ! P_GiveWeapon( player, wp_bfg, .F. )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTBFG9000 )
        sound := sfx_wpnup
        EXIT

    CASE SPR_MGUN
        IF ! P_GiveWeapon( player, wp_chaingun, ( special:flags & MF_DROPPED ) != 0 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTCHAINGUN )
        sound := sfx_wpnup
        EXIT

    CASE SPR_CSAW
        IF ! P_GiveWeapon( player, wp_chainsaw, .F. )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTCHAINSAW )
        sound := sfx_wpnup
        EXIT

    CASE SPR_LAUN
        IF ! P_GiveWeapon( player, wp_missile, .F. )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTLAUNCHER )
        sound := sfx_wpnup
        EXIT

    CASE SPR_PLAS
        IF ! P_GiveWeapon( player, wp_plasma, .F. )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTPLASMA )
        sound := sfx_wpnup
        EXIT

    CASE SPR_SHOT
        IF ! P_GiveWeapon( player, wp_shotgun, ( special:flags & MF_DROPPED ) != 0 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTSHOTGUN )
        sound := sfx_wpnup
        EXIT

    CASE SPR_SGN2
        IF ! P_GiveWeapon( player, wp_supershotgun, ( special:flags & MF_DROPPED ) != 0 )
            RETURN NIL
        ENDIF
        player:message := DEH_String( GOTSHOTGUN2 )
        sound := sfx_wpnup
        EXIT

    OTHERWISE
        I_Error( "P_SpecialThing: Unknown gettable thing" )
        EXIT
    ENDSWITCH

    IF ( special:flags & MF_COUNTITEM ) != 0
        player:itemcount += 1
    ENDIF
    P_RemoveMobj( special )
    player:bonuscount += BONUSADD
    IF player == players[ consoleplayer + 1 ]
        S_StartSound( NIL, sound )
    ENDIF
RETURN NIL

FUNCTION P_KillMobj( source, target )
    LOCAL item
    LOCAL mo
    LOCAL nPlayer
    MEMVAR automapactive
    MEMVAR consoleplayer
    MEMVAR gameversion
    MEMVAR netgame
    MEMVAR players

    target:flags := ( target:flags & ( ( ( MF_SHOOTABLE | MF_FLOAT ) | MF_SKULLFLY ) ^^ 0xFFFFFFFF ) )

    IF target:type != MT_SKULL
        target:flags := ( target:flags & ( MF_NOGRAVITY ^^ 0xFFFFFFFF ) )
    ENDIF

    target:flags := ( target:flags | ( MF_CORPSE | MF_DROPOFF ) )
    target:height := Int( target:height / 4 )

    IF !( source == NIL ) .AND. !( source:player == NIL )
        IF ( target:flags & MF_COUNTKILL ) != 0
            source:player:killcount += 1
        ENDIF
        IF !( target:player == NIL )
            nPlayer := PlayerIndex( target:player )
            source:player:frags[ nPlayer + 1 ] += 1
        ENDIF
    ELSEIF ! netgame .AND. ( target:flags & MF_COUNTKILL ) != 0
        players[ 1 ]:killcount += 1
    ENDIF

    IF !( target:player == NIL )
        IF source == NIL
            nPlayer := PlayerIndex( target:player )
            target:player:frags[ nPlayer + 1 ] += 1
        ENDIF

        target:flags := ( target:flags & ( MF_SOLID ^^ 0xFFFFFFFF ) )
        target:player:playerstate := PST_DEAD
        P_DropWeapon( target:player )

        IF target:player == players[ consoleplayer + 1 ] .AND. automapactive
            AM_Stop()
        ENDIF
    ENDIF

    IF target:health < -target:info:spawnhealth .AND. target:info:xdeathstate != 0
        P_SetMobjState( target, target:info:xdeathstate )
    ELSE
        P_SetMobjState( target, target:info:deathstate )
    ENDIF
    target:tics -= ( P_Random() & 3 )

    IF target:tics < 1
        target:tics := 1
    ENDIF

    IF gameversion == exe_chex
        RETURN NIL
    ENDIF

    SWITCH target:type
    CASE MT_WOLFSS
        item := MT_CLIP
        EXIT
    CASE MT_POSSESSED
        item := MT_CLIP
        EXIT
    CASE MT_SHOTGUY
        item := MT_SHOTGUN
        EXIT
    CASE MT_CHAINGUY
        item := MT_CHAINGUN
        EXIT
    OTHERWISE
        RETURN NIL
    ENDSWITCH

    mo := P_SpawnMobj( target:x, target:y, ONFLOORZ, item )
    mo:flags := ( mo:flags | MF_DROPPED )
RETURN NIL

FUNCTION P_DamageMobj( target, inflictor, source, damage )
    LOCAL ang
    LOCAL saved
    LOCAL player
    LOCAL thrust
    LOCAL temp
    MEMVAR consoleplayer
    MEMVAR finecosine
    MEMVAR finesine
    MEMVAR gameskill
    MEMVAR players
    MEMVAR states

    IF ( target:flags & MF_SHOOTABLE ) == 0
        RETURN NIL
    ENDIF

    IF target:health <= 0
        RETURN NIL
    ENDIF

    IF ( target:flags & MF_SKULLFLY ) != 0
        target:momx := 0
        target:momy := 0
        target:momz := 0
    ENDIF

    player := target:player
    IF !( player == NIL ) .AND. gameskill == sk_baby
        damage := Shar( damage, 1 )
    ENDIF

    IF !( inflictor == NIL ) ;
       .AND. ( target:flags & MF_NOCLIP ) == 0 ;
       .AND. ( source == NIL ;
               .OR. source:player == NIL ;
               .OR. source:player:readyweapon != wp_chainsaw )
        ang := R_PointToAngle2( inflictor:x, inflictor:y, target:x, target:y )

        thrust := Int( damage * Int( FRACUNIT / 8 ) * 100 / target:info:mass )

        IF damage < 40 ;
           .AND. damage > target:health ;
           .AND. target:z - inflictor:z > 64 * FRACUNIT ;
           .AND. ( P_Random() & 1 ) != 0
            ang := ( ( ang + ANG180 ) & 0xFFFFFFFF )
            thrust := thrust * 4
        ENDIF

        ang := UShr( ang, ANGLETOFINESHIFT )
        target:momx += FixedMul( thrust, finecosine[ ang + 1 ] )
        target:momy += FixedMul( thrust, finesine[ ang + 1 ] )
    ENDIF

    IF !( player == NIL )
        IF target:subsector:sector:special == 11 .AND. damage >= target:health
            damage := target:health - 1
        ENDIF

        IF damage < 1000 ;
           .AND. ( ( player:cheats & CF_GODMODE ) != 0 ;
                   .OR. player:powers[ pw_invulnerability + 1 ] != 0 )
            RETURN NIL
        ENDIF

        IF player:armortype != 0
            IF player:armortype == 1
                saved := Int( damage / 3 )
            ELSE
                saved := Int( damage / 2 )
            ENDIF

            IF player:armorpoints <= saved
                saved := player:armorpoints
                player:armortype := 0
            ENDIF
            player:armorpoints -= saved
            damage -= saved
        ENDIF
        player:health -= damage
        IF player:health < 0
            player:health := 0
        ENDIF

        player:attacker := source
        player:damagecount += damage

        IF player:damagecount > 100
            player:damagecount := 100
        ENDIF

        temp := iif( damage < 100, damage, 100 )

        IF player == players[ consoleplayer + 1 ]
            I_Tactile( 40, 10, 40 + temp * 2 )
        ENDIF
    ENDIF

    target:health -= damage
    IF target:health <= 0
        P_KillMobj( source, target )
        RETURN NIL
    ENDIF

    IF P_Random() < target:info:painchance .AND. ( target:flags & MF_SKULLFLY ) == 0
        target:flags := ( target:flags | MF_JUSTHIT )
        P_SetMobjState( target, target:info:painstate )
    ENDIF

    target:reactiontime := 0

    IF ( target:threshold == 0 .OR. target:type == MT_VILE ) ;
       .AND. !( source == NIL ) .AND. !( source == target ) ;
       .AND. source:type != MT_VILE
        target:target := source
        target:threshold := BASETHRESHOLD
        // p_inter.c: only spawnstate (not spawnstate+1).
        IF target:iState == target:info:spawnstate ;
           .AND. target:info:seestate != S_NULL
            P_SetMobjState( target, target:info:seestate )
        ENDIF
    ENDIF
RETURN NIL
