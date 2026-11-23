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

STATIC onground

#include "p_user.ch"
#include "p_local.ch"
#include "p_mobj.ch"
#include "p_pspr.ch"
#include "d_event.ch"
#include "d_player.ch"
#include "doomstat.ch"
#include "doomdef.ch"
#include "info.ch"
#include "d_mode.ch"



STATIC FUNCTION AsU32( n )
RETURN ( n & 0xFFFFFFFF )

PROCEDURE P_Thrust( player, angle, move )
    MEMVAR finecosine
    MEMVAR finesine
    angle := UShr( angle, ANGLETOFINESHIFT )
    player:mo:momx += FixedMul( move, finecosine[ angle + 1 ] )
    player:mo:momy += FixedMul( move, finesine[ angle + 1 ] )
RETURN

PROCEDURE P_CalcHeight( player )
    LOCAL angle
    LOCAL bob
    MEMVAR finesine
    MEMVAR leveltime

    player:bob := FixedMul( player:mo:momx, player:mo:momx ) ;
                + FixedMul( player:mo:momy, player:mo:momy )

    player:bob := Int( player:bob / 4 )

    IF player:bob > MAXBOB
        player:bob := MAXBOB
    ENDIF

    IF ( player:cheats & CF_NOMOMENTUM ) != 0 .OR. ! onground
        player:viewz := player:mo:z + VIEWHEIGHT

        IF player:viewz > player:mo:ceilingz - 4 * FRACUNIT
            player:viewz := player:mo:ceilingz - 4 * FRACUNIT
        ENDIF

        player:viewz := player:mo:z + player:viewheight
        RETURN
    ENDIF

    angle := ( ( Int( FINEANGLES / 20 ) * leveltime ) & FINEMASK )
    bob := FixedMul( Int( player:bob / 2 ), finesine[ angle + 1 ] )

    IF player:playerstate == PST_LIVE
        player:viewheight += player:deltaviewheight

        IF player:viewheight > VIEWHEIGHT
            player:viewheight := VIEWHEIGHT
            player:deltaviewheight := 0
        ENDIF

        IF player:viewheight < Int( VIEWHEIGHT / 2 )
            player:viewheight := Int( VIEWHEIGHT / 2 )
            IF player:deltaviewheight <= 0
                player:deltaviewheight := 1
            ENDIF
        ENDIF

        IF player:deltaviewheight != 0
            player:deltaviewheight += Int( FRACUNIT / 4 )
            IF player:deltaviewheight == 0
                player:deltaviewheight := 1
            ENDIF
        ENDIF
    ENDIF
    player:viewz := player:mo:z + player:viewheight + bob

    IF player:viewz > player:mo:ceilingz - 4 * FRACUNIT
        player:viewz := player:mo:ceilingz - 4 * FRACUNIT
    ENDIF
RETURN

PROCEDURE P_MovePlayer( player )
    LOCAL cmd

    cmd := player:cmd

    player:mo:angle := AsU32( player:mo:angle + ( cmd:angleturn * 65536 ) )

    onground := ( player:mo:z <= player:mo:floorz )

    IF cmd:forwardmove != 0 .AND. onground
        P_Thrust( player, player:mo:angle, cmd:forwardmove * 2048 )
    ENDIF

    IF cmd:sidemove != 0 .AND. onground
        P_Thrust( player, AsU32( player:mo:angle - ANG90 ), cmd:sidemove * 2048 )
    ENDIF

    IF ( cmd:forwardmove != 0 .OR. cmd:sidemove != 0 ) ;
         .AND. player:mo:iState == S_PLAY
        P_SetMobjState( player:mo, S_PLAY_RUN1 )
    ENDIF
RETURN

PROCEDURE P_DeathThink( player )
    LOCAL angle
    LOCAL delta

    P_MovePsprites( player )

    IF player:viewheight > 6 * FRACUNIT
        player:viewheight -= FRACUNIT
    ENDIF

    IF player:viewheight < 6 * FRACUNIT
        player:viewheight := 6 * FRACUNIT
    ENDIF

    player:deltaviewheight := 0
    onground := ( player:mo:z <= player:mo:floorz )
    P_CalcHeight( player )

    IF !( player:attacker == NIL ) .AND. !( player:attacker == player:mo )
        angle := R_PointToAngle2( player:mo:x, ;
                                  player:mo:y, ;
                                  player:attacker:x, ;
                                  player:attacker:y )

        delta := AsU32( angle - player:mo:angle )

        IF delta < ANG5 .OR. delta > AsU32( 0 - ANG5 )
            player:mo:angle := angle

            IF player:damagecount != 0
                player:damagecount--
            ENDIF
        ELSEIF delta < ANG180
            player:mo:angle := AsU32( player:mo:angle + ANG5 )
        ELSE
            player:mo:angle := AsU32( player:mo:angle - ANG5 )
        ENDIF
    ELSEIF player:damagecount != 0
        player:damagecount--
    ENDIF

    IF ( player:cmd:buttons & BT_USE ) != 0
        player:playerstate := PST_REBORN
    ENDIF
RETURN

PROCEDURE P_PlayerThink( player )
    LOCAL cmd
    LOCAL newweapon
    MEMVAR gamemode

    IF ( player:cheats & CF_NOCLIP ) != 0
        player:mo:flags := ( player:mo:flags | MF_NOCLIP )
    ELSE
        player:mo:flags := ( player:mo:flags & ( MF_NOCLIP ^^ 0xFFFFFFFF ) )
    ENDIF

    cmd := player:cmd
    IF ( player:mo:flags & MF_JUSTATTACKED ) != 0
        cmd:angleturn := 0
        cmd:forwardmove := Int( 0xc800 / 512 )
        cmd:sidemove := 0
        player:mo:flags := ( player:mo:flags & ( MF_JUSTATTACKED ^^ 0xFFFFFFFF ) )
    ENDIF

    IF player:playerstate == PST_DEAD
        P_DeathThink( player )
        RETURN
    ENDIF

    IF player:mo:reactiontime != 0
        player:mo:reactiontime--
    ELSE
        P_MovePlayer( player )
    ENDIF

    P_CalcHeight( player )

    IF player:mo:subsector:sector:special != 0
        P_PlayerInSpecialSector( player )
    ENDIF

    IF ( cmd:buttons & BT_SPECIAL ) != 0
        cmd:buttons := 0
    ENDIF

    IF ( cmd:buttons & BT_CHANGE ) != 0
        newweapon := UShr( ( cmd:buttons & BT_WEAPONMASK ), BT_WEAPONSHIFT )

        IF newweapon == wp_fist ;
             .AND. player:weaponowned[ wp_chainsaw + 1 ] ;
             .AND. ! ( player:readyweapon == wp_chainsaw ;
                       .AND. player:powers[ pw_strength + 1 ] != 0 )
            newweapon := wp_chainsaw
        ENDIF

        IF gamemode == commercial ;
             .AND. newweapon == wp_shotgun ;
             .AND. player:weaponowned[ wp_supershotgun + 1 ] ;
             .AND. player:readyweapon != wp_supershotgun
            newweapon := wp_supershotgun
        ENDIF

        IF player:weaponowned[ newweapon + 1 ] ;
             .AND. newweapon != player:readyweapon
            IF ( newweapon != wp_plasma ;
                 .AND. newweapon != wp_bfg ) ;
                 .OR. gamemode != shareware
                player:pendingweapon := newweapon
            ENDIF
        ENDIF
    ENDIF

    IF ( cmd:buttons & BT_USE ) != 0
        IF ! player:usedown
            P_UseLines( player )
            player:usedown := .T.
        ENDIF
    ELSE
        player:usedown := .F.
    ENDIF

    P_MovePsprites( player )

    IF player:powers[ pw_strength + 1 ] != 0
        player:powers[ pw_strength + 1 ]++
    ENDIF

    IF player:powers[ pw_invulnerability + 1 ] != 0
        player:powers[ pw_invulnerability + 1 ]--
    ENDIF

    IF player:powers[ pw_invisibility + 1 ] != 0
        player:powers[ pw_invisibility + 1 ]--
        IF player:powers[ pw_invisibility + 1 ] == 0
            player:mo:flags := ( player:mo:flags & ( MF_SHADOW ^^ 0xFFFFFFFF ) )
        ENDIF
    ENDIF

    IF player:powers[ pw_infrared + 1 ] != 0
        player:powers[ pw_infrared + 1 ]--
    ENDIF

    IF player:powers[ pw_ironfeet + 1 ] != 0
        player:powers[ pw_ironfeet + 1 ]--
    ENDIF

    IF player:damagecount != 0
        player:damagecount--
    ENDIF

    IF player:bonuscount != 0
        player:bonuscount--
    ENDIF

    IF player:powers[ pw_invulnerability + 1 ] != 0
        IF player:powers[ pw_invulnerability + 1 ] > 4 * 32 ;
             .OR. ( player:powers[ pw_invulnerability + 1 ] & 8 ) != 0
            player:fixedcolormap := INVERSECOLORMAP
        ELSE
            player:fixedcolormap := 0
        ENDIF
    ELSEIF player:powers[ pw_infrared + 1 ] != 0
        IF player:powers[ pw_infrared + 1 ] > 4 * 32 ;
             .OR. ( player:powers[ pw_infrared + 1 ] & 8 ) != 0
            player:fixedcolormap := 1
        ELSE
            player:fixedcolormap := 0
        ENDIF
    ELSE
        player:fixedcolormap := 0
    ENDIF
RETURN

INIT PROCEDURE init_p_user
    onground := .F.
RETURN
