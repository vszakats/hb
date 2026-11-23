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

/* Original: #include "info.h" — converter info.h para info.ch */
#include "d_items.ch"

CLASS weaponinfo_t
    DATA ammo
    DATA upstate
    DATA downstate
    DATA readystate
    DATA atkstate
    DATA flashstate
    METHOD New()
ENDCLASS

#ifndef NUMWEAPONS
#define NUMWEAPONS 9
#define am_clip    0
#define am_shell   1
#define am_cell    2
#define am_misl    3
#define am_noammo  5
#endif

#ifndef S_NULL
#define S_NULL           0
#define S_PUNCH          2
#define S_PUNCHDOWN      3
#define S_PUNCHUP        4
#define S_PUNCH1         5
#define S_PISTOL         10
#define S_PISTOLDOWN     11
#define S_PISTOLUP       12
#define S_PISTOL1        13
#define S_PISTOLFLASH    17
#define S_SGUN           18
#define S_SGUNDOWN       19
#define S_SGUNUP         20
#define S_SGUN1          21
#define S_SGUNFLASH1     30
#define S_DSGUN          32
#define S_DSGUNDOWN      33
#define S_DSGUNUP        34
#define S_DSGUN1         35
#define S_DSGUNFLASH1    47
#define S_CHAIN          49
#define S_CHAINDOWN      50
#define S_CHAINUP        51
#define S_CHAIN1         52
#define S_CHAINFLASH1    55
#define S_MISSILE        57
#define S_MISSILEDOWN    58
#define S_MISSILEUP      59
#define S_MISSILE1       60
#define S_MISSILEFLASH1  63
#define S_SAW            67
#define S_SAWDOWN        69
#define S_SAWUP          70
#define S_SAW1           71
#define S_PLASMA         74
#define S_PLASMADOWN     75
#define S_PLASMAUP       76
#define S_PLASMA1        77
#define S_PLASMAFLASH1   79
#define S_BFG            81
#define S_BFGDOWN        82
#define S_BFGUP          83
#define S_BFG1           84
#define S_BFGFLASH1      88
#endif

METHOD New( ammo, upstate, downstate, readystate, atkstate, flashstate ) CLASS weaponinfo_t
    ::ammo       := iif( ammo == NIL, 0, ammo )
    ::upstate    := iif( upstate == NIL, 0, upstate )
    ::downstate  := iif( downstate == NIL, 0, downstate )
    ::readystate := iif( readystate == NIL, 0, readystate )
    ::atkstate   := iif( atkstate == NIL, 0, atkstate )
    ::flashstate := iif( flashstate == NIL, 0, flashstate )
RETURN Self

INIT PROCEDURE init_d_items

    PUBLIC weaponinfo

    weaponinfo := {}

    /* wp_fist */
    AAdd( weaponinfo, weaponinfo_t():New( am_noammo, S_PUNCHUP, S_PUNCHDOWN, S_PUNCH, S_PUNCH1, S_NULL ) )
    /* wp_pistol */
    AAdd( weaponinfo, weaponinfo_t():New( am_clip, S_PISTOLUP, S_PISTOLDOWN, S_PISTOL, S_PISTOL1, S_PISTOLFLASH ) )
    /* wp_shotgun */
    AAdd( weaponinfo, weaponinfo_t():New( am_shell, S_SGUNUP, S_SGUNDOWN, S_SGUN, S_SGUN1, S_SGUNFLASH1 ) )
    /* wp_chaingun */
    AAdd( weaponinfo, weaponinfo_t():New( am_clip, S_CHAINUP, S_CHAINDOWN, S_CHAIN, S_CHAIN1, S_CHAINFLASH1 ) )
    /* wp_missile */
    AAdd( weaponinfo, weaponinfo_t():New( am_misl, S_MISSILEUP, S_MISSILEDOWN, S_MISSILE, S_MISSILE1, S_MISSILEFLASH1 ) )
    /* wp_plasma */
    AAdd( weaponinfo, weaponinfo_t():New( am_cell, S_PLASMAUP, S_PLASMADOWN, S_PLASMA, S_PLASMA1, S_PLASMAFLASH1 ) )
    /* wp_bfg */
    AAdd( weaponinfo, weaponinfo_t():New( am_cell, S_BFGUP, S_BFGDOWN, S_BFG, S_BFG1, S_BFGFLASH1 ) )
    /* wp_chainsaw */
    AAdd( weaponinfo, weaponinfo_t():New( am_noammo, S_SAWUP, S_SAWDOWN, S_SAW, S_SAW1, S_NULL ) )
    /* wp_supershotgun */
    AAdd( weaponinfo, weaponinfo_t():New( am_shell, S_DSGUNUP, S_DSGUNDOWN, S_DSGUN, S_DSGUN1, S_DSGUNFLASH1 ) )

RETURN
