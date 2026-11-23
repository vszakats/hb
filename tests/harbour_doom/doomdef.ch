/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __DOOMDEF__
#define __DOOMDEF__

/* Original: doomtype.h, i_timer.h, d_mode.h */
#include "d_mode.ch"

#ifndef TICRATE
#define TICRATE 35
#endif

#define DOOM_VERSION 109
#define DOOM_191_VERSION 111

#define RANGECHECK

#ifndef MAXPLAYERS
#define MAXPLAYERS 4
#endif

#ifndef GS_LEVEL
#define GS_LEVEL         0
#define GS_INTERMISSION  1
#define GS_FINALE        2
#define GS_DEMOSCREEN    3
#endif

#ifndef ga_nothing
#define ga_nothing     0
#define ga_loadlevel   1
#define ga_newgame     2
#define ga_loadgame    3
#define ga_savegame    4
#define ga_playdemo    5
#define ga_completed   6
#define ga_victory     7
#define ga_worlddone   8
#define ga_screenshot  9
#endif

#define MTF_EASY    1
#define MTF_NORMAL  2
#define MTF_HARD    4
#define MTF_AMBUSH  8

#ifndef NUMCARDS
#define it_bluecard     0
#define it_yellowcard   1
#define it_redcard      2
#define it_blueskull    3
#define it_yellowskull  4
#define it_redskull     5
#define NUMCARDS        6
#endif

#ifndef NUMWEAPONS
#define wp_fist         0
#define wp_pistol       1
#define wp_shotgun      2
#define wp_chaingun     3
#define wp_missile      4
#define wp_plasma       5
#define wp_bfg          6
#define wp_chainsaw     7
#define wp_supershotgun 8
#define NUMWEAPONS      9
#define wp_nochange     10
#endif

#ifndef NUMAMMO
#define am_clip    0
#define am_shell   1
#define am_cell    2
#define am_misl    3
#define NUMAMMO    4
#define am_noammo  5
#endif

#ifndef NUMPOWERS
#define pw_invulnerability 0
#define pw_strength        1
#define pw_invisibility    2
#define pw_ironfeet        3
#define pw_allmap          4
#define pw_infrared        5
#define NUMPOWERS          6
#endif

#define INVULNTICS ( 30 * TICRATE )
#define INVISTICS  ( 60 * TICRATE )
#define INFRATICS  ( 120 * TICRATE )
#define IRONTICS   ( 60 * TICRATE )

#endif
