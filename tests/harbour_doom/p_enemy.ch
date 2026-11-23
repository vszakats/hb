/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __P_ENEMY__
#define __P_ENEMY__

#include "info.ch"
#include "doomstat.ch"
#include "doomdata.ch"
#include "m_fixed.ch"
#include "p_doors.ch"

#ifndef DI_EAST
#define DI_EAST      0
#define DI_NORTHEAST 1
#define DI_NORTH     2
#define DI_NORTHWEST 3
#define DI_WEST      4
#define DI_SOUTHWEST 5
#define DI_SOUTH     6
#define DI_SOUTHEAST 7
#define DI_NODIR     8
#define NUMDIRS      9
#endif

#ifndef FLOATSPEED
#define FLOATSPEED    ( FRACUNIT * 4 )
#endif
#ifndef MAXRADIUS
#define MAXRADIUS     ( 32 * FRACUNIT )
#endif
#ifndef MELEERANGE
#define MELEERANGE    ( 64 * FRACUNIT )
#endif
#ifndef MISSILERANGE
#define MISSILERANGE  ( 32 * 64 * FRACUNIT )
#endif
#ifndef MAPBLOCKSHIFT
#define MAPBLOCKSHIFT 23
#endif
#ifndef ANGLETOFINESHIFT
#define ANGLETOFINESHIFT 19
#endif

#ifndef ANG90
#define ANG45  0x20000000
#define ANG90  0x40000000
#define ANG180 0x80000000
#define ANG270 0xC0000000
#endif

#ifndef FATSPREAD
#define FATSPREAD  ( ANG90 / 8 )
#endif
#ifndef SKULLSPEED
#define SKULLSPEED ( 20 * FRACUNIT )
#endif
#ifndef TRACEANGLE
#define TRACEANGLE 0xC000000
#endif

#ifndef lowerFloor
#define lowerFloor            0
#define lowerFloorToLowest    1
#define turboLower            2
#define raiseFloor            3
#define raiseFloorToNearest   4
#define raiseToTexture        5
#endif

#ifndef MF_SHOOTABLE
#define MF_SHOOTABLE     4
#endif
#ifndef MF_AMBUSH
#define MF_AMBUSH        32
#endif
#ifndef MF_JUSTHIT
#define MF_JUSTHIT       64
#endif
#ifndef MF_JUSTATTACKED
#define MF_JUSTATTACKED  128
#endif
#ifndef MF_SOLID
#define MF_SOLID         2
#endif
#ifndef MF_FLOAT
#define MF_FLOAT         0x4000
#endif
#ifndef MF_SHADOW
#define MF_SHADOW        0x40000
#endif
#ifndef MF_CORPSE
#define MF_CORPSE        0x100000
#endif
#ifndef MF_INFLOAT
#define MF_INFLOAT       0x200000
#endif
#ifndef MF_SKULLFLY
#define MF_SKULLFLY      0x1000000
#endif

#ifndef sfx_pistol
#define sfx_pistol  1
#define sfx_shotgn  2
#define sfx_dbopn   5
#define sfx_dbcls   6
#define sfx_dbload  7
#define sfx_slop    31
#define sfx_telept  35
#define sfx_posit1  36
#define sfx_posit2  37
#define sfx_posit3  38
#define sfx_bgsit1  39
#define sfx_bgsit2  40
#define sfx_skepch  53
#define sfx_vilatk  54
#define sfx_claw    55
#define sfx_skeswg  56
#define sfx_pldeth  57
#define sfx_pdiehi  58
#define sfx_podth1  59
#define sfx_podth2  60
#define sfx_podth3  61
#define sfx_bgdth1  62
#define sfx_bgdth2  63
#define sfx_bspwlk  79
#define sfx_barexp  82
#define sfx_hoof    84
#define sfx_metal   85
#define sfx_flame   91
#define sfx_flamst  92
#define sfx_bospit  94
#define sfx_boscub  95
#define sfx_bossit  96
#define sfx_bospn   97
#define sfx_bosdth  98
#define sfx_manatk  99
#endif

#endif
