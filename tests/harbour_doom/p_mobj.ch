/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __P_MOBJ__
#define __P_MOBJ__

#include "d_think.ch"
#include "m_fixed.ch"
#include "doomdata.ch"
#include "info.ch"

#ifndef MF_SPECIAL
#define MF_SPECIAL       1
#define MF_SOLID         2
#define MF_SHOOTABLE     4
#define MF_NOSECTOR      8
#define MF_NOBLOCKMAP    16
#define MF_AMBUSH        32
#define MF_JUSTHIT       64
#define MF_JUSTATTACKED  128
#define MF_SPAWNCEILING  256
#define MF_NOGRAVITY     512
#define MF_DROPOFF       0x400
#define MF_PICKUP        0x800
#define MF_NOCLIP        0x1000
#define MF_SLIDE         0x2000
#define MF_FLOAT         0x4000
#define MF_TELEPORT      0x8000
#define MF_MISSILE       0x10000
#define MF_DROPPED       0x20000
#define MF_SHADOW        0x40000
#define MF_NOBLOOD       0x80000
#define MF_CORPSE        0x100000
#define MF_INFLOAT       0x200000
#define MF_COUNTKILL     0x400000
#define MF_COUNTITEM     0x800000
#define MF_SKULLFLY      0x1000000
#define MF_NOTDMATCH     0x2000000
#define MF_TRANSLATION   0xc000000
#define MF_TRANSSHIFT    26
#endif

#ifndef STOPSPEED
#define STOPSPEED  0x1000
#define FRICTION   0xe800
#endif

#ifndef sfx_oof
#define sfx_oof    34
#define sfx_telept 35
#define sfx_itmbk  90
#endif


#endif
