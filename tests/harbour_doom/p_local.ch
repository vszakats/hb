/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __P_LOCAL__
#define __P_LOCAL__

#include "m_fixed.ch"
#include "m_bbox.ch"
#include "d_think.ch"
#include "doomdata.ch"
#include "p_mobj.ch"
#include "d_player.ch"

#ifndef FLOATSPEED
#define FLOATSPEED    ( FRACUNIT * 4 )
#endif
#ifndef MAXHEALTH
#define MAXHEALTH     100
#endif
#ifndef VIEWHEIGHT
#define VIEWHEIGHT    ( 41 * FRACUNIT )
#endif

#define MAPBLOCKUNITS  128
#define MAPBLOCKSIZE   ( MAPBLOCKUNITS * FRACUNIT )
#ifndef MAPBLOCKSHIFT
#define MAPBLOCKSHIFT  ( FRACBITS + 7 )
#endif
#define MAPBMASK       ( MAPBLOCKSIZE - 1 )
#define MAPBTOFRAC     ( MAPBLOCKSHIFT - FRACBITS )

#define PLAYERRADIUS   ( 16 * FRACUNIT )
#ifndef MAXRADIUS
#define MAXRADIUS      ( 32 * FRACUNIT )
#endif
#define GRAVITY        FRACUNIT
#define MAXMOVE        ( 30 * FRACUNIT )

#define USERANGE       ( 64 * FRACUNIT )
#ifndef MELEERANGE
#define MELEERANGE     ( 64 * FRACUNIT )
#endif
#ifndef MISSILERANGE
#define MISSILERANGE   ( 32 * 64 * FRACUNIT )
#endif
#define BASETHRESHOLD  100

#ifndef ONFLOORZ
#define ONFLOORZ    INT_MIN
#define ONCEILINGZ  INT_MAX
#endif

#define ITEMQUESIZE    128

#define MAXINTERCEPTS_ORIGINAL 128
#define MAXINTERCEPTS          ( MAXINTERCEPTS_ORIGINAL + 61 )

#define PT_ADDLINES   1
#define PT_ADDTHINGS  2
#define PT_EARLYOUT   4

#define MAXSPECIALCROSS          20
#define MAXSPECIALCROSS_ORIGINAL 8

#ifndef ST_HORIZONTAL
#define ST_HORIZONTAL 0
#define ST_VERTICAL   1
#define ST_POSITIVE   2
#define ST_NEGATIVE   3
#endif

#ifndef ANGLETOFINESHIFT
#define ANGLETOFINESHIFT 19
#endif

#ifndef ANG45
#define ANG45  0x20000000
#define ANG90  0x40000000
#define ANG180 0x80000000
#define ANG270 0xC0000000
#endif

#endif
