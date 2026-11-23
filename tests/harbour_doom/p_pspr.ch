/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __P_PSPR__
#define __P_PSPR__

#include "m_fixed.ch"
#include "d_player.ch"
#include "info.ch"

#define FF_FULLBRIGHT  0x8000
#define FF_FRAMEMASK   0x7fff

#ifndef LOWERSPEED
#define LOWERSPEED    ( FRACUNIT * 6 )
#define RAISESPEED    ( FRACUNIT * 6 )
#define WEAPONBOTTOM  ( 128 * FRACUNIT )
#define WEAPONTOP     ( 32 * FRACUNIT )
#endif

#ifndef FINEANGLES
#define FINEANGLES 8192
#define FINEMASK   8191
#endif

#ifndef sfx_pistol
#define sfx_pistol  1
#define sfx_shotgn  2
#define sfx_dshtgn  4
#define sfx_bfg     9
#define sfx_sawup   10
#define sfx_sawidl  11
#define sfx_sawful  12
#define sfx_sawhit  13
#define sfx_punch   83
#endif


#endif
