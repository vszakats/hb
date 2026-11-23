/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __P_DOORS__
#define __P_DOORS__

#include "d_think.ch"
#include "m_fixed.ch"
#include "p_ceilng.ch"
#include "d_player.ch"
#include "dstrings.ch"
#include "deh_str.ch"
#include "doomdef.ch"

#ifndef VDOORSPEED
#define VDOORSPEED  ( FRACUNIT * 2 )
#endif
#ifndef VDOORWAIT
#define VDOORWAIT   150
#endif

#ifndef vld_normal
#define vld_normal          0
#define vld_close30ThenOpen 1
#define vld_close           2
#define vld_open            3
#define vld_raiseIn5Mins    4
#define vld_blazeRaise      5
#define vld_blazeOpen       6
#define vld_blazeClose      7
#endif

#ifndef sfx_doropn
#define sfx_doropn 20
#define sfx_dorcls 21
#define sfx_oof    34
#define sfx_bdopn  88
#define sfx_bdcls  89
#endif

#endif
