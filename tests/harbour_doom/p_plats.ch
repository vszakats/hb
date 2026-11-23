/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __P_PLATS__
#define __P_PLATS__

#include "d_think.ch"
#include "m_fixed.ch"
#include "p_ceilng.ch"
#include "doomdef.ch"

#ifndef plat_up
#define plat_up        0
#define plat_down      1
#define plat_waiting   2
#define plat_in_stasis 3
#endif

#ifndef perpetualRaise
#define perpetualRaise         0
#define downWaitUpStay         1
#define raiseAndChange         2
#define raiseToNearestAndChange 3
#define blazeDWUS              4
#endif

#ifndef PLATWAIT
#define PLATWAIT  3
#define PLATSPEED FRACUNIT
#define MAXPLATS  30
#endif

#ifndef sfx_pstart
#define sfx_pstart 18
#define sfx_pstop  19
#define sfx_stnmov 22
#endif

#endif
