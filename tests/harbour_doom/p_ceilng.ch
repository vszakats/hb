/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __P_CEILNG__
#define __P_CEILNG__

#include "d_think.ch"
#include "m_fixed.ch"

#ifndef CEILSPEED
#define CEILSPEED  FRACUNIT
#endif
#ifndef CEILWAIT
#define CEILWAIT   150
#endif
#ifndef MAXCEILINGS
#define MAXCEILINGS 30
#endif

#ifndef lowerToFloor
#define lowerToFloor        0
#define raiseToHighest      1
#define lowerAndCrush       2
#define crushAndRaise       3
#define fastCrushAndRaise   4
#define silentCrushAndRaise 5
#endif

#ifndef result_ok
#define result_ok       0
#define result_crushed  1
#define result_pastdest 2
#endif

#ifndef sfx_pstop
#define sfx_pstop  19
#define sfx_stnmov 22
#endif

#endif
