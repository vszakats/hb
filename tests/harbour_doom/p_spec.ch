/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __P_SPEC__
#define __P_SPEC__

#include "p_local.ch"
#include "p_ceilng.ch"
#include "p_doors.ch"
#include "p_floor.ch"
#include "p_plats.ch"
#include "p_lights.ch"
#include "doomdata.ch"

#define MO_TELEPORTMAN 14
#define MAXANIMS       32
#define MAXLINEANIMS   64
#define MAXSWITCHES    50
#define MAXBUTTONS     16
#define BUTTONTIME     35

#ifndef sw_top
#define sw_top    0
#define sw_middle 1
#define sw_bottom 2
#endif

#ifndef sfx_swtchn
#define sfx_swtchn 23
#define sfx_swtchx 24
#endif

#endif
