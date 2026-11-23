/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __P_FLOOR__
#define __P_FLOOR__

#include "d_think.ch"
#include "m_fixed.ch"
#include "m_bbox.ch"
#include "doomdata.ch"
#include "p_ceilng.ch"

#ifndef FLOORSPEED
#define FLOORSPEED FRACUNIT
#endif

#ifndef lowerFloor
#define lowerFloor            0
#define lowerFloorToLowest    1
#define turboLower            2
#define raiseFloor            3
#define raiseFloorToNearest   4
#define raiseToTexture        5
#define lowerAndChange        6
#define raiseFloor24          7
#define raiseFloor24AndChange 8
#define raiseFloorCrush       9
#define raiseFloorTurbo       10
#define donutRaise            11
#define raiseFloor512         12
#endif

#ifndef build8
#define build8  0
#define turbo16 1
#endif

#endif
