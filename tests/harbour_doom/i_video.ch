/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __I_VIDEO__
#define __I_VIDEO__

#include "doomtype.ch"

#ifndef SCREENWIDTH
#define SCREENWIDTH       320
#define SCREENHEIGHT      200
#define SCREENWIDTH_4_3   256
#define SCREENHEIGHT_4_3  240
#endif

/* .T. = I_SetPalette / I_FinishUpdate em Harbour. Ou: Doom_hb.exe -videoc */
#ifndef I_VIDEO_HARBOUR
#define I_VIDEO_HARBOUR   .T.
#endif

#define MAX_MOUSE_BUTTONS 8

#endif
