/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __S_SOUND__
#define __S_SOUND__

#include "p_mobj.ch"
#include "sounds.ch"

#ifndef S_CLIPPING_DIST
#define S_CLIPPING_DIST  ( 1200 * FRACUNIT )
#define S_CLOSE_DIST     ( 200 * FRACUNIT )
#define S_STEREO_SWING   ( 96 * FRACUNIT )
#define NORM_PITCH       128
#define NORM_PRIORITY    64
#define NORM_SEP         128
#endif

#endif
