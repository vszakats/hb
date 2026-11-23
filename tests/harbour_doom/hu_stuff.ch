/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __HU_STUFF_H__
#define __HU_STUFF_H__

#include "d_event.ch"

#ifndef TICRATE
#define TICRATE 35
#endif

#define HU_FONTSTART  33
#define HU_FONTEND    95
#define HU_FONTSIZE   ( HU_FONTEND - HU_FONTSTART + 1 )

#define HU_BROADCAST  5

#define HU_MSGX       0
#define HU_MSGY       0
#define HU_MSGWIDTH   64
#define HU_MSGHEIGHT  1

#define HU_MSGTIMEOUT ( 4 * TICRATE )


#endif
