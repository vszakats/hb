/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __STSTUFF_H__
#define __STSTUFF_H__

#include "doomtype.ch"
#include "d_event.ch"
#include "m_cheat.ch"
#include "i_video.ch"

#define ST_HEIGHT  32
#define ST_WIDTH   SCREENWIDTH
#define ST_Y       ( SCREENHEIGHT - ST_HEIGHT )

#define AutomapState     0
#define FirstPersonState 1

#define StartChatState   0
#define WaitDestState    1
#define GetChatState     2


#endif
