/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef DEH_STR_H
#define DEH_STR_H

#include "doomfeatures.ch"

#ifdef FEATURE_DEHACKED
#else
#define DEH_String( x ) ( x )
#define DEH_AddStringReplacement( x, y )
#endif

#endif
