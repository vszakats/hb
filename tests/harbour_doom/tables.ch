/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __TABLES__
#define __TABLES__

#include "m_fixed.ch"

#ifndef FINEANGLES
#define FINEANGLES         8192
#define FINEMASK           ( FINEANGLES - 1 )
#define ANGLETOFINESHIFT   19
#endif

#ifndef ANG45
#define ANG45    0x20000000
#define ANG90    0x40000000
#define ANG180   0x80000000
#define ANG270   0xC0000000
#define ANG_MAX  0xFFFFFFFF
#define ANG1     ( ANG45 / 45 )
#define ANG60    ( ANG180 / 3 )
#define ANG1_X   0x01000000
#endif

#ifndef SLOPERANGE
#define SLOPERANGE  2048
#define SLOPEBITS   11
#define DBITS       ( FRACBITS - SLOPEBITS )
#endif


#endif
