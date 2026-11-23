/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __D_IWAD__
#define __D_IWAD__

/* Original: #include "d_mode.h" — converter d_mode.h para d_mode.ch */

#ifndef doom
#define doom       0
#define doom2      1
#define pack_tnt   2
#define pack_plut  3
#define pack_chex  4
#define pack_hacx  5
#define heretic    6
#define hexen      7
#define strife     8
#define none       9
#endif

#ifndef shareware
#define shareware     0
#define registered    1
#define commercial    2
#define retail        3
#define indetermined  4
#endif

/* IWAD_MASK_DOOM = (1<<doom)|(1<<doom2)|(1<<pack_tnt)|(1<<pack_plut)|(1<<pack_chex)|(1<<pack_hacx) */
#define IWAD_MASK_DOOM    63
#define IWAD_MASK_HERETIC 64
#define IWAD_MASK_HEXEN   128
#define IWAD_MASK_STRIFE  256



#endif
