/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __D_MAIN__
#define __D_MAIN__

/* Original: #include "doomdef.h" — converter doomdef.h para doomdef.ch */

#ifndef GS_LEVEL
#define GS_LEVEL         0
#define GS_INTERMISSION  1
#define GS_FINALE        2
#define GS_DEMOSCREEN    3
#endif

#ifndef ga_nothing
#define ga_nothing     0
#define ga_loadlevel   1
#define ga_newgame     2
#define ga_loadgame    3
#define ga_savegame    4
#define ga_playdemo    5
#define ga_completed   6
#define ga_victory     7
#define ga_worlddone   8
#define ga_screenshot  9
#endif

#ifndef MAXPLAYERS
#define MAXPLAYERS 4
#endif


#endif
