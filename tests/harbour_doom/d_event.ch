/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __D_EVENT__
#define __D_EVENT__

/* Original: #include "doomtype.h" — converter doomtype.h para doomtype.ch */

#define ev_keydown    0
#define ev_keyup      1
#define ev_mouse      2
#define ev_joystick   3
#define ev_quit       4

#define BT_ATTACK        1
#define BT_USE           2
#define BT_SPECIAL       128
#define BT_SPECIALMASK   3
#define BT_CHANGE        4
#define BT_WEAPONMASK    ( 8 + 16 + 32 )
#define BT_WEAPONSHIFT   3
#define BTS_PAUSE        1
#define BTS_SAVEGAME     2
#define BTS_SAVEMASK     ( 4 + 8 + 16 )
#define BTS_SAVESHIFT    2

#define BT2_LOOKUP       1
#define BT2_LOOKDOWN     2
#define BT2_CENTERVIEW   4
#define BT2_INVUSE       8
#define BT2_INVDROP      16
#define BT2_JUMP         32
#define BT2_HEALTH       128


#endif
