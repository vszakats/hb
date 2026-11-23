/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __D_PLAYER__
#define __D_PLAYER__

/* Original: d_items.h, p_pspr.h, p_mobj.h, d_ticcmd.h, net_defs.h */

#ifndef PST_LIVE
#define PST_LIVE   0
#define PST_DEAD   1
#define PST_REBORN 2
#endif

#define CF_NOCLIP     1
#define CF_GODMODE    2
#define CF_NOMOMENTUM 4

#ifndef MAXPLAYERS
#define MAXPLAYERS 4
#endif

#ifndef NUMCARDS
#define it_bluecard     0
#define it_yellowcard   1
#define it_redcard      2
#define it_blueskull    3
#define it_yellowskull  4
#define it_redskull     5
#define NUMCARDS        6
#endif

#ifndef NUMWEAPONS
#define wp_fist         0
#define wp_pistol       1
#define wp_shotgun      2
#define wp_chaingun     3
#define wp_missile      4
#define wp_plasma       5
#define wp_bfg          6
#define wp_chainsaw     7
#define wp_supershotgun 8
#define NUMWEAPONS      9
#define wp_nochange     10
#endif

#ifndef NUMAMMO
#define am_clip    0
#define am_shell   1
#define am_cell    2
#define am_misl    3
#define NUMAMMO    4
#define am_noammo  5
#endif

#ifndef NUMPOWERS
#define pw_invulnerability 0
#define pw_strength        1
#define pw_invisibility    2
#define pw_ironfeet        3
#define pw_allmap          4
#define pw_infrared        5
#define NUMPOWERS          6
#endif

#ifndef NUMPSPRITES
#define ps_weapon   0
#define ps_flash    1
#define NUMPSPRITES 2
#endif

/* CLASS pspdef_t origem: p_pspr.h */




#endif
