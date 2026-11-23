/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __D_MODE__
#define __D_MODE__

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

#ifndef exe_doom_1_2
#define exe_doom_1_2      0
#define exe_doom_1_666    1
#define exe_doom_1_7      2
#define exe_doom_1_8      3
#define exe_doom_1_9      4
#define exe_hacx          5
#define exe_ultimate      6
#define exe_final         7
#define exe_final2        8
#define exe_chex          9
#define exe_heretic_1_3   10
#define exe_hexen_1_1     11
#define exe_strife_1_2    12
#define exe_strife_1_31   13
#endif

#ifndef sk_medium
#define sk_noitems    -1
#define sk_baby        0
#define sk_easy        1
#define sk_medium      2
#define sk_hard        3
#define sk_nightmare   4
#endif



#endif
