/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __SOUNDS__
#define __SOUNDS__

#include "i_sound.ch"

#ifndef mus_None
#define mus_None  0
#endif
#ifndef mus_e1m1
#define mus_e1m1  1
#endif
#ifndef mus_e1m2
#define mus_e1m2  2
#endif
#ifndef mus_e1m3
#define mus_e1m3  3
#endif
#ifndef mus_e1m4
#define mus_e1m4  4
#endif
#ifndef mus_e1m5
#define mus_e1m5  5
#endif
#ifndef mus_e1m6
#define mus_e1m6  6
#endif
#ifndef mus_e1m7
#define mus_e1m7  7
#endif
#ifndef mus_e1m8
#define mus_e1m8  8
#endif
#ifndef mus_e1m9
#define mus_e1m9  9
#endif
#ifndef mus_e2m1
#define mus_e2m1  10
#endif
#ifndef mus_e2m2
#define mus_e2m2  11
#endif
#ifndef mus_e2m3
#define mus_e2m3  12
#endif
#ifndef mus_e2m4
#define mus_e2m4  13
#endif
#ifndef mus_e2m5
#define mus_e2m5  14
#endif
#ifndef mus_e2m6
#define mus_e2m6  15
#endif
#ifndef mus_e2m7
#define mus_e2m7  16
#endif
#ifndef mus_e2m8
#define mus_e2m8  17
#endif
#ifndef mus_e2m9
#define mus_e2m9  18
#endif
#ifndef mus_e3m1
#define mus_e3m1  19
#endif
#ifndef mus_e3m2
#define mus_e3m2  20
#endif
#ifndef mus_e3m3
#define mus_e3m3  21
#endif
#ifndef mus_e3m4
#define mus_e3m4  22
#endif
#ifndef mus_e3m5
#define mus_e3m5  23
#endif
#ifndef mus_e3m6
#define mus_e3m6  24
#endif
#ifndef mus_e3m7
#define mus_e3m7  25
#endif
#ifndef mus_e3m8
#define mus_e3m8  26
#endif
#ifndef mus_e3m9
#define mus_e3m9  27
#endif
#ifndef mus_inter
#define mus_inter  28
#endif
#ifndef mus_intro
#define mus_intro  29
#endif
#ifndef mus_bunny
#define mus_bunny  30
#endif
#ifndef mus_victor
#define mus_victor  31
#endif
#ifndef mus_introa
#define mus_introa  32
#endif
#ifndef mus_runnin
#define mus_runnin  33
#endif
#ifndef mus_stalks
#define mus_stalks  34
#endif
#ifndef mus_countd
#define mus_countd  35
#endif
#ifndef mus_betwee
#define mus_betwee  36
#endif
#ifndef mus_doom
#define mus_doom  37
#endif
#ifndef mus_the_da
#define mus_the_da  38
#endif
#ifndef mus_shawn
#define mus_shawn  39
#endif
#ifndef mus_ddtblu
#define mus_ddtblu  40
#endif
#ifndef mus_in_cit
#define mus_in_cit  41
#endif
#ifndef mus_dead
#define mus_dead  42
#endif
#ifndef mus_stlks2
#define mus_stlks2  43
#endif
#ifndef mus_theda2
#define mus_theda2  44
#endif
#ifndef mus_doom2
#define mus_doom2  45
#endif
#ifndef mus_ddtbl2
#define mus_ddtbl2  46
#endif
#ifndef mus_runni2
#define mus_runni2  47
#endif
#ifndef mus_dead2
#define mus_dead2  48
#endif
#ifndef mus_stlks3
#define mus_stlks3  49
#endif
#ifndef mus_romero
#define mus_romero  50
#endif
#ifndef mus_shawn2
#define mus_shawn2  51
#endif
#ifndef mus_messag
#define mus_messag  52
#endif
#ifndef mus_count2
#define mus_count2  53
#endif
#ifndef mus_ddtbl3
#define mus_ddtbl3  54
#endif
#ifndef mus_ampie
#define mus_ampie  55
#endif
#ifndef mus_theda3
#define mus_theda3  56
#endif
#ifndef mus_adrian
#define mus_adrian  57
#endif
#ifndef mus_messg2
#define mus_messg2  58
#endif
#ifndef mus_romer2
#define mus_romer2  59
#endif
#ifndef mus_tense
#define mus_tense  60
#endif
#ifndef mus_shawn3
#define mus_shawn3  61
#endif
#ifndef mus_openin
#define mus_openin  62
#endif
#ifndef mus_evil
#define mus_evil  63
#endif
#ifndef mus_ultima
#define mus_ultima  64
#endif
#ifndef mus_read_m
#define mus_read_m  65
#endif
#ifndef mus_dm2ttl
#define mus_dm2ttl  66
#endif
#ifndef mus_dm2int
#define mus_dm2int  67
#endif
#ifndef NUMMUSIC
#define NUMMUSIC  68
#endif

#ifndef sfx_None
#define sfx_None  0
#endif
#ifndef sfx_pistol
#define sfx_pistol  1
#endif
#ifndef sfx_shotgn
#define sfx_shotgn  2
#endif
#ifndef sfx_sgcock
#define sfx_sgcock  3
#endif
#ifndef sfx_dshtgn
#define sfx_dshtgn  4
#endif
#ifndef sfx_dbopn
#define sfx_dbopn  5
#endif
#ifndef sfx_dbcls
#define sfx_dbcls  6
#endif
#ifndef sfx_dbload
#define sfx_dbload  7
#endif
#ifndef sfx_plasma
#define sfx_plasma  8
#endif
#ifndef sfx_bfg
#define sfx_bfg  9
#endif
#ifndef sfx_sawup
#define sfx_sawup  10
#endif
#ifndef sfx_sawidl
#define sfx_sawidl  11
#endif
#ifndef sfx_sawful
#define sfx_sawful  12
#endif
#ifndef sfx_sawhit
#define sfx_sawhit  13
#endif
#ifndef sfx_rlaunc
#define sfx_rlaunc  14
#endif
#ifndef sfx_rxplod
#define sfx_rxplod  15
#endif
#ifndef sfx_firsht
#define sfx_firsht  16
#endif
#ifndef sfx_firxpl
#define sfx_firxpl  17
#endif
#ifndef sfx_pstart
#define sfx_pstart  18
#endif
#ifndef sfx_pstop
#define sfx_pstop  19
#endif
#ifndef sfx_doropn
#define sfx_doropn  20
#endif
#ifndef sfx_dorcls
#define sfx_dorcls  21
#endif
#ifndef sfx_stnmov
#define sfx_stnmov  22
#endif
#ifndef sfx_swtchn
#define sfx_swtchn  23
#endif
#ifndef sfx_swtchx
#define sfx_swtchx  24
#endif
#ifndef sfx_plpain
#define sfx_plpain  25
#endif
#ifndef sfx_dmpain
#define sfx_dmpain  26
#endif
#ifndef sfx_popain
#define sfx_popain  27
#endif
#ifndef sfx_vipain
#define sfx_vipain  28
#endif
#ifndef sfx_mnpain
#define sfx_mnpain  29
#endif
#ifndef sfx_pepain
#define sfx_pepain  30
#endif
#ifndef sfx_slop
#define sfx_slop  31
#endif
#ifndef sfx_itemup
#define sfx_itemup  32
#endif
#ifndef sfx_wpnup
#define sfx_wpnup  33
#endif
#ifndef sfx_oof
#define sfx_oof  34
#endif
#ifndef sfx_telept
#define sfx_telept  35
#endif
#ifndef sfx_posit1
#define sfx_posit1  36
#endif
#ifndef sfx_posit2
#define sfx_posit2  37
#endif
#ifndef sfx_posit3
#define sfx_posit3  38
#endif
#ifndef sfx_bgsit1
#define sfx_bgsit1  39
#endif
#ifndef sfx_bgsit2
#define sfx_bgsit2  40
#endif
#ifndef sfx_sgtsit
#define sfx_sgtsit  41
#endif
#ifndef sfx_cacsit
#define sfx_cacsit  42
#endif
#ifndef sfx_brssit
#define sfx_brssit  43
#endif
#ifndef sfx_cybsit
#define sfx_cybsit  44
#endif
#ifndef sfx_spisit
#define sfx_spisit  45
#endif
#ifndef sfx_bspsit
#define sfx_bspsit  46
#endif
#ifndef sfx_kntsit
#define sfx_kntsit  47
#endif
#ifndef sfx_vilsit
#define sfx_vilsit  48
#endif
#ifndef sfx_mansit
#define sfx_mansit  49
#endif
#ifndef sfx_pesit
#define sfx_pesit  50
#endif
#ifndef sfx_sklatk
#define sfx_sklatk  51
#endif
#ifndef sfx_sgtatk
#define sfx_sgtatk  52
#endif
#ifndef sfx_skepch
#define sfx_skepch  53
#endif
#ifndef sfx_vilatk
#define sfx_vilatk  54
#endif
#ifndef sfx_claw
#define sfx_claw  55
#endif
#ifndef sfx_skeswg
#define sfx_skeswg  56
#endif
#ifndef sfx_pldeth
#define sfx_pldeth  57
#endif
#ifndef sfx_pdiehi
#define sfx_pdiehi  58
#endif
#ifndef sfx_podth1
#define sfx_podth1  59
#endif
#ifndef sfx_podth2
#define sfx_podth2  60
#endif
#ifndef sfx_podth3
#define sfx_podth3  61
#endif
#ifndef sfx_bgdth1
#define sfx_bgdth1  62
#endif
#ifndef sfx_bgdth2
#define sfx_bgdth2  63
#endif
#ifndef sfx_sgtdth
#define sfx_sgtdth  64
#endif
#ifndef sfx_cacdth
#define sfx_cacdth  65
#endif
#ifndef sfx_skldth
#define sfx_skldth  66
#endif
#ifndef sfx_brsdth
#define sfx_brsdth  67
#endif
#ifndef sfx_cybdth
#define sfx_cybdth  68
#endif
#ifndef sfx_spidth
#define sfx_spidth  69
#endif
#ifndef sfx_bspdth
#define sfx_bspdth  70
#endif
#ifndef sfx_vildth
#define sfx_vildth  71
#endif
#ifndef sfx_kntdth
#define sfx_kntdth  72
#endif
#ifndef sfx_pedth
#define sfx_pedth  73
#endif
#ifndef sfx_skedth
#define sfx_skedth  74
#endif
#ifndef sfx_posact
#define sfx_posact  75
#endif
#ifndef sfx_bgact
#define sfx_bgact  76
#endif
#ifndef sfx_dmact
#define sfx_dmact  77
#endif
#ifndef sfx_bspact
#define sfx_bspact  78
#endif
#ifndef sfx_bspwlk
#define sfx_bspwlk  79
#endif
#ifndef sfx_vilact
#define sfx_vilact  80
#endif
#ifndef sfx_noway
#define sfx_noway  81
#endif
#ifndef sfx_barexp
#define sfx_barexp  82
#endif
#ifndef sfx_punch
#define sfx_punch  83
#endif
#ifndef sfx_hoof
#define sfx_hoof  84
#endif
#ifndef sfx_metal
#define sfx_metal  85
#endif
#ifndef sfx_chgun
#define sfx_chgun  86
#endif
#ifndef sfx_tink
#define sfx_tink  87
#endif
#ifndef sfx_bdopn
#define sfx_bdopn  88
#endif
#ifndef sfx_bdcls
#define sfx_bdcls  89
#endif
#ifndef sfx_itmbk
#define sfx_itmbk  90
#endif
#ifndef sfx_flame
#define sfx_flame  91
#endif
#ifndef sfx_flamst
#define sfx_flamst  92
#endif
#ifndef sfx_getpow
#define sfx_getpow  93
#endif
#ifndef sfx_bospit
#define sfx_bospit  94
#endif
#ifndef sfx_boscub
#define sfx_boscub  95
#endif
#ifndef sfx_bossit
#define sfx_bossit  96
#endif
#ifndef sfx_bospn
#define sfx_bospn  97
#endif
#ifndef sfx_bosdth
#define sfx_bosdth  98
#endif
#ifndef sfx_manatk
#define sfx_manatk  99
#endif
#ifndef sfx_mandth
#define sfx_mandth  100
#endif
#ifndef sfx_sssit
#define sfx_sssit  101
#endif
#ifndef sfx_ssdth
#define sfx_ssdth  102
#endif
#ifndef sfx_keenpn
#define sfx_keenpn  103
#endif
#ifndef sfx_keendt
#define sfx_keendt  104
#endif
#ifndef sfx_skeact
#define sfx_skeact  105
#endif
#ifndef sfx_skesit
#define sfx_skesit  106
#endif
#ifndef sfx_skeatk
#define sfx_skeatk  107
#endif
#ifndef sfx_radio
#define sfx_radio  108
#endif
#ifndef NUMSFX
#define NUMSFX  109
#endif


#endif
