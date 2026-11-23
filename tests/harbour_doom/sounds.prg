/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

#include "sounds.ch"


STATIC FUNCTION NewMusic( cName )
    LOCAL o := musicinfo_t():New()
    o:name := cName
    o:lumpnum := 0
    o:data := NIL
    o:handle := NIL
RETURN o

STATIC FUNCTION NewSound( cName, nPri )
    LOCAL o := sfxinfo_t():New()
    o:tagname := ""
    o:name := cName
    o:priority := nPri
    o:link := NIL
    o:pitch := -1
    o:volume := -1
    o:usefulness := 0
    o:lumpnum := -1
    o:numchannels := 0
    o:driver_data := NIL
RETURN o

INIT PROCEDURE init_sounds
    PUBLIC S_sfx
    PUBLIC S_music

    S_music := {}
    AAdd( S_music, NewMusic( "" ) )
    AAdd( S_music, NewMusic( "e1m1" ) )
    AAdd( S_music, NewMusic( "e1m2" ) )
    AAdd( S_music, NewMusic( "e1m3" ) )
    AAdd( S_music, NewMusic( "e1m4" ) )
    AAdd( S_music, NewMusic( "e1m5" ) )
    AAdd( S_music, NewMusic( "e1m6" ) )
    AAdd( S_music, NewMusic( "e1m7" ) )
    AAdd( S_music, NewMusic( "e1m8" ) )
    AAdd( S_music, NewMusic( "e1m9" ) )
    AAdd( S_music, NewMusic( "e2m1" ) )
    AAdd( S_music, NewMusic( "e2m2" ) )
    AAdd( S_music, NewMusic( "e2m3" ) )
    AAdd( S_music, NewMusic( "e2m4" ) )
    AAdd( S_music, NewMusic( "e2m5" ) )
    AAdd( S_music, NewMusic( "e2m6" ) )
    AAdd( S_music, NewMusic( "e2m7" ) )
    AAdd( S_music, NewMusic( "e2m8" ) )
    AAdd( S_music, NewMusic( "e2m9" ) )
    AAdd( S_music, NewMusic( "e3m1" ) )
    AAdd( S_music, NewMusic( "e3m2" ) )
    AAdd( S_music, NewMusic( "e3m3" ) )
    AAdd( S_music, NewMusic( "e3m4" ) )
    AAdd( S_music, NewMusic( "e3m5" ) )
    AAdd( S_music, NewMusic( "e3m6" ) )
    AAdd( S_music, NewMusic( "e3m7" ) )
    AAdd( S_music, NewMusic( "e3m8" ) )
    AAdd( S_music, NewMusic( "e3m9" ) )
    AAdd( S_music, NewMusic( "inter" ) )
    AAdd( S_music, NewMusic( "intro" ) )
    AAdd( S_music, NewMusic( "bunny" ) )
    AAdd( S_music, NewMusic( "victor" ) )
    AAdd( S_music, NewMusic( "introa" ) )
    AAdd( S_music, NewMusic( "runnin" ) )
    AAdd( S_music, NewMusic( "stalks" ) )
    AAdd( S_music, NewMusic( "countd" ) )
    AAdd( S_music, NewMusic( "betwee" ) )
    AAdd( S_music, NewMusic( "doom" ) )
    AAdd( S_music, NewMusic( "the_da" ) )
    AAdd( S_music, NewMusic( "shawn" ) )
    AAdd( S_music, NewMusic( "ddtblu" ) )
    AAdd( S_music, NewMusic( "in_cit" ) )
    AAdd( S_music, NewMusic( "dead" ) )
    AAdd( S_music, NewMusic( "stlks2" ) )
    AAdd( S_music, NewMusic( "theda2" ) )
    AAdd( S_music, NewMusic( "doom2" ) )
    AAdd( S_music, NewMusic( "ddtbl2" ) )
    AAdd( S_music, NewMusic( "runni2" ) )
    AAdd( S_music, NewMusic( "dead2" ) )
    AAdd( S_music, NewMusic( "stlks3" ) )
    AAdd( S_music, NewMusic( "romero" ) )
    AAdd( S_music, NewMusic( "shawn2" ) )
    AAdd( S_music, NewMusic( "messag" ) )
    AAdd( S_music, NewMusic( "count2" ) )
    AAdd( S_music, NewMusic( "ddtbl3" ) )
    AAdd( S_music, NewMusic( "ampie" ) )
    AAdd( S_music, NewMusic( "theda3" ) )
    AAdd( S_music, NewMusic( "adrian" ) )
    AAdd( S_music, NewMusic( "messg2" ) )
    AAdd( S_music, NewMusic( "romer2" ) )
    AAdd( S_music, NewMusic( "tense" ) )
    AAdd( S_music, NewMusic( "shawn3" ) )
    AAdd( S_music, NewMusic( "openin" ) )
    AAdd( S_music, NewMusic( "evil" ) )
    AAdd( S_music, NewMusic( "ultima" ) )
    AAdd( S_music, NewMusic( "read_m" ) )
    AAdd( S_music, NewMusic( "dm2ttl" ) )
    AAdd( S_music, NewMusic( "dm2int" ) )

    S_sfx := {}
    AAdd( S_sfx, NewSound( "none", 0 ) )
    AAdd( S_sfx, NewSound( "pistol", 64 ) )
    AAdd( S_sfx, NewSound( "shotgn", 64 ) )
    AAdd( S_sfx, NewSound( "sgcock", 64 ) )
    AAdd( S_sfx, NewSound( "dshtgn", 64 ) )
    AAdd( S_sfx, NewSound( "dbopn", 64 ) )
    AAdd( S_sfx, NewSound( "dbcls", 64 ) )
    AAdd( S_sfx, NewSound( "dbload", 64 ) )
    AAdd( S_sfx, NewSound( "plasma", 64 ) )
    AAdd( S_sfx, NewSound( "bfg", 64 ) )
    AAdd( S_sfx, NewSound( "sawup", 64 ) )
    AAdd( S_sfx, NewSound( "sawidl", 118 ) )
    AAdd( S_sfx, NewSound( "sawful", 64 ) )
    AAdd( S_sfx, NewSound( "sawhit", 64 ) )
    AAdd( S_sfx, NewSound( "rlaunc", 64 ) )
    AAdd( S_sfx, NewSound( "rxplod", 70 ) )
    AAdd( S_sfx, NewSound( "firsht", 70 ) )
    AAdd( S_sfx, NewSound( "firxpl", 70 ) )
    AAdd( S_sfx, NewSound( "pstart", 100 ) )
    AAdd( S_sfx, NewSound( "pstop", 100 ) )
    AAdd( S_sfx, NewSound( "doropn", 100 ) )
    AAdd( S_sfx, NewSound( "dorcls", 100 ) )
    AAdd( S_sfx, NewSound( "stnmov", 119 ) )
    AAdd( S_sfx, NewSound( "swtchn", 78 ) )
    AAdd( S_sfx, NewSound( "swtchx", 78 ) )
    AAdd( S_sfx, NewSound( "plpain", 96 ) )
    AAdd( S_sfx, NewSound( "dmpain", 96 ) )
    AAdd( S_sfx, NewSound( "popain", 96 ) )
    AAdd( S_sfx, NewSound( "vipain", 96 ) )
    AAdd( S_sfx, NewSound( "mnpain", 96 ) )
    AAdd( S_sfx, NewSound( "pepain", 96 ) )
    AAdd( S_sfx, NewSound( "slop", 78 ) )
    AAdd( S_sfx, NewSound( "itemup", 78 ) )
    AAdd( S_sfx, NewSound( "wpnup", 78 ) )
    AAdd( S_sfx, NewSound( "oof", 96 ) )
    AAdd( S_sfx, NewSound( "telept", 32 ) )
    AAdd( S_sfx, NewSound( "posit1", 98 ) )
    AAdd( S_sfx, NewSound( "posit2", 98 ) )
    AAdd( S_sfx, NewSound( "posit3", 98 ) )
    AAdd( S_sfx, NewSound( "bgsit1", 98 ) )
    AAdd( S_sfx, NewSound( "bgsit2", 98 ) )
    AAdd( S_sfx, NewSound( "sgtsit", 98 ) )
    AAdd( S_sfx, NewSound( "cacsit", 98 ) )
    AAdd( S_sfx, NewSound( "brssit", 94 ) )
    AAdd( S_sfx, NewSound( "cybsit", 92 ) )
    AAdd( S_sfx, NewSound( "spisit", 90 ) )
    AAdd( S_sfx, NewSound( "bspsit", 90 ) )
    AAdd( S_sfx, NewSound( "kntsit", 90 ) )
    AAdd( S_sfx, NewSound( "vilsit", 90 ) )
    AAdd( S_sfx, NewSound( "mansit", 90 ) )
    AAdd( S_sfx, NewSound( "pesit", 90 ) )
    AAdd( S_sfx, NewSound( "sklatk", 70 ) )
    AAdd( S_sfx, NewSound( "sgtatk", 70 ) )
    AAdd( S_sfx, NewSound( "skepch", 70 ) )
    AAdd( S_sfx, NewSound( "vilatk", 70 ) )
    AAdd( S_sfx, NewSound( "claw", 70 ) )
    AAdd( S_sfx, NewSound( "skeswg", 70 ) )
    AAdd( S_sfx, NewSound( "pldeth", 32 ) )
    AAdd( S_sfx, NewSound( "pdiehi", 32 ) )
    AAdd( S_sfx, NewSound( "podth1", 70 ) )
    AAdd( S_sfx, NewSound( "podth2", 70 ) )
    AAdd( S_sfx, NewSound( "podth3", 70 ) )
    AAdd( S_sfx, NewSound( "bgdth1", 70 ) )
    AAdd( S_sfx, NewSound( "bgdth2", 70 ) )
    AAdd( S_sfx, NewSound( "sgtdth", 70 ) )
    AAdd( S_sfx, NewSound( "cacdth", 70 ) )
    AAdd( S_sfx, NewSound( "skldth", 70 ) )
    AAdd( S_sfx, NewSound( "brsdth", 32 ) )
    AAdd( S_sfx, NewSound( "cybdth", 32 ) )
    AAdd( S_sfx, NewSound( "spidth", 32 ) )
    AAdd( S_sfx, NewSound( "bspdth", 32 ) )
    AAdd( S_sfx, NewSound( "vildth", 32 ) )
    AAdd( S_sfx, NewSound( "kntdth", 32 ) )
    AAdd( S_sfx, NewSound( "pedth", 32 ) )
    AAdd( S_sfx, NewSound( "skedth", 32 ) )
    AAdd( S_sfx, NewSound( "posact", 120 ) )
    AAdd( S_sfx, NewSound( "bgact", 120 ) )
    AAdd( S_sfx, NewSound( "dmact", 120 ) )
    AAdd( S_sfx, NewSound( "bspact", 100 ) )
    AAdd( S_sfx, NewSound( "bspwlk", 100 ) )
    AAdd( S_sfx, NewSound( "vilact", 100 ) )
    AAdd( S_sfx, NewSound( "noway", 78 ) )
    AAdd( S_sfx, NewSound( "barexp", 60 ) )
    AAdd( S_sfx, NewSound( "punch", 64 ) )
    AAdd( S_sfx, NewSound( "hoof", 70 ) )
    AAdd( S_sfx, NewSound( "metal", 70 ) )
    AAdd( S_sfx, NewSound( "chgun", 64 ) )
    AAdd( S_sfx, NewSound( "tink", 60 ) )
    AAdd( S_sfx, NewSound( "bdopn", 100 ) )
    AAdd( S_sfx, NewSound( "bdcls", 100 ) )
    AAdd( S_sfx, NewSound( "itmbk", 100 ) )
    AAdd( S_sfx, NewSound( "flame", 32 ) )
    AAdd( S_sfx, NewSound( "flamst", 32 ) )
    AAdd( S_sfx, NewSound( "getpow", 60 ) )
    AAdd( S_sfx, NewSound( "bospit", 70 ) )
    AAdd( S_sfx, NewSound( "boscub", 70 ) )
    AAdd( S_sfx, NewSound( "bossit", 70 ) )
    AAdd( S_sfx, NewSound( "bospn", 70 ) )
    AAdd( S_sfx, NewSound( "bosdth", 70 ) )
    AAdd( S_sfx, NewSound( "manatk", 70 ) )
    AAdd( S_sfx, NewSound( "mandth", 70 ) )
    AAdd( S_sfx, NewSound( "sssit", 70 ) )
    AAdd( S_sfx, NewSound( "ssdth", 70 ) )
    AAdd( S_sfx, NewSound( "keenpn", 70 ) )
    AAdd( S_sfx, NewSound( "keendt", 70 ) )
    AAdd( S_sfx, NewSound( "skeact", 70 ) )
    AAdd( S_sfx, NewSound( "skesit", 70 ) )
    AAdd( S_sfx, NewSound( "skeatk", 70 ) )
    AAdd( S_sfx, NewSound( "radio", 60 ) )

    S_sfx[ 87 ]:link := S_sfx[ sfx_pistol + 1 ]
    S_sfx[ 87 ]:pitch := 150
    S_sfx[ 87 ]:volume := 0
RETURN
