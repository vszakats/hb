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

#include "d_items.ch"
#include "d_ticcmd.ch"
#include "d_player.ch"

CLASS pspdef_t
    DATA state
    DATA iState
    DATA tics
    DATA sx
    DATA sy
    METHOD New()
ENDCLASS
CLASS player_t
    DATA mo
    DATA playerstate
    DATA cmd
    DATA viewz
    DATA viewheight
    DATA deltaviewheight
    DATA bob
    DATA health
    DATA armorpoints
    DATA armortype
    DATA powers
    DATA cards
    DATA backpack
    DATA frags
    DATA readyweapon
    DATA pendingweapon
    DATA weaponowned
    DATA ammo
    DATA maxammo
    DATA attackdown
    DATA usedown
    DATA cheats
    DATA refire
    DATA killcount
    DATA itemcount
    DATA secretcount
    DATA message
    DATA damagecount
    DATA bonuscount
    DATA attacker
    DATA extralight
    DATA fixedcolormap
    DATA colormap
    DATA psprites
    DATA didsecret
    METHOD New()
ENDCLASS
CLASS wbplayerstruct_t
    DATA in
    DATA skills
    DATA sitems
    DATA ssecret
    DATA stime
    DATA frags
    DATA score
    METHOD New()
ENDCLASS
CLASS wbstartstruct_t
    DATA epsd
    DATA didsecret
    DATA last
    DATA next
    DATA maxkills
    DATA maxitems
    DATA maxsecret
    DATA maxfrags
    DATA partime
    DATA pnum
    DATA plyr
    METHOD New()
ENDCLASS

METHOD New() CLASS pspdef_t
    ::state  := NIL
    ::iState := 0
    ::tics   := 0
    ::sx     := 0
    ::sy     := 0
RETURN Self

METHOD New() CLASS player_t
    LOCAL i

    ::mo := NIL
    ::playerstate := PST_LIVE
    ::cmd := ticcmd_t():New()
    ::viewz := 0
    ::viewheight := 0
    ::deltaviewheight := 0
    ::bob := 0
    ::health := 0
    ::armorpoints := 0
    ::armortype := 0

    ::powers := {}
    FOR i := 1 TO NUMPOWERS
        AAdd( ::powers, 0 )
    NEXT

    ::cards := {}
    FOR i := 1 TO NUMCARDS
        AAdd( ::cards, .F. )
    NEXT

    ::backpack := .F.

    ::frags := {}
    FOR i := 1 TO MAXPLAYERS
        AAdd( ::frags, 0 )
    NEXT

    ::readyweapon := wp_fist
    ::pendingweapon := wp_fist

    ::weaponowned := {}
    FOR i := 1 TO NUMWEAPONS
        AAdd( ::weaponowned, .F. )
    NEXT

    ::ammo := {}
    ::maxammo := {}
    FOR i := 1 TO NUMAMMO
        AAdd( ::ammo, 0 )
        AAdd( ::maxammo, 0 )
    NEXT

    ::attackdown := .F.
    ::usedown := .F.
    ::cheats := 0
    ::refire := 0
    ::killcount := 0
    ::itemcount := 0
    ::secretcount := 0
    ::message := NIL
    ::damagecount := 0
    ::bonuscount := 0
    ::attacker := NIL
    ::extralight := 0
    ::fixedcolormap := 0
    ::colormap := 0

    ::psprites := {}
    FOR i := 1 TO NUMPSPRITES
        AAdd( ::psprites, pspdef_t():New() )
    NEXT

    ::didsecret := .F.
RETURN Self

METHOD New() CLASS wbplayerstruct_t
    LOCAL i

    ::in := .F.
    ::skills := 0
    ::sitems := 0
    ::ssecret := 0
    ::stime := 0
    ::frags := {}
    FOR i := 1 TO MAXPLAYERS
        AAdd( ::frags, 0 )
    NEXT
    ::score := 0
RETURN Self

METHOD New() CLASS wbstartstruct_t
    LOCAL i

    ::epsd := 0
    ::didsecret := .F.
    ::last := 0
    ::next := 0
    ::maxkills := 0
    ::maxitems := 0
    ::maxsecret := 0
    ::maxfrags := 0
    ::partime := 0
    ::pnum := 0
    ::plyr := {}
    FOR i := 1 TO MAXPLAYERS
        AAdd( ::plyr, wbplayerstruct_t():New() )
    NEXT
RETURN Self
