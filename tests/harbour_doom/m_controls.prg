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

STATIC key_jump
STATIC key_flyup
STATIC key_flydown
STATIC key_flycenter
STATIC key_lookup
STATIC key_lookdown
STATIC key_lookcenter
STATIC key_invleft
STATIC key_invright
STATIC key_useartifact
STATIC key_usehealth
STATIC key_invquery
STATIC key_mission
STATIC key_invpop
STATIC key_invkey
STATIC key_invhome
STATIC key_invend
STATIC key_invuse
STATIC key_invdrop
STATIC key_arti_all
STATIC key_arti_health
STATIC key_arti_poisonbag
STATIC key_arti_blastradius
STATIC key_arti_teleport
STATIC key_arti_teleportother
STATIC key_arti_egg
STATIC key_arti_invulnerability
STATIC mousebjump
STATIC joybjump

#include "doomkeys.ch"
#include "m_controls.ch"


INIT PROCEDURE init_m_controls
    LOCAL i

    PUBLIC key_right
    PUBLIC key_left
    PUBLIC key_up
    PUBLIC key_down
    PUBLIC key_strafeleft
    PUBLIC key_straferight
    PUBLIC key_fire
    PUBLIC key_use
    PUBLIC key_strafe
    PUBLIC key_speed
    PUBLIC key_message_refresh
    PUBLIC key_pause
    PUBLIC key_multi_msg
    PUBLIC key_multi_msgplayer
    PUBLIC key_weapon1
    PUBLIC key_weapon2
    PUBLIC key_weapon3
    PUBLIC key_weapon4
    PUBLIC key_weapon5
    PUBLIC key_weapon6
    PUBLIC key_weapon7
    PUBLIC key_weapon8
    PUBLIC key_demo_quit
    PUBLIC key_spy
    PUBLIC key_prevweapon
    PUBLIC key_nextweapon
    PUBLIC key_map_north
    PUBLIC key_map_south
    PUBLIC key_map_east
    PUBLIC key_map_west
    PUBLIC key_map_zoomin
    PUBLIC key_map_zoomout
    PUBLIC key_map_toggle
    PUBLIC key_map_maxzoom
    PUBLIC key_map_follow
    PUBLIC key_map_grid
    PUBLIC key_map_mark
    PUBLIC key_map_clearmark
    PUBLIC key_menu_activate
    PUBLIC key_menu_up
    PUBLIC key_menu_down
    PUBLIC key_menu_left
    PUBLIC key_menu_right
    PUBLIC key_menu_back
    PUBLIC key_menu_forward
    PUBLIC key_menu_confirm
    PUBLIC key_menu_abort
    PUBLIC key_menu_help
    PUBLIC key_menu_save
    PUBLIC key_menu_load
    PUBLIC key_menu_volume
    PUBLIC key_menu_detail
    PUBLIC key_menu_qsave
    PUBLIC key_menu_endgame
    PUBLIC key_menu_messages
    PUBLIC key_menu_qload
    PUBLIC key_menu_quit
    PUBLIC key_menu_gamma
    PUBLIC key_menu_incscreen
    PUBLIC key_menu_decscreen
    PUBLIC key_menu_screenshot
    PUBLIC mousebfire
    PUBLIC mousebstrafe
    PUBLIC mousebforward
    PUBLIC mousebstrafeleft
    PUBLIC mousebstraferight
    PUBLIC mousebbackward
    PUBLIC mousebuse
    PUBLIC mousebprevweapon
    PUBLIC mousebnextweapon
    PUBLIC joybfire
    PUBLIC joybstrafe
    PUBLIC joybuse
    PUBLIC joybspeed
    PUBLIC joybstrafeleft
    PUBLIC joybstraferight
    PUBLIC joybprevweapon
    PUBLIC joybnextweapon
    PUBLIC joybmenu
    PUBLIC dclick_use

    key_right := KEY_RIGHTARROW
    key_left := KEY_LEFTARROW
    key_up := KEY_UPARROW
    key_down := KEY_DOWNARROW
    key_strafeleft := KEY_STRAFE_L
    key_straferight := KEY_STRAFE_R
    key_fire := KEY_FIRE
    key_use := KEY_USE
    key_strafe := KEY_RALT
    key_speed := KEY_RSHIFT

    key_flyup := KEY_PGUP
    key_flydown := KEY_INS
    key_flycenter := KEY_HOME

    key_lookup := KEY_PGDN
    key_lookdown := KEY_DEL
    key_lookcenter := KEY_END

    key_invleft := 91
    key_invright := 93
    key_useartifact := KEY_ENTER

    key_jump := 47

    key_arti_all := KEY_BACKSPACE
    key_arti_health := 92
    key_arti_poisonbag := 48
    key_arti_blastradius := 57
    key_arti_teleport := 56
    key_arti_teleportother := 55
    key_arti_egg := 54
    key_arti_invulnerability := 53

    key_usehealth := 104
    key_invquery := 113
    key_mission := 119
    key_invpop := 122
    key_invkey := 107
    key_invhome := KEY_HOME
    key_invend := KEY_END
    key_invuse := KEY_ENTER
    key_invdrop := KEY_BACKSPACE

    mousebfire := 0
    mousebstrafe := 1
    mousebforward := 2
    mousebjump := -1
    mousebstrafeleft := -1
    mousebstraferight := -1
    mousebbackward := -1
    mousebuse := -1
    mousebprevweapon := -1
    mousebnextweapon := -1

    key_message_refresh := KEY_ENTER
    key_pause := KEY_PAUSE
    key_demo_quit := 113
    key_spy := KEY_F12

    key_multi_msg := 116
    key_multi_msgplayer := {}
    FOR i := 1 TO 8
        AAdd( key_multi_msgplayer, 0 )
    NEXT

    key_weapon1 := 49
    key_weapon2 := 50
    key_weapon3 := 51
    key_weapon4 := 52
    key_weapon5 := 53
    key_weapon6 := 54
    key_weapon7 := 55
    key_weapon8 := 56
    key_prevweapon := 0
    key_nextweapon := 0

    key_map_north := KEY_UPARROW
    key_map_south := KEY_DOWNARROW
    key_map_east := KEY_RIGHTARROW
    key_map_west := KEY_LEFTARROW
    key_map_zoomin := 61
    key_map_zoomout := 45
    key_map_toggle := KEY_TAB
    key_map_maxzoom := 48
    key_map_follow := 102
    key_map_grid := 103
    key_map_mark := 109
    key_map_clearmark := 99

    key_menu_activate := KEY_ESCAPE
    key_menu_up := KEY_UPARROW
    key_menu_down := KEY_DOWNARROW
    key_menu_left := KEY_LEFTARROW
    key_menu_right := KEY_RIGHTARROW
    key_menu_back := KEY_BACKSPACE
    key_menu_forward := KEY_ENTER
    key_menu_confirm := 121
    key_menu_abort := 110

    key_menu_help := KEY_F1
    key_menu_save := KEY_F2
    key_menu_load := KEY_F3
    key_menu_volume := KEY_F4
    key_menu_detail := KEY_F5
    key_menu_qsave := KEY_F6
    key_menu_endgame := KEY_F7
    key_menu_messages := KEY_F8
    key_menu_qload := KEY_F9
    key_menu_quit := KEY_F10
    key_menu_gamma := KEY_F11

    key_menu_incscreen := KEY_EQUALS
    key_menu_decscreen := KEY_MINUS
    key_menu_screenshot := 0

    joybfire := 0
    joybstrafe := 1
    joybuse := 3
    joybspeed := 2
    joybstrafeleft := -1
    joybstraferight := -1
    joybjump := -1
    joybprevweapon := -1
    joybnextweapon := -1
    joybmenu := -1
    dclick_use := 1
RETURN

FUNCTION M_BindBaseControls()
    MEMVAR dclick_use
    MEMVAR joybfire
    MEMVAR joybmenu
    MEMVAR joybspeed
    MEMVAR joybstrafe
    MEMVAR joybstrafeleft
    MEMVAR joybstraferight
    MEMVAR joybuse
    MEMVAR key_down
    MEMVAR key_left
    MEMVAR key_message_refresh
    MEMVAR key_right
    MEMVAR key_speed
    MEMVAR key_strafe
    MEMVAR key_strafeleft
    MEMVAR key_straferight
    MEMVAR key_up
    MEMVAR mousebbackward
    MEMVAR mousebfire
    MEMVAR mousebforward
    MEMVAR mousebstrafe
    MEMVAR mousebstrafeleft
    MEMVAR mousebstraferight
    MEMVAR mousebuse
    M_BindVariable( "key_right",           @key_right )
    M_BindVariable( "key_left",            @key_left )
    M_BindVariable( "key_up",              @key_up )
    M_BindVariable( "key_down",            @key_down )
    M_BindVariable( "key_strafeleft",      @key_strafeleft )
    M_BindVariable( "key_straferight",     @key_straferight )
    M_BindVariable( "key_fire",            @key_fire )
    M_BindVariable( "key_use",             @key_use )
    M_BindVariable( "key_strafe",          @key_strafe )
    M_BindVariable( "key_speed",           @key_speed )

    M_BindVariable( "mouseb_fire",         @mousebfire )
    M_BindVariable( "mouseb_strafe",       @mousebstrafe )
    M_BindVariable( "mouseb_forward",      @mousebforward )

    M_BindVariable( "joyb_fire",           @joybfire )
    M_BindVariable( "joyb_strafe",         @joybstrafe )
    M_BindVariable( "joyb_use",            @joybuse )
    M_BindVariable( "joyb_speed",          @joybspeed )

    M_BindVariable( "joyb_menu_activate",  @joybmenu )

    M_BindVariable( "joyb_strafeleft",     @joybstrafeleft )
    M_BindVariable( "joyb_straferight",    @joybstraferight )
    M_BindVariable( "mouseb_strafeleft",   @mousebstrafeleft )
    M_BindVariable( "mouseb_straferight",  @mousebstraferight )
    M_BindVariable( "mouseb_use",          @mousebuse )
    M_BindVariable( "mouseb_backward",     @mousebbackward )
    M_BindVariable( "dclick_use",          @dclick_use )
    M_BindVariable( "key_pause",           @key_pause )
    M_BindVariable( "key_message_refresh", @key_message_refresh )
RETURN NIL

FUNCTION M_BindHereticControls()
    M_BindVariable( "key_flyup",       @key_flyup )
    M_BindVariable( "key_flydown",     @key_flydown )
    M_BindVariable( "key_flycenter",   @key_flycenter )
    M_BindVariable( "key_lookup",      @key_lookup )
    M_BindVariable( "key_lookdown",    @key_lookdown )
    M_BindVariable( "key_lookcenter",  @key_lookcenter )
    M_BindVariable( "key_invleft",     @key_invleft )
    M_BindVariable( "key_invright",    @key_invright )
    M_BindVariable( "key_useartifact", @key_useartifact )
RETURN NIL

FUNCTION M_BindHexenControls()
    M_BindVariable( "key_jump",                @key_jump )
    M_BindVariable( "mouseb_jump",             @mousebjump )
    M_BindVariable( "joyb_jump",               @joybjump )
    M_BindVariable( "key_arti_all",            @key_arti_all )
    M_BindVariable( "key_arti_health",         @key_arti_health )
    M_BindVariable( "key_arti_poisonbag",      @key_arti_poisonbag )
    M_BindVariable( "key_arti_blastradius",    @key_arti_blastradius )
    M_BindVariable( "key_arti_teleport",       @key_arti_teleport )
    M_BindVariable( "key_arti_teleportother",  @key_arti_teleportother )
    M_BindVariable( "key_arti_egg",            @key_arti_egg )
    M_BindVariable( "key_arti_invulnerability", @key_arti_invulnerability )
RETURN NIL

FUNCTION M_BindStrifeControls()
    MEMVAR key_message_refresh
    key_message_refresh := 47
    key_jump     := 97
    key_lookup   := KEY_PGUP
    key_lookdown := KEY_PGDN
    key_invleft  := KEY_INS
    key_invright := KEY_DEL

    M_BindVariable( "key_jump",      @key_jump )
    M_BindVariable( "key_lookUp",    @key_lookup )
    M_BindVariable( "key_lookDown",  @key_lookdown )
    M_BindVariable( "key_invLeft",   @key_invleft )
    M_BindVariable( "key_invRight",  @key_invright )
    M_BindVariable( "key_useHealth", @key_usehealth )
    M_BindVariable( "key_invquery",  @key_invquery )
    M_BindVariable( "key_mission",   @key_mission )
    M_BindVariable( "key_invPop",    @key_invpop )
    M_BindVariable( "key_invKey",    @key_invkey )
    M_BindVariable( "key_invHome",   @key_invhome )
    M_BindVariable( "key_invEnd",    @key_invend )
    M_BindVariable( "key_invUse",    @key_invuse )
    M_BindVariable( "key_invDrop",   @key_invdrop )
    M_BindVariable( "mouseb_jump",   @mousebjump )
    M_BindVariable( "joyb_jump",     @joybjump )
RETURN NIL

FUNCTION M_BindWeaponControls()
    MEMVAR joybnextweapon
    MEMVAR joybprevweapon
    MEMVAR key_nextweapon
    MEMVAR key_prevweapon
    MEMVAR key_weapon1
    MEMVAR key_weapon2
    MEMVAR key_weapon3
    MEMVAR key_weapon4
    MEMVAR key_weapon5
    MEMVAR key_weapon6
    MEMVAR key_weapon7
    MEMVAR key_weapon8
    MEMVAR mousebnextweapon
    MEMVAR mousebprevweapon
    M_BindVariable( "key_weapon1",       @key_weapon1 )
    M_BindVariable( "key_weapon2",       @key_weapon2 )
    M_BindVariable( "key_weapon3",       @key_weapon3 )
    M_BindVariable( "key_weapon4",       @key_weapon4 )
    M_BindVariable( "key_weapon5",       @key_weapon5 )
    M_BindVariable( "key_weapon6",       @key_weapon6 )
    M_BindVariable( "key_weapon7",       @key_weapon7 )
    M_BindVariable( "key_weapon8",       @key_weapon8 )
    M_BindVariable( "key_prevweapon",    @key_prevweapon )
    M_BindVariable( "key_nextweapon",    @key_nextweapon )
    M_BindVariable( "joyb_prevweapon",   @joybprevweapon )
    M_BindVariable( "joyb_nextweapon",   @joybnextweapon )
    M_BindVariable( "mouseb_prevweapon", @mousebprevweapon )
    M_BindVariable( "mouseb_nextweapon", @mousebnextweapon )
RETURN NIL

FUNCTION M_BindMapControls()
    MEMVAR key_map_clearmark
    MEMVAR key_map_east
    MEMVAR key_map_follow
    MEMVAR key_map_grid
    MEMVAR key_map_mark
    MEMVAR key_map_maxzoom
    MEMVAR key_map_north
    MEMVAR key_map_south
    MEMVAR key_map_toggle
    MEMVAR key_map_west
    MEMVAR key_map_zoomin
    MEMVAR key_map_zoomout
    M_BindVariable( "key_map_north",     @key_map_north )
    M_BindVariable( "key_map_south",     @key_map_south )
    M_BindVariable( "key_map_east",      @key_map_east )
    M_BindVariable( "key_map_west",      @key_map_west )
    M_BindVariable( "key_map_zoomin",    @key_map_zoomin )
    M_BindVariable( "key_map_zoomout",   @key_map_zoomout )
    M_BindVariable( "key_map_toggle",    @key_map_toggle )
    M_BindVariable( "key_map_maxzoom",   @key_map_maxzoom )
    M_BindVariable( "key_map_follow",    @key_map_follow )
    M_BindVariable( "key_map_grid",      @key_map_grid )
    M_BindVariable( "key_map_mark",      @key_map_mark )
    M_BindVariable( "key_map_clearmark", @key_map_clearmark )
RETURN NIL

FUNCTION M_BindMenuControls()
    MEMVAR key_demo_quit
    MEMVAR key_menu_abort
    MEMVAR key_menu_activate
    MEMVAR key_menu_back
    MEMVAR key_menu_confirm
    MEMVAR key_menu_decscreen
    MEMVAR key_menu_detail
    MEMVAR key_menu_down
    MEMVAR key_menu_endgame
    MEMVAR key_menu_forward
    MEMVAR key_menu_gamma
    MEMVAR key_menu_help
    MEMVAR key_menu_incscreen
    MEMVAR key_menu_left
    MEMVAR key_menu_load
    MEMVAR key_menu_messages
    MEMVAR key_menu_qload
    MEMVAR key_menu_qsave
    MEMVAR key_menu_quit
    MEMVAR key_menu_right
    MEMVAR key_menu_save
    MEMVAR key_menu_screenshot
    MEMVAR key_menu_up
    MEMVAR key_menu_volume
    MEMVAR key_spy
    M_BindVariable( "key_menu_activate",   @key_menu_activate )
    M_BindVariable( "key_menu_up",         @key_menu_up )
    M_BindVariable( "key_menu_down",       @key_menu_down )
    M_BindVariable( "key_menu_left",       @key_menu_left )
    M_BindVariable( "key_menu_right",      @key_menu_right )
    M_BindVariable( "key_menu_back",       @key_menu_back )
    M_BindVariable( "key_menu_forward",    @key_menu_forward )
    M_BindVariable( "key_menu_confirm",    @key_menu_confirm )
    M_BindVariable( "key_menu_abort",      @key_menu_abort )
    M_BindVariable( "key_menu_help",       @key_menu_help )
    M_BindVariable( "key_menu_save",       @key_menu_save )
    M_BindVariable( "key_menu_load",       @key_menu_load )
    M_BindVariable( "key_menu_volume",     @key_menu_volume )
    M_BindVariable( "key_menu_detail",     @key_menu_detail )
    M_BindVariable( "key_menu_qsave",      @key_menu_qsave )
    M_BindVariable( "key_menu_endgame",    @key_menu_endgame )
    M_BindVariable( "key_menu_messages",   @key_menu_messages )
    M_BindVariable( "key_menu_qload",      @key_menu_qload )
    M_BindVariable( "key_menu_quit",       @key_menu_quit )
    M_BindVariable( "key_menu_gamma",      @key_menu_gamma )
    M_BindVariable( "key_menu_incscreen",  @key_menu_incscreen )
    M_BindVariable( "key_menu_decscreen",  @key_menu_decscreen )
    M_BindVariable( "key_menu_screenshot", @key_menu_screenshot )
    M_BindVariable( "key_demo_quit",       @key_demo_quit )
    M_BindVariable( "key_spy",             @key_spy )
RETURN NIL

FUNCTION M_BindChatControls( num_players )
    LOCAL i
    LOCAL cName
    MEMVAR key_multi_msg
    MEMVAR key_multi_msgplayer

    M_BindVariable( "key_multi_msg", @key_multi_msg )
    FOR i := 0 TO num_players - 1
        cName := "key_multi_msgplayer" + hb_ntos( i + 1 )
        M_BindVariable( cName, @key_multi_msgplayer[ i + 1 ] )
    NEXT
RETURN NIL

FUNCTION M_ApplyPlatformDefaults()
RETURN NIL
