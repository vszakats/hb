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

STATIC default_main_config
STATIC default_extra_config
STATIC doom_defaults_list
STATIC extra_defaults_list
STATIC doom_defaults
STATIC extra_defaults
STATIC scantokey

#include "doomtype.ch"
#include "doomkeys.ch"
#include "doomfeatures.ch"
#include "m_argv.ch"
#include "m_config.ch"

CLASS default_t
    DATA name
    DATA location
    DATA type
    DATA untranslated
    DATA original_translated
    DATA bound
    METHOD New()
ENDCLASS
CLASS default_collection_t
    DATA defaults
    DATA numdefaults
    DATA filename
    METHOD New()
ENDCLASS



METHOD New( name, location, type, untranslated, original_translated, bound ) CLASS default_t
    ::name                 := iif( name == NIL, "", name )
    ::location             := location
    ::type                 := iif( type == NIL, DEFAULT_INT, type )
    ::untranslated         := iif( untranslated == NIL, 0, untranslated )
    ::original_translated  := iif( original_translated == NIL, 0, original_translated )
    ::bound                := iif( bound == NIL, .F., bound )
RETURN Self

METHOD New( defaults, numdefaults, filename ) CLASS default_collection_t
    ::defaults    := iif( defaults == NIL, {}, defaults )
    ::numdefaults := iif( numdefaults == NIL, Len( ::defaults ), numdefaults )
    ::filename    := filename
RETURN Self

STATIC FUNCTION Cfg( cName, nType )
RETURN default_t():New( cName, NIL, nType, 0, 0, .F. )

INIT PROCEDURE init_m_config

    PUBLIC configdir

    configdir := NIL
    default_main_config := NIL
    default_extra_config := NIL

    doom_defaults_list := {}
    AAdd( doom_defaults_list, Cfg( "mouse_sensitivity", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "sfx_volume", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "music_volume", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "show_talk", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "voice_volume", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "show_messages", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "key_right", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_left", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_up", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_down", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_strafeleft", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_straferight", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_useHealth", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_jump", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_flyup", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_flydown", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_flycenter", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_lookup", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_lookdown", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_lookcenter", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invquery", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_mission", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invPop", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invKey", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invHome", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invEnd", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invleft", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invright", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invLeft", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invRight", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_useartifact", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invUse", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_invDrop", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_lookUp", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_lookDown", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_fire", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_use", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_strafe", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "key_speed", DEFAULT_KEY ) )
    AAdd( doom_defaults_list, Cfg( "use_mouse", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "mouseb_fire", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "mouseb_strafe", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "mouseb_forward", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "mouseb_jump", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "use_joystick", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "joyb_fire", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "joyb_strafe", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "joyb_use", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "joyb_speed", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "joyb_jump", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "screenblocks", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "screensize", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "detaillevel", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "snd_channels", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "snd_musicdevice", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "snd_sfxdevice", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "snd_sbport", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "snd_sbirq", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "snd_sbdma", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "snd_mport", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "usegamma", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "savedir", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "messageson", DEFAULT_INT ) )
    AAdd( doom_defaults_list, Cfg( "back_flat", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "nickname", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro0", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro1", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro2", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro3", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro4", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro5", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro6", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro7", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro8", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "chatmacro9", DEFAULT_STRING ) )
    AAdd( doom_defaults_list, Cfg( "comport", DEFAULT_INT ) )

    extra_defaults_list := {}
    AAdd( extra_defaults_list, Cfg( "graphical_startup", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "autoadjust_video_settings", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "fullscreen", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "aspect_ratio_correct", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "startup_delay", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "screen_width", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "screen_height", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "screen_bpp", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "grabmouse", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "novert", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "mouse_acceleration", DEFAULT_FLOAT ) )
    AAdd( extra_defaults_list, Cfg( "mouse_threshold", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "snd_samplerate", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "snd_cachesize", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "snd_maxslicetime_ms", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "snd_musiccmd", DEFAULT_STRING ) )
    AAdd( extra_defaults_list, Cfg( "opl_io_port", DEFAULT_INT_HEX ) )
    AAdd( extra_defaults_list, Cfg( "show_endoom", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "png_screenshots", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "vanilla_savegame_limit", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "vanilla_demo_limit", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "vanilla_keyboard_mapping", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "video_driver", DEFAULT_STRING ) )
    AAdd( extra_defaults_list, Cfg( "window_position", DEFAULT_STRING ) )
    AAdd( extra_defaults_list, Cfg( "joystick_index", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_x_axis", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_x_invert", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_y_axis", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_y_invert", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_strafe_axis", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_strafe_invert", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button0", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button1", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button2", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button3", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button4", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button5", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button6", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button7", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button8", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joystick_physical_button9", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joyb_strafeleft", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joyb_straferight", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joyb_menu_activate", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joyb_prevweapon", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "joyb_nextweapon", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "mouseb_strafeleft", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "mouseb_straferight", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "mouseb_use", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "mouseb_backward", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "mouseb_prevweapon", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "mouseb_nextweapon", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "dclick_use", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "use_libsamplerate", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "libsamplerate_scale", DEFAULT_FLOAT ) )
    AAdd( extra_defaults_list, Cfg( "timidity_cfg_path", DEFAULT_STRING ) )
    AAdd( extra_defaults_list, Cfg( "gus_patch_path", DEFAULT_STRING ) )
    AAdd( extra_defaults_list, Cfg( "gus_ram_kb", DEFAULT_INT ) )
    AAdd( extra_defaults_list, Cfg( "key_pause", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_activate", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_up", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_down", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_left", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_right", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_back", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_forward", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_confirm", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_abort", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_help", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_save", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_load", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_volume", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_detail", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_qsave", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_endgame", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_messages", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_qload", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_quit", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_gamma", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_spy", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_incscreen", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_decscreen", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_menu_screenshot", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_toggle", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_north", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_south", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_east", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_west", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_zoomin", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_zoomout", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_maxzoom", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_follow", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_grid", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_mark", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_map_clearmark", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_weapon1", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_weapon2", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_weapon3", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_weapon4", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_weapon5", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_weapon6", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_weapon7", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_weapon8", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_prevweapon", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_nextweapon", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_arti_all", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_arti_health", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_arti_poisonbag", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_arti_blastradius", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_arti_teleport", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_arti_teleportother", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_arti_egg", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_arti_invulnerability", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_message_refresh", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_demo_quit", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_multi_msg", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_multi_msgplayer1", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_multi_msgplayer2", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_multi_msgplayer3", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_multi_msgplayer4", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_multi_msgplayer5", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_multi_msgplayer6", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_multi_msgplayer7", DEFAULT_KEY ) )
    AAdd( extra_defaults_list, Cfg( "key_multi_msgplayer8", DEFAULT_KEY ) )

    doom_defaults := default_collection_t():New( doom_defaults_list, Len( doom_defaults_list ), NIL )
    extra_defaults := default_collection_t():New( extra_defaults_list, Len( extra_defaults_list ), NIL )

    scantokey := { ;
        0, 27, 49, 50, 51, 52, 53, 54, ;
        55, 56, 57, 48, 45, 61, KEY_BACKSPACE, 9, ;
        113, 119, 101, 114, 116, 121, 117, 105, ;
        111, 112, 91, 93, 13, KEY_RCTRL, 97, 115, ;
        100, 102, 103, 104, 106, 107, 108, 59, ;
        39, 96, KEY_RSHIFT, 92, 122, 120, 99, 118, ;
        98, 110, 109, 44, 46, 47, KEY_RSHIFT, KEYP_MULTIPLY, ;
        KEY_RALT, 32, KEY_CAPSLOCK, KEY_F1, KEY_F2, KEY_F3, KEY_F4, KEY_F5, ;
        KEY_F6, KEY_F7, KEY_F8, KEY_F9, KEY_F10, KEY_PAUSE, KEY_SCRLCK, KEY_HOME, ;
        KEY_UPARROW, KEY_PGUP, KEY_MINUS, KEY_LEFTARROW, KEYP_5, KEY_RIGHTARROW, KEYP_PLUS, KEY_END, ;
        KEY_DOWNARROW, KEY_PGDN, KEY_INS, KEY_DEL, 0, 0, 0, KEY_F11, ;
        KEY_F12, 0, 0, 0, 0, 0, 0, 0, ;
        0, 0, 0, 0, 0, 0, 0, 0, ;
        0, 0, 0, 0, 0, 0, 0, 0, ;
        0, 0, 0, 0, 0, 0, 0, 0, ;
        0, 0, 0, 0, 0, 0, KEY_PRTSCR, 0 }
RETURN

STATIC FUNCTION SearchCollection( collection, name )
    LOCAL i
    LOCAL oDef

    FOR i := 0 TO collection:numdefaults - 1
        oDef := collection:defaults[ i + 1 ]
        IF name == oDef:name
            RETURN oDef
        ENDIF
    NEXT
RETURN NIL

STATIC PROCEDURE SaveDefaultCollection( collection )
    HB_SYMBOL_UNUSED( collection )
RETURN

STATIC FUNCTION ParseIntParameter( strparm )
    IF Len( strparm ) >= 2 .AND. Lower( Left( strparm, 2 ) ) == "0x"
        RETURN hb_HexToNum( SubStr( strparm, 3 ) )
    ENDIF
RETURN Int( Val( strparm ) )

STATIC PROCEDURE SetVariable( def, value )
    LOCAL intparm

    SWITCH def:type
    CASE DEFAULT_STRING
        Eval( def:location, value )
        EXIT
    CASE DEFAULT_INT
        Eval( def:location, ParseIntParameter( value ) )
        EXIT
    CASE DEFAULT_INT_HEX
        Eval( def:location, ParseIntParameter( value ) )
        EXIT
    CASE DEFAULT_KEY
        intparm := ParseIntParameter( value )
        def:untranslated := intparm
        IF intparm >= 0 .AND. intparm < 128
            intparm := scantokey[ intparm + 1 ]
        ELSE
            intparm := 0
        ENDIF
        def:original_translated := intparm
        Eval( def:location, intparm )
        EXIT
    CASE DEFAULT_FLOAT
        Eval( def:location, Val( value ) )
        EXIT
    ENDSWITCH
RETURN

STATIC PROCEDURE LoadDefaultCollection( collection )
    HB_SYMBOL_UNUSED( collection )
RETURN

FUNCTION M_SetConfigFilenames( main_config, extra_config )
    default_main_config := main_config
    default_extra_config := extra_config
RETURN NIL

FUNCTION M_SaveDefaults()
    SaveDefaultCollection( doom_defaults )
    SaveDefaultCollection( extra_defaults )
RETURN NIL

FUNCTION M_SaveDefaultsAlternate( main, extra )
    LOCAL orig_main
    LOCAL orig_extra

    orig_main := doom_defaults:filename
    orig_extra := extra_defaults:filename
    doom_defaults:filename := main
    extra_defaults:filename := extra
    M_SaveDefaults()
    doom_defaults:filename := orig_main
    extra_defaults:filename := orig_extra
RETURN NIL

FUNCTION M_LoadDefaults()
    LOCAL i
    MEMVAR configdir
    MEMVAR myargv

    i := M_CheckParmWithArgs( "-config", 1 )
    IF i != 0
        doom_defaults:filename := myargv[ i + 1 + 1 ]
        OutStd( "	default file: " + doom_defaults:filename + hb_eol() )
    ELSE
        doom_defaults:filename := M_StringJoin( configdir, default_main_config, NIL )
    ENDIF

    OutStd( "saving config in " + doom_defaults:filename + hb_eol() )

    i := M_CheckParmWithArgs( "-extraconfig", 1 )
    IF i != 0
        extra_defaults:filename := myargv[ i + 1 + 1 ]
        OutStd( "        extra configuration file: " + extra_defaults:filename + hb_eol() )
    ELSE
        extra_defaults:filename := M_StringJoin( configdir, default_extra_config, NIL )
    ENDIF

    LoadDefaultCollection( doom_defaults )
    LoadDefaultCollection( extra_defaults )
RETURN NIL

STATIC FUNCTION GetDefaultForName( name )
    LOCAL result

    result := SearchCollection( doom_defaults, name )
    IF result == NIL
        result := SearchCollection( extra_defaults, name )
    ENDIF
    IF result == NIL
        I_Error( "Unknown configuration variable: '" + name + "'" )
    ENDIF
RETURN result

FUNCTION M_BindVariable( name, location )
    LOCAL variable

    variable := GetDefaultForName( name )
    variable:location := {|x|
        IF PCount() == 0
            RETURN location
        ENDIF
        location := x
        RETURN location
    }
    variable:bound := .T.
RETURN NIL

FUNCTION M_SetVariable( name, value )
    LOCAL variable

    variable := GetDefaultForName( name )
    IF variable == NIL .OR. ! variable:bound
        RETURN .F.
    ENDIF
    SetVariable( variable, value )
RETURN .T.

FUNCTION M_GetIntVariable( name )
    LOCAL variable

    variable := GetDefaultForName( name )
    IF variable == NIL .OR. ! variable:bound ;
      .OR. ( variable:type != DEFAULT_INT .AND. variable:type != DEFAULT_INT_HEX )
        RETURN 0
    ENDIF
RETURN Eval( variable:location )

FUNCTION M_GetStrVariable( name )
    LOCAL variable

    variable := GetDefaultForName( name )
    IF variable == NIL .OR. ! variable:bound .OR. variable:type != DEFAULT_STRING
        RETURN NIL
    ENDIF
RETURN Eval( variable:location )

FUNCTION M_GetFloatVariable( name )
    LOCAL variable

    variable := GetDefaultForName( name )
    IF variable == NIL .OR. ! variable:bound .OR. variable:type != DEFAULT_FLOAT
        RETURN 0
    ENDIF
RETURN Eval( variable:location )

STATIC FUNCTION GetDefaultConfigDir()
RETURN "."

FUNCTION M_SetConfigDir( dir )
    MEMVAR configdir
    IF dir != NIL
        configdir := dir
    ELSE
        configdir := GetDefaultConfigDir()
    ENDIF

    IF configdir != ""
        OutStd( "Using " + configdir + " for configuration and saves" + hb_eol() )
    ENDIF

    M_MakeDirectory( configdir )
RETURN NIL

FUNCTION M_GetSaveGameDir( iwadname )
    LOCAL savegamedir
    MEMVAR configdir

    HB_SYMBOL_UNUSED( iwadname )

    IF configdir == ""
        savegamedir := ""
    ELSE
        savegamedir := M_StringJoin( configdir, DIR_SEPARATOR_S, ".savegame/", NIL )
        M_MakeDirectory( savegamedir )
        OutStd( "Using " + savegamedir + " for savegames" + hb_eol() )
    ENDIF
RETURN savegamedir
