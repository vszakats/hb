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

STATIC plr
STATIC w_title
STATIC w_chat
STATIC always_off := .F.
STATIC chat_dest := {}
STATIC w_inputbuffer := {}
STATIC message_on := .F.
STATIC message_nottobefuckedwith := .F.
STATIC w_message
STATIC message_counter := 0
STATIC headsupactive := .F.
STATIC mapnames := {}
STATIC mapnames_commercial := {}
STATIC chatchars := {}
STATIC chat_head := 0
STATIC chat_tail := 0
STATIC lastmessage := ""
STATIC altdown := .F.
STATIC num_nobrainers := 0
STATIC chat_char

#include "deh_main.ch"
#include "dstrings.ch"
#include "doomstat.ch"
#include "doomkeys.ch"
#include "hu_lib.ch"
#include "hu_stuff.ch"

#ifndef PU_STATIC
#define PU_STATIC 1
#endif

#ifndef SCREENWIDTH
#define SCREENWIDTH  320
#endif

#ifndef sfx_tink
#define sfx_tink  87
#define sfx_radio 108
#endif

#define HU_TITLEHEIGHT 1
#define HU_TITLEX      0
#define HU_INPUTTOGGLE 116
#define HU_INPUTX      HU_MSGX
#define HU_INPUTWIDTH  64
#define HU_INPUTHEIGHT 1
#define QUEUESIZE      128



INIT PROCEDURE init_hu_stuff
    LOCAL i

    PUBLIC chat_macros
    PUBLIC player_names
    PUBLIC hu_font
    PUBLIC chat_on
    PUBLIC message_dontfuckwithme

    chat_macros := {}
    AAdd( chat_macros, HUSTR_CHATMACRO0 )
    AAdd( chat_macros, HUSTR_CHATMACRO1 )
    AAdd( chat_macros, HUSTR_CHATMACRO2 )
    AAdd( chat_macros, HUSTR_CHATMACRO3 )
    AAdd( chat_macros, HUSTR_CHATMACRO4 )
    AAdd( chat_macros, HUSTR_CHATMACRO5 )
    AAdd( chat_macros, HUSTR_CHATMACRO6 )
    AAdd( chat_macros, HUSTR_CHATMACRO7 )
    AAdd( chat_macros, HUSTR_CHATMACRO8 )
    AAdd( chat_macros, HUSTR_CHATMACRO9 )

    player_names := { HUSTR_PLRGREEN, HUSTR_PLRINDIGO, HUSTR_PLRBROWN, HUSTR_PLRRED }

    hu_font := {}
    chat_on := .F.
    message_dontfuckwithme := .F.
    chat_char := 0

    w_title := hu_textline_t():New()
    w_chat := hu_itext_t():New()
    w_message := hu_stext_t():New()
    always_off := .F.

    chat_dest := {}
    w_inputbuffer := {}
    FOR i := 1 TO MAXPLAYERS
        AAdd( chat_dest, 0 )
        AAdd( w_inputbuffer, hu_itext_t():New() )
    NEXT

    chatchars := {}
    FOR i := 1 TO QUEUESIZE
        AAdd( chatchars, 0 )
    NEXT

    mapnames := {}
    AAdd( mapnames, HUSTR_E1M1 ) ; AAdd( mapnames, HUSTR_E1M2 ) ; AAdd( mapnames, HUSTR_E1M3 )
    AAdd( mapnames, HUSTR_E1M4 ) ; AAdd( mapnames, HUSTR_E1M5 ) ; AAdd( mapnames, HUSTR_E1M6 )
    AAdd( mapnames, HUSTR_E1M7 ) ; AAdd( mapnames, HUSTR_E1M8 ) ; AAdd( mapnames, HUSTR_E1M9 )
    AAdd( mapnames, HUSTR_E2M1 ) ; AAdd( mapnames, HUSTR_E2M2 ) ; AAdd( mapnames, HUSTR_E2M3 )
    AAdd( mapnames, HUSTR_E2M4 ) ; AAdd( mapnames, HUSTR_E2M5 ) ; AAdd( mapnames, HUSTR_E2M6 )
    AAdd( mapnames, HUSTR_E2M7 ) ; AAdd( mapnames, HUSTR_E2M8 ) ; AAdd( mapnames, HUSTR_E2M9 )
    AAdd( mapnames, HUSTR_E3M1 ) ; AAdd( mapnames, HUSTR_E3M2 ) ; AAdd( mapnames, HUSTR_E3M3 )
    AAdd( mapnames, HUSTR_E3M4 ) ; AAdd( mapnames, HUSTR_E3M5 ) ; AAdd( mapnames, HUSTR_E3M6 )
    AAdd( mapnames, HUSTR_E3M7 ) ; AAdd( mapnames, HUSTR_E3M8 ) ; AAdd( mapnames, HUSTR_E3M9 )
    AAdd( mapnames, HUSTR_E4M1 ) ; AAdd( mapnames, HUSTR_E4M2 ) ; AAdd( mapnames, HUSTR_E4M3 )
    AAdd( mapnames, HUSTR_E4M4 ) ; AAdd( mapnames, HUSTR_E4M5 ) ; AAdd( mapnames, HUSTR_E4M6 )
    AAdd( mapnames, HUSTR_E4M7 ) ; AAdd( mapnames, HUSTR_E4M8 ) ; AAdd( mapnames, HUSTR_E4M9 )
    AAdd( mapnames, "NEWLEVEL" ) ; AAdd( mapnames, "NEWLEVEL" ) ; AAdd( mapnames, "NEWLEVEL" )
    AAdd( mapnames, "NEWLEVEL" ) ; AAdd( mapnames, "NEWLEVEL" ) ; AAdd( mapnames, "NEWLEVEL" )
    AAdd( mapnames, "NEWLEVEL" ) ; AAdd( mapnames, "NEWLEVEL" ) ; AAdd( mapnames, "NEWLEVEL" )

    mapnames_commercial := {}
    AAdd( mapnames_commercial, HUSTR_1 )  ; AAdd( mapnames_commercial, HUSTR_2 )
    AAdd( mapnames_commercial, HUSTR_3 )  ; AAdd( mapnames_commercial, HUSTR_4 )
    AAdd( mapnames_commercial, HUSTR_5 )  ; AAdd( mapnames_commercial, HUSTR_6 )
    AAdd( mapnames_commercial, HUSTR_7 )  ; AAdd( mapnames_commercial, HUSTR_8 )
    AAdd( mapnames_commercial, HUSTR_9 )  ; AAdd( mapnames_commercial, HUSTR_10 )
    AAdd( mapnames_commercial, HUSTR_11 ) ; AAdd( mapnames_commercial, HUSTR_12 )
    AAdd( mapnames_commercial, HUSTR_13 ) ; AAdd( mapnames_commercial, HUSTR_14 )
    AAdd( mapnames_commercial, HUSTR_15 ) ; AAdd( mapnames_commercial, HUSTR_16 )
    AAdd( mapnames_commercial, HUSTR_17 ) ; AAdd( mapnames_commercial, HUSTR_18 )
    AAdd( mapnames_commercial, HUSTR_19 ) ; AAdd( mapnames_commercial, HUSTR_20 )
    AAdd( mapnames_commercial, HUSTR_21 ) ; AAdd( mapnames_commercial, HUSTR_22 )
    AAdd( mapnames_commercial, HUSTR_23 ) ; AAdd( mapnames_commercial, HUSTR_24 )
    AAdd( mapnames_commercial, HUSTR_25 ) ; AAdd( mapnames_commercial, HUSTR_26 )
    AAdd( mapnames_commercial, HUSTR_27 ) ; AAdd( mapnames_commercial, HUSTR_28 )
    AAdd( mapnames_commercial, HUSTR_29 ) ; AAdd( mapnames_commercial, HUSTR_30 )
    AAdd( mapnames_commercial, HUSTR_31 ) ; AAdd( mapnames_commercial, HUSTR_32 )
    AAdd( mapnames_commercial, PHUSTR_1 )  ; AAdd( mapnames_commercial, PHUSTR_2 )
    AAdd( mapnames_commercial, PHUSTR_3 )  ; AAdd( mapnames_commercial, PHUSTR_4 )
    AAdd( mapnames_commercial, PHUSTR_5 )  ; AAdd( mapnames_commercial, PHUSTR_6 )
    AAdd( mapnames_commercial, PHUSTR_7 )  ; AAdd( mapnames_commercial, PHUSTR_8 )
    AAdd( mapnames_commercial, PHUSTR_9 )  ; AAdd( mapnames_commercial, PHUSTR_10 )
    AAdd( mapnames_commercial, PHUSTR_11 ) ; AAdd( mapnames_commercial, PHUSTR_12 )
    AAdd( mapnames_commercial, PHUSTR_13 ) ; AAdd( mapnames_commercial, PHUSTR_14 )
    AAdd( mapnames_commercial, PHUSTR_15 ) ; AAdd( mapnames_commercial, PHUSTR_16 )
    AAdd( mapnames_commercial, PHUSTR_17 ) ; AAdd( mapnames_commercial, PHUSTR_18 )
    AAdd( mapnames_commercial, PHUSTR_19 ) ; AAdd( mapnames_commercial, PHUSTR_20 )
    AAdd( mapnames_commercial, PHUSTR_21 ) ; AAdd( mapnames_commercial, PHUSTR_22 )
    AAdd( mapnames_commercial, PHUSTR_23 ) ; AAdd( mapnames_commercial, PHUSTR_24 )
    AAdd( mapnames_commercial, PHUSTR_25 ) ; AAdd( mapnames_commercial, PHUSTR_26 )
    AAdd( mapnames_commercial, PHUSTR_27 ) ; AAdd( mapnames_commercial, PHUSTR_28 )
    AAdd( mapnames_commercial, PHUSTR_29 ) ; AAdd( mapnames_commercial, PHUSTR_30 )
    AAdd( mapnames_commercial, PHUSTR_31 ) ; AAdd( mapnames_commercial, PHUSTR_32 )
    AAdd( mapnames_commercial, THUSTR_1 )  ; AAdd( mapnames_commercial, THUSTR_2 )
    AAdd( mapnames_commercial, THUSTR_3 )  ; AAdd( mapnames_commercial, THUSTR_4 )
    AAdd( mapnames_commercial, THUSTR_5 )  ; AAdd( mapnames_commercial, THUSTR_6 )
    AAdd( mapnames_commercial, THUSTR_7 )  ; AAdd( mapnames_commercial, THUSTR_8 )
    AAdd( mapnames_commercial, THUSTR_9 )  ; AAdd( mapnames_commercial, THUSTR_10 )
    AAdd( mapnames_commercial, THUSTR_11 ) ; AAdd( mapnames_commercial, THUSTR_12 )
    AAdd( mapnames_commercial, THUSTR_13 ) ; AAdd( mapnames_commercial, THUSTR_14 )
    AAdd( mapnames_commercial, THUSTR_15 ) ; AAdd( mapnames_commercial, THUSTR_16 )
    AAdd( mapnames_commercial, THUSTR_17 ) ; AAdd( mapnames_commercial, THUSTR_18 )
    AAdd( mapnames_commercial, THUSTR_19 ) ; AAdd( mapnames_commercial, THUSTR_20 )
    AAdd( mapnames_commercial, THUSTR_21 ) ; AAdd( mapnames_commercial, THUSTR_22 )
    AAdd( mapnames_commercial, THUSTR_23 ) ; AAdd( mapnames_commercial, THUSTR_24 )
    AAdd( mapnames_commercial, THUSTR_25 ) ; AAdd( mapnames_commercial, THUSTR_26 )
    AAdd( mapnames_commercial, THUSTR_27 ) ; AAdd( mapnames_commercial, THUSTR_28 )
    AAdd( mapnames_commercial, THUSTR_29 ) ; AAdd( mapnames_commercial, THUSTR_30 )
    AAdd( mapnames_commercial, THUSTR_31 ) ; AAdd( mapnames_commercial, THUSTR_32 )
RETURN

STATIC FUNCTION PeekShort( cBuf, nPos )
    LOCAL n

    IF ValType( cBuf ) != "C" .OR. nPos < 1 .OR. nPos + 1 > Len( cBuf )
        RETURN 0
    ENDIF
    n := Asc( SubStr( cBuf, nPos, 1 ) ) + Asc( SubStr( cBuf, nPos + 1, 1 ) ) * 256
    IF n >= 32768
        n := n - 65536
    ENDIF
RETURN n

STATIC FUNCTION PatchHeight( patch )
    IF ValType( patch ) == "O"
        RETURN patch:height
    ENDIF
    IF ValType( patch ) == "C"
        RETURN PeekShort( patch, 3 )
    ENDIF
RETURN 0

STATIC FUNCTION HU_TitleY()
    MEMVAR hu_font
    IF Len( hu_font ) >= 1
        RETURN 167 - PatchHeight( hu_font[ 1 ] )
    ENDIF
RETURN 167

STATIC FUNCTION HU_InputY()
    MEMVAR hu_font
    IF Len( hu_font ) >= 1
        RETURN HU_MSGY + HU_MSGHEIGHT * ( PatchHeight( hu_font[ 1 ] ) + 1 )
    ENDIF
RETURN HU_MSGY + HU_MSGHEIGHT

STATIC FUNCTION MapTitle()
    LOCAL cS
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gameversion

    SWITCH logical_gamemission
    CASE doom
        cS := mapnames[ ( gameepisode - 1 ) * 9 + gamemap ]
        EXIT
    CASE doom2
        cS := mapnames_commercial[ gamemap ]
        EXIT
    CASE pack_plut
        cS := mapnames_commercial[ gamemap + 32 ]
        EXIT
    CASE pack_tnt
        cS := mapnames_commercial[ gamemap + 64 ]
        EXIT
    OTHERWISE
        cS := "Unknown level"
    ENDSWITCH

    IF gameversion == exe_chex
        cS := mapnames[ gamemap ]
    ENDIF
RETURN cS

FUNCTION HU_Init()
    LOCAL i
    LOCAL j
    LOCAL cBuf
    MEMVAR hu_font

    hu_font := {}
    j := HU_FONTSTART
    FOR i := 0 TO HU_FONTSIZE - 1
        cBuf := "STCFN" + PadL( hb_ntos( j ), 3, "0" )
        j := j + 1
        AAdd( hu_font, W_CacheLumpName( cBuf, PU_STATIC ) )
    NEXT
RETURN NIL

FUNCTION HU_Stop()
    headsupactive := .F.
RETURN NIL

FUNCTION HU_Start()
    LOCAL i
    LOCAL cS
    LOCAL nPos
    MEMVAR chat_on
    MEMVAR consoleplayer
    MEMVAR hu_font
    MEMVAR message_dontfuckwithme
    MEMVAR players

    IF headsupactive
        HU_Stop()
    ENDIF

    plr := players[ consoleplayer + 1 ]
    message_on := .F.
    message_dontfuckwithme := .F.
    message_nottobefuckedwith := .F.
    chat_on := .F.

    HUlib_initSText( w_message, HU_MSGX, HU_MSGY, HU_MSGHEIGHT, ;
        hu_font, HU_FONTSTART, {|| message_on } )

    HUlib_initTextLine( w_title, HU_TITLEX, HU_TitleY(), hu_font, HU_FONTSTART )

    cS := DEH_String( MapTitle() )
    FOR nPos := 1 TO Len( cS )
        HUlib_addCharToTextLine( w_title, SubStr( cS, nPos, 1 ) )
    NEXT

    HUlib_initIText( w_chat, HU_INPUTX, HU_InputY(), hu_font, HU_FONTSTART, {|| chat_on } )

    FOR i := 0 TO MAXPLAYERS - 1
        HUlib_initIText( w_inputbuffer[ i + 1 ], 0, 0, {}, 0, {|| always_off } )
    NEXT

    headsupactive := .T.
RETURN NIL

FUNCTION HU_Drawer()
    MEMVAR automapactive
    HUlib_drawSText( w_message )
    HUlib_drawIText( w_chat )
    IF automapactive
        HUlib_drawTextLine( w_title, .F. )
    ENDIF
RETURN NIL

FUNCTION HU_Erase()
    HUlib_eraseSText( w_message )
    HUlib_eraseIText( w_chat )
    HUlib_eraseTextLine( w_title )
RETURN NIL

FUNCTION HU_Ticker()
    LOCAL i
    LOCAL lRc
    LOCAL nC
    MEMVAR consoleplayer
    MEMVAR gamemode
    MEMVAR message_dontfuckwithme
    MEMVAR netgame
    MEMVAR player_names
    MEMVAR playeringame
    MEMVAR players
    MEMVAR showMessages

    IF message_counter != 0
        message_counter := message_counter - 1
        IF message_counter == 0
            message_on := .F.
            message_nottobefuckedwith := .F.
        ENDIF
    ENDIF

    IF showMessages != 0 .OR. message_dontfuckwithme
        IF plr != NIL .AND. ! Empty( plr:message ) ;
             .AND. ( ! message_nottobefuckedwith .OR. message_dontfuckwithme )
            HUlib_addMessageToSText( w_message, NIL, plr:message )
            plr:message := NIL
            message_on := .T.
            message_counter := HU_MSGTIMEOUT
            message_nottobefuckedwith := message_dontfuckwithme
            message_dontfuckwithme := .F.
        ENDIF
    ENDIF

    IF netgame
        FOR i := 0 TO MAXPLAYERS - 1
            IF ! playeringame[ i + 1 ]
                LOOP
            ENDIF
            IF i != consoleplayer
                nC := players[ i + 1 ]:cmd:chatchar
                IF nC != 0
                    IF nC <= HU_BROADCAST
                        chat_dest[ i + 1 ] := nC
                    ELSE
                        lRc := HUlib_keyInIText( w_inputbuffer[ i + 1 ], nC )
                        IF lRc .AND. nC == KEY_ENTER
                            IF w_inputbuffer[ i + 1 ]:l:len != 0 ;
                                 .AND. ( chat_dest[ i + 1 ] == consoleplayer + 1 ;
                                 .OR. chat_dest[ i + 1 ] == HU_BROADCAST )
                                HUlib_addMessageToSText( w_message, ;
                                    DEH_String( player_names[ i + 1 ] ), ;
                                    w_inputbuffer[ i + 1 ]:l:l )
                                message_nottobefuckedwith := .T.
                                message_on := .T.
                                message_counter := HU_MSGTIMEOUT
                                IF gamemode == commercial
                                    S_StartSound( NIL, sfx_radio )
                                ELSE
                                    S_StartSound( NIL, sfx_tink )
                                ENDIF
                            ENDIF
                            HUlib_resetIText( w_inputbuffer[ i + 1 ] )
                        ENDIF
                    ENDIF
                    players[ i + 1 ]:cmd:chatchar := 0
                ENDIF
            ENDIF
        NEXT
    ENDIF
RETURN NIL

FUNCTION HU_queueChatChar( c )
    IF ValType( c ) == "C"
        c := Asc( c )
    ENDIF
    IF ( ( ( chat_head + 1 ) & ( QUEUESIZE - 1 ) ) == chat_tail )
        IF plr != NIL
            plr:message := DEH_String( HUSTR_MSGU )
        ENDIF
    ELSE
        chatchars[ chat_head + 1 ] := ( c & 0xFF )
        chat_head := ( ( chat_head + 1 ) & ( QUEUESIZE - 1 ) )
    ENDIF
RETURN NIL

FUNCTION HU_dequeueChatChar()
    LOCAL nC

    IF chat_head != chat_tail
        nC := chatchars[ chat_tail + 1 ]
        chat_tail := ( ( chat_tail + 1 ) & ( QUEUESIZE - 1 ) )
        RETURN nC
    ENDIF
RETURN 0

FUNCTION HU_Responder( ev )
    LOCAL lEat := .F.
    LOCAL c
    LOCAL i
    LOCAL nPlayers
    LOCAL cMacro
    MEMVAR chat_macros
    MEMVAR chat_on
    MEMVAR consoleplayer
    MEMVAR key_message_refresh
    MEMVAR key_multi_msg
    MEMVAR key_multi_msgplayer
    MEMVAR netgame
    MEMVAR playeringame

    nPlayers := 0
    FOR i := 0 TO MAXPLAYERS - 1
        IF playeringame[ i + 1 ]
            nPlayers := nPlayers + 1
        ENDIF
    NEXT

    IF ev:data1 == KEY_RSHIFT
        RETURN .F.
    ELSEIF ev:data1 == KEY_RALT .OR. ev:data1 == KEY_LALT
        altdown := ( ev:type == ev_keydown )
        RETURN .F.
    ENDIF

    IF ev:type != ev_keydown
        RETURN .F.
    ENDIF

    IF ! chat_on
        IF ev:data1 == key_message_refresh
            message_on := .T.
            message_counter := HU_MSGTIMEOUT
            lEat := .T.
        ELSEIF netgame .AND. ev:data2 == key_multi_msg
            chat_on := .T.
            lEat := .T.
            HUlib_resetIText( w_chat )
            HU_queueChatChar( HU_BROADCAST )
        ELSEIF netgame .AND. nPlayers > 2
            FOR i := 0 TO MAXPLAYERS - 1
                IF ev:data2 == key_multi_msgplayer[ i + 1 ]
                    IF playeringame[ i + 1 ] .AND. i != consoleplayer
                        chat_on := .T.
                        lEat := .T.
                        HUlib_resetIText( w_chat )
                        HU_queueChatChar( i + 1 )
                        EXIT
                    ELSEIF i == consoleplayer
                        num_nobrainers := num_nobrainers + 1
                        IF plr != NIL
                            IF num_nobrainers < 3
                                plr:message := DEH_String( HUSTR_TALKTOSELF1 )
                            ELSEIF num_nobrainers < 6
                                plr:message := DEH_String( HUSTR_TALKTOSELF2 )
                            ELSEIF num_nobrainers < 9
                                plr:message := DEH_String( HUSTR_TALKTOSELF3 )
                            ELSEIF num_nobrainers < 32
                                plr:message := DEH_String( HUSTR_TALKTOSELF4 )
                            ELSE
                                plr:message := DEH_String( HUSTR_TALKTOSELF5 )
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            NEXT
        ENDIF
    ELSE
        IF altdown
            c := ev:data1 - Asc( "0" )
            IF c < 0 .OR. c > 9
                RETURN .F.
            ENDIF
            cMacro := chat_macros[ c + 1 ]
            HU_queueChatChar( KEY_ENTER )
            FOR i := 1 TO Len( cMacro )
                HU_queueChatChar( SubStr( cMacro, i, 1 ) )
            NEXT
            HU_queueChatChar( KEY_ENTER )
            chat_on := .F.
            lastmessage := cMacro
            IF plr != NIL
                plr:message := lastmessage
            ENDIF
            lEat := .T.
        ELSE
            c := ev:data2
            lEat := HUlib_keyInIText( w_chat, c )
            IF lEat
                HU_queueChatChar( c )
            ENDIF
            IF c == KEY_ENTER
                chat_on := .F.
                IF w_chat:l:len != 0
                    lastmessage := w_chat:l:l
                    IF plr != NIL
                        plr:message := lastmessage
                    ENDIF
                ENDIF
            ELSEIF c == KEY_ESCAPE
                chat_on := .F.
            ENDIF
        ENDIF
    ENDIF
RETURN lEat
