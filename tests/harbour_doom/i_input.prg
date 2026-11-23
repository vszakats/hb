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

STATIC shiftdown := 0
STATIC at_to_doom := {}
STATIC shiftxform := {}

#include "doomkeys.ch"
#include "d_event.ch"
#include "doomtype.ch"
#include "i_input.ch"


INIT PROCEDURE init_i_input

    PUBLIC vanilla_keyboard_mapping

    vanilla_keyboard_mapping := 1
    shiftdown := 0

    at_to_doom := { ;
        0, 27, 49, 50, 51, 52, 53, 54, 55, 56, 57, 48, 45, 61, 127, 9, ;
        113, 119, 101, 114, 116, 121, 117, 105, 111, 112, 91, 93, 13, 163, 97, 115, ;
        100, 102, 103, 104, 106, 107, 108, 59, 39, 96, 182, 92, 122, 120, 99, 118, ;
        98, 110, 109, 44, 46, 47, 182, 42, 184, 162, 186, 187, 188, 189, 190, 191, ;
        192, 193, 194, 195, 196, 197, 0, 0, 0, 0, 0, 0, 0, 0, ;
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ;
        0, 0, 0, 0, 0, 0, 0, 173, 0, 172, 174, 0, 175, 0, 0, 0, ;
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 163 }

    shiftxform := { ;
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ;
        16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, ;
        32, 33, 34, 35, 36, 37, 38, 34, 40, 41, 42, 43, 60, 95, 62, 63, ;
        41, 33, 64, 35, 36, 37, 94, 38, 42, 40, 58, 58, 60, 43, 62, 63, ;
        64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, ;
        80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 33, 93, 34, 95, ;
        39, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, ;
        80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 123, 124, 125, 126, 127 }
RETURN

STATIC FUNCTION TranslateKey( key )
RETURN ( key & 0xFF )

STATIC FUNCTION GetTypedChar( key )
    key := TranslateKey( key )
    IF shiftdown > 0
        IF key >= 0 .AND. key < Len( shiftxform )
            key := ( shiftxform[ key + 1 ] & 0xFF )
        ELSE
            key := 0
        ENDIF
    ENDIF
RETURN ( key & 0xFF )

STATIC PROCEDURE UpdateShiftStatus( pressed, key )
    LOCAL nChange

    IF pressed != 0
        nChange := 1
    ELSE
        nChange := -1
    ENDIF

    IF ( key & 0xFF ) == KEY_RSHIFT
        shiftdown := shiftdown + nChange
    ENDIF
RETURN

FUNCTION I_GetEvent()
    LOCAL event
    LOCAL pressed := 0
    LOCAL key := 0

    event := event_t():New()
    DO WHILE DG_GetKey( @pressed, @key ) != 0
        UpdateShiftStatus( pressed, key )
        IF pressed != 0
            event:type := ev_keydown
            event:data1 := TranslateKey( key )
            event:data2 := GetTypedChar( key )
            IF event:data1 != 0
                D_PostEvent( event )
            ENDIF
        ELSE
            event:type := ev_keyup
            event:data1 := TranslateKey( key )
            event:data2 := 0
            IF event:data1 != 0
                D_PostEvent( event )
            ENDIF
            EXIT
        ENDIF
    ENDDO
RETURN NIL

FUNCTION I_InitInput()
RETURN NIL
