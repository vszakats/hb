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

STATIC usejoystick := 0
STATIC joystick_index := -1
STATIC joystick_x_axis := 0
STATIC joystick_x_invert := 0
STATIC joystick_y_axis := 1
STATIC joystick_y_invert := 0
STATIC joystick_strafe_axis := -1
STATIC joystick_strafe_invert := 0
STATIC joystick_physical_buttons := {}

#include "i_joystick.ch"

#define DEAD_ZONE Int( 32768 / 3 )


INIT PROCEDURE init_i_joystick
    LOCAL i

    usejoystick := 0
    joystick_index := -1
    joystick_x_axis := 0
    joystick_x_invert := 0
    joystick_y_axis := 1
    joystick_y_invert := 0
    joystick_strafe_axis := -1
    joystick_strafe_invert := 0

    joystick_physical_buttons := {}
    FOR i := 0 TO NUM_VIRTUAL_BUTTONS - 1
        AAdd( joystick_physical_buttons, i )
    NEXT
RETURN

FUNCTION I_ShutdownJoystick()
RETURN NIL

FUNCTION I_InitJoystick()
RETURN NIL

FUNCTION I_UpdateJoystick()
RETURN NIL

FUNCTION I_BindJoystickVariables()
    LOCAL i
    LOCAL cName

    M_BindVariable( "use_joystick",           @usejoystick )
    M_BindVariable( "joystick_index",         @joystick_index )
    M_BindVariable( "joystick_x_axis",        @joystick_x_axis )
    M_BindVariable( "joystick_y_axis",        @joystick_y_axis )
    M_BindVariable( "joystick_strafe_axis",   @joystick_strafe_axis )
    M_BindVariable( "joystick_x_invert",      @joystick_x_invert )
    M_BindVariable( "joystick_y_invert",      @joystick_y_invert )
    M_BindVariable( "joystick_strafe_invert", @joystick_strafe_invert )

    FOR i := 0 TO NUM_VIRTUAL_BUTTONS - 1
        cName := "joystick_physical_button" + hb_ntos( i )
        M_BindVariable( cName, @joystick_physical_buttons[ i + 1 ] )
    NEXT
RETURN NIL
