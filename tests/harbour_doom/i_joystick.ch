/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __I_JOYSTICK__
#define __I_JOYSTICK__

#define NUM_VIRTUAL_BUTTONS 10

#define BUTTON_AXIS 0x10000

#define IS_BUTTON_AXIS( axis ) ( (axis) >= 0 .AND. ( ( (axis) & BUTTON_AXIS ) != 0 ) )

#define BUTTON_AXIS_NEG( axis )  ( (axis) & 0xFF )
#define BUTTON_AXIS_POS( axis )  ( Int( (axis) / 256 ) & 0xFF )

#define CREATE_BUTTON_AXIS( neg, pos ) ( ( BUTTON_AXIS | (neg) ) | ( (pos) * 256 ) )

#define HAT_AXIS 0x20000

#define IS_HAT_AXIS( axis ) ( (axis) >= 0 .AND. ( ( (axis) & HAT_AXIS ) != 0 ) )

#define HAT_AXIS_HAT( axis )       ( (axis) & 0xFF )
#define HAT_AXIS_DIRECTION( axis ) ( Int( (axis) / 256 ) & 0xFF )

#define CREATE_HAT_AXIS( hat, direction ) ( ( HAT_AXIS | (hat) ) | ( (direction) * 256 ) )

#define HAT_AXIS_HORIZONTAL 1
#define HAT_AXIS_VERTICAL   2

#endif
