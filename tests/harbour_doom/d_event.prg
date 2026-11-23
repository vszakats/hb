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

STATIC events := {}
STATIC eventhead
STATIC eventtail

#include "d_event.ch"

CLASS event_t
    DATA type
    DATA data1
    DATA data2
    DATA data3
    DATA data4
    METHOD New()
ENDCLASS

#define MAXEVENTS 64


METHOD New() CLASS event_t
    ::type  := 0
    ::data1 := 0
    ::data2 := 0
    ::data3 := 0
    ::data4 := 0
RETURN Self

INIT PROCEDURE init_d_event
    LOCAL i

    events := {}
    FOR i := 1 TO MAXEVENTS
        AAdd( events, event_t():New() )
    NEXT

    eventhead := 0
    eventtail := 0
RETURN

FUNCTION D_PostEvent( ev )
    LOCAL dest

    dest := events[ eventhead + 1 ]
    dest:type  := ev:type
    dest:data1 := ev:data1
    dest:data2 := ev:data2
    dest:data3 := ev:data3
    dest:data4 := ev:data4

    eventhead := ( eventhead + 1 ) % MAXEVENTS
RETURN NIL

FUNCTION D_PopEvent()
    LOCAL result

    IF eventtail == eventhead
        RETURN NIL
    ENDIF

    result := events[ eventtail + 1 ]

    eventtail := ( eventtail + 1 ) % MAXEVENTS
RETURN result
