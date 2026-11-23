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

#include "dstrings.ch"

INIT PROCEDURE init_dstrings

    PUBLIC doom1_endmsg
    PUBLIC doom2_endmsg

    doom1_endmsg := {}
    AAdd( doom1_endmsg, e"are you sure you want to\nquit this great game?" )
    AAdd( doom1_endmsg, e"please don't leave, there's more\ndemons to toast!" )
    AAdd( doom1_endmsg, e"let's beat it -- this is turning\ninto a bloodbath!" )
    AAdd( doom1_endmsg, e"i wouldn't leave if i were you.\ndos is much worse." )
    AAdd( doom1_endmsg, e"you're trying to say you like dos\nbetter than me, right?" )
    AAdd( doom1_endmsg, e"don't leave yet -- there's a\ndemon around that corner!" )
    AAdd( doom1_endmsg, e"ya know, next time you come in here\ni'm gonna toast ya." )
    AAdd( doom1_endmsg, e"go ahead and leave. see if i care." )

    doom2_endmsg := {}
    AAdd( doom2_endmsg, e"are you sure you want to\nquit this great game?" )
    AAdd( doom2_endmsg, e"you want to quit?\nthen, thou hast lost an eighth!" )
    AAdd( doom2_endmsg, e"don't go now, there's a \ndimensional shambler waiting\nat the dos prompt!" )
    AAdd( doom2_endmsg, e"get outta here and go back\nto your boring programs." )
    AAdd( doom2_endmsg, e"if i were your boss, i'd \n deathmatch ya in a minute!" )
    AAdd( doom2_endmsg, e"look, bud. you leave now\nand you forfeit your body count!" )
    AAdd( doom2_endmsg, e"just leave. when you come\nback, i'll be waiting with a bat." )
    AAdd( doom2_endmsg, e"you're lucky i don't smack\nyou for thinking about leaving." )
RETURN
