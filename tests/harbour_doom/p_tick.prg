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

#include "p_tick.ch"
#include "p_local.ch"
#include "d_player.ch"
#include "doomstat.ch"
#include "doomdef.ch"

/* Full DEMO1 checksum log: harbour_doom/sync_demo1.log
   fields: tic health armor x y z floorz momx momy angle nAlive sxSum prnd weapon mislammo
   Regenerate with: Doom_hb.exe -iwad ..\DOOM1.WAD -timedemo demo1
*/
STATIC sSyncH := NIL
STATIC sSyncPath := "c:\projetos\git\doomgeneric\doomgeneric\doom_minimal\harbour_doom\sync_demo1.log"


PROCEDURE P_InitThinkers()
    MEMVAR thinkercap
    thinkercap:prev := thinkercap
    thinkercap:next := thinkercap
RETURN

PROCEDURE P_AddThinker( thinker )
    MEMVAR thinkercap
    thinkercap:prev:next := thinker
    thinker:next := thinkercap
    thinker:prev := thinkercap:prev
    thinkercap:prev := thinker
RETURN

PROCEDURE P_RemoveThinker( thinker )
    thinker:function:acv := -1
RETURN

PROCEDURE P_AllocateThinker( thinker )
    HB_SYMBOL_UNUSED( thinker )
RETURN

STATIC FUNCTION IfaceCall( xFun, x1, x2 )
    LOCAL nArgs := PCount() - 1

    IF xFun == NIL
        RETURN NIL
    ENDIF

    IF ValType( xFun ) == "B"
        IF nArgs <= 0
            RETURN Eval( xFun )
        ELSEIF nArgs == 1
            RETURN Eval( xFun, x1 )
        ENDIF
        RETURN Eval( xFun, x1, x2 )
    ENDIF

    IF ValType( xFun ) == "C"
        IF nArgs <= 0
            RETURN &( xFun )()
        ELSEIF nArgs == 1
            RETURN &( xFun )( x1 )
        ENDIF
        RETURN &( xFun )( x1, x2 )
    ENDIF
RETURN NIL

STATIC FUNCTION IsRemovedThinker( thinker )
    LOCAL x

    IF thinker == NIL .OR. thinker:function == NIL
        RETURN .F.
    ENDIF
    x := thinker:function:acv
RETURN ValType( x ) == "N" .AND. x == -1

STATIC PROCEDURE RunThinkerFn( currentthinker )
    LOCAL x

    IF currentthinker:function == NIL
        RETURN
    ENDIF
    x := currentthinker:function:acp1
    IF ValType( x ) == "B"
        Eval( x )
    ELSEIF ValType( x ) == "C" .AND. ! Empty( x )
        IF currentthinker:owner != NIL
            IfaceCall( x, currentthinker:owner )
        ELSE
            IfaceCall( x, currentthinker )
        ENDIF
    ENDIF
RETURN

PROCEDURE P_RunThinkers()
    LOCAL currentthinker
    LOCAL nextthinker
    MEMVAR thinkercap

    currentthinker := thinkercap:next
    DO WHILE currentthinker != NIL .AND. !( currentthinker == thinkercap )
        nextthinker := currentthinker:next
        IF IsRemovedThinker( currentthinker )
            IF currentthinker:next != NIL
                currentthinker:next:prev := currentthinker:prev
            ENDIF
            IF currentthinker:prev != NIL
                currentthinker:prev:next := currentthinker:next
            ENDIF
        ELSE
            RunThinkerFn( currentthinker )
        ENDIF
        currentthinker := nextthinker
    ENDDO
RETURN

PROCEDURE P_Ticker()
    LOCAL i
    MEMVAR consoleplayer
    MEMVAR demoplayback
    MEMVAR leveltime
    MEMVAR menuactive
    MEMVAR netgame
    MEMVAR paused
    MEMVAR playeringame
    MEMVAR players

    IF paused
        RETURN
    ENDIF

    IF ! netgame ;
         .AND. menuactive != 0 ;
         .AND. ! demoplayback ;
         .AND. players[ consoleplayer + 1 ]:viewz != 1
        RETURN
    ENDIF

    FOR i := 0 TO MAXPLAYERS - 1
        IF playeringame[ i + 1 ]
            P_PlayerThink( players[ i + 1 ] )
        ENDIF
    NEXT

    P_RunThinkers()
    P_UpdateSpecials()
    P_RespawnSpecials()

    IF demoplayback
        SyncLog()
    ENDIF

    leveltime++
RETURN

PROCEDURE SyncFlush()
    IF sSyncH != NIL .AND. sSyncH >= 0
        FClose( sSyncH )
        sSyncH := -1
    ENDIF
RETURN

STATIC PROCEDURE SyncLog()
    LOCAL mo
    LOCAL p
    LOCAL c
    LOCAL th
    LOCAL o
    LOCAL n := 0
    LOCAL sx := 0
    MEMVAR consoleplayer
    MEMVAR leveltime
    MEMVAR players
    MEMVAR thinkercap

    IF sSyncH != NIL .AND. sSyncH < 0
        RETURN
    ENDIF
    IF sSyncH == NIL
        sSyncH := FCreate( sSyncPath )
        IF sSyncH < 0
            sSyncH := -1
            RETURN
        ENDIF
    ENDIF
    p := players[ consoleplayer + 1 ]
    mo := p:mo
    IF mo == NIL
        RETURN
    ENDIF
    th := thinkercap:next
    DO WHILE th != NIL .AND. !( th == thinkercap )
        o := th:owner
        IF o != NIL .AND. o:ClassName() == "MOBJ_T"
            IF ( o:flags & 4194304 ) != 0 .AND. o:health > 0
                n++
                sx += Int( o:x / 65536 )
            ENDIF
        ENDIF
        th := th:next
    ENDDO
    c := hb_ntos( leveltime ) + " " + hb_ntos( Int( p:health ) ) + " " + hb_ntos( Int( p:armorpoints ) ) ;
       + " " + hb_ntos( Int( mo:x / 65536 ) ) + " " + hb_ntos( Int( mo:y / 65536 ) ) ;
       + " " + hb_ntos( Int( mo:z / 65536 ) ) + " " + hb_ntos( Int( mo:floorz / 65536 ) ) ;
       + " " + hb_ntos( mo:momx ) + " " + hb_ntos( mo:momy ) + " " + hb_ntos( mo:angle ) ;
       + " " + hb_ntos( n ) + " " + hb_ntos( sx ) + " " + hb_ntos( P_PrndIndex() ) ;
       + " " + hb_ntos( p:readyweapon ) + " " + hb_ntos( p:ammo[ am_misl + 1 ] ) + Chr( 10 )
    FWrite( sSyncH, c )
RETURN

INIT PROCEDURE init_p_tick
    PUBLIC thinkercap
    PUBLIC leveltime

    thinkercap := thinker_t():New()
    thinkercap:prev := thinkercap
    thinkercap:next := thinkercap
    leveltime := 0
RETURN
