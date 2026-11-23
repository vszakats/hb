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

#include "p_lights.ch"

CLASS fireflicker_t
    DATA thinker
    DATA thinkfn
    DATA sector
    DATA count
    DATA maxlight
    DATA minlight
    METHOD New()
ENDCLASS
CLASS lightflash_t
    DATA thinker
    DATA thinkfn
    DATA sector
    DATA count
    DATA maxlight
    DATA minlight
    DATA maxtime
    DATA mintime
    METHOD New()
ENDCLASS
CLASS strobe_t
    DATA thinker
    DATA thinkfn
    DATA sector
    DATA count
    DATA minlight
    DATA maxlight
    DATA darktime
    DATA brighttime
    METHOD New()
ENDCLASS
CLASS glow_t
    DATA thinker
    DATA thinkfn
    DATA sector
    DATA minlight
    DATA maxlight
    DATA direction
    METHOD New()
ENDCLASS

METHOD New() CLASS fireflicker_t
    ::thinker  := thinker_t():New()
    ::thinkfn  := ""
    ::sector   := NIL
    ::count    := 0
    ::maxlight := 0
    ::minlight := 0
RETURN Self

METHOD New() CLASS lightflash_t
    ::thinker  := thinker_t():New()
    ::thinkfn  := ""
    ::sector   := NIL
    ::count    := 0
    ::maxlight := 0
    ::minlight := 0
    ::maxtime  := 0
    ::mintime  := 0
RETURN Self

METHOD New() CLASS strobe_t
    ::thinker    := thinker_t():New()
    ::thinkfn    := ""
    ::sector     := NIL
    ::count      := 0
    ::minlight   := 0
    ::maxlight   := 0
    ::darktime   := 0
    ::brighttime := 0
RETURN Self

METHOD New() CLASS glow_t
    ::thinker   := thinker_t():New()
    ::thinkfn   := ""
    ::sector    := NIL
    ::minlight  := 0
    ::maxlight  := 0
    ::direction := 0
RETURN Self

STATIC PROCEDURE BindLightThink( o, cName, bBlock )
    o:thinkfn := cName
    o:thinker:thinkfn := cName
    o:thinker:owner := o
    o:thinker:function:acp1 := bBlock
RETURN

FUNCTION T_FireFlicker( flick )
    LOCAL amount
    flick:count := flick:count - 1
    IF flick:count != 0
        RETURN NIL
    ENDIF
    amount := ( P_Random() & 3 ) * 16
    IF flick:sector:lightlevel - amount < flick:minlight
        flick:sector:lightlevel := flick:minlight
    ELSE
        flick:sector:lightlevel := flick:maxlight - amount
    ENDIF
    flick:count := 4
RETURN NIL

FUNCTION P_SpawnFireFlicker( sector )
    LOCAL flick
    LOCAL oFlick
    sector:special := 0
    flick := fireflicker_t():New()
    oFlick := flick
    BindLightThink( flick, "T_FireFlicker", {|| T_FireFlicker( oFlick ) } )
    P_AddThinker( flick:thinker )
    flick:sector := sector
    flick:maxlight := sector:lightlevel
    flick:minlight := P_FindMinSurroundingLight( sector, sector:lightlevel ) + 16
    flick:count := 4
RETURN NIL

FUNCTION T_LightFlash( flash )
    flash:count := flash:count - 1
    IF flash:count != 0
        RETURN NIL
    ENDIF
    IF flash:sector:lightlevel == flash:maxlight
        flash:sector:lightlevel := flash:minlight
        flash:count := ( P_Random() & flash:mintime ) + 1
    ELSE
        flash:sector:lightlevel := flash:maxlight
        flash:count := ( P_Random() & flash:maxtime ) + 1
    ENDIF
RETURN NIL

FUNCTION P_SpawnLightFlash( sector )
    LOCAL flash
    LOCAL oFlash
    sector:special := 0
    flash := lightflash_t():New()
    oFlash := flash
    BindLightThink( flash, "T_LightFlash", {|| T_LightFlash( oFlash ) } )
    P_AddThinker( flash:thinker )
    flash:sector := sector
    flash:maxlight := sector:lightlevel
    flash:minlight := P_FindMinSurroundingLight( sector, sector:lightlevel )
    flash:maxtime := 64
    flash:mintime := 7
    flash:count := ( P_Random() & flash:maxtime ) + 1
RETURN NIL

FUNCTION T_StrobeFlash( flash )
    flash:count := flash:count - 1
    IF flash:count != 0
        RETURN NIL
    ENDIF
    IF flash:sector:lightlevel == flash:minlight
        flash:sector:lightlevel := flash:maxlight
        flash:count := flash:brighttime
    ELSE
        flash:sector:lightlevel := flash:minlight
        flash:count := flash:darktime
    ENDIF
RETURN NIL

FUNCTION P_SpawnStrobeFlash( sector, fastOrSlow, inSync )
    LOCAL flash
    LOCAL oFlash
    flash := strobe_t():New()
    oFlash := flash
    P_AddThinker( flash:thinker )
    flash:sector := sector
    flash:darktime := fastOrSlow
    flash:brighttime := STROBEBRIGHT
    BindLightThink( flash, "T_StrobeFlash", {|| T_StrobeFlash( oFlash ) } )
    flash:maxlight := sector:lightlevel
    flash:minlight := P_FindMinSurroundingLight( sector, sector:lightlevel )
    IF flash:minlight == flash:maxlight
        flash:minlight := 0
    ENDIF
    sector:special := 0
    IF inSync == 0
        flash:count := ( P_Random() & 7 ) + 1
    ELSE
        flash:count := 1
    ENDIF
RETURN NIL

FUNCTION EV_StartLightStrobing( line )
    LOCAL secnum
    LOCAL sec
    MEMVAR sectors
    secnum := -1
    DO WHILE .T.
        secnum := P_FindSectorFromLineTag( line, secnum )
        IF secnum < 0
            EXIT
        ENDIF
        sec := sectors[ secnum + 1 ]
        IF sec:specialdata != NIL
            LOOP
        ENDIF
        P_SpawnStrobeFlash( sec, SLOWDARK, 0 )
    ENDDO
RETURN NIL

FUNCTION EV_TurnTagLightsOff( line )
    LOCAL i
    LOCAL j
    LOCAL min
    LOCAL sector
    LOCAL tsec
    LOCAL templine
    MEMVAR numsectors
    MEMVAR sectors
    FOR j := 0 TO numsectors - 1
        sector := sectors[ j + 1 ]
        IF sector:tag == line:tag
            min := sector:lightlevel
            FOR i := 0 TO sector:linecount - 1
                templine := sector:lines[ i + 1 ]
                tsec := getNextSector( templine, sector )
                IF tsec == NIL
                    LOOP
                ENDIF
                IF tsec:lightlevel < min
                    min := tsec:lightlevel
                ENDIF
            NEXT
            sector:lightlevel := min
        ENDIF
    NEXT
RETURN NIL

FUNCTION EV_LightTurnOn( line, bright )
    LOCAL i
    LOCAL j
    LOCAL sector
    LOCAL temp
    LOCAL templine
    LOCAL nBright
    MEMVAR numsectors
    MEMVAR sectors
    FOR i := 0 TO numsectors - 1
        sector := sectors[ i + 1 ]
        IF sector:tag == line:tag
            nBright := bright
            IF nBright == 0
                FOR j := 0 TO sector:linecount - 1
                    templine := sector:lines[ j + 1 ]
                    temp := getNextSector( templine, sector )
                    IF temp == NIL
                        LOOP
                    ENDIF
                    IF temp:lightlevel > nBright
                        nBright := temp:lightlevel
                    ENDIF
                NEXT
            ENDIF
            sector:lightlevel := nBright
        ENDIF
    NEXT
RETURN NIL

FUNCTION T_Glow( g )
    SWITCH g:direction
    CASE -1
        g:sector:lightlevel -= GLOWSPEED
        IF g:sector:lightlevel <= g:minlight
            g:sector:lightlevel += GLOWSPEED
            g:direction := 1
        ENDIF
        EXIT
    CASE 1
        g:sector:lightlevel += GLOWSPEED
        IF g:sector:lightlevel >= g:maxlight
            g:sector:lightlevel -= GLOWSPEED
            g:direction := -1
        ENDIF
        EXIT
    ENDSWITCH
RETURN NIL

FUNCTION P_SpawnGlowingLight( sector )
    LOCAL g
    LOCAL oG
    g := glow_t():New()
    oG := g
    P_AddThinker( g:thinker )
    g:sector := sector
    g:minlight := P_FindMinSurroundingLight( sector, sector:lightlevel )
    g:maxlight := sector:lightlevel
    BindLightThink( g, "T_Glow", {|| T_Glow( oG ) } )
    g:direction := -1
    sector:special := 0
RETURN NIL
