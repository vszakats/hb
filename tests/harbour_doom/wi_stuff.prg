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

#translate ( <exp1> | <exp2> ) => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> ) => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> ) => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

STATIC lnodes
STATIC anims
STATIC NUMANIMS

#include "wi_stuff.ch"

CLASS wi_point_t
    DATA x
    DATA y
    METHOD New()
ENDCLASS
CLASS wi_anim_t
    DATA type
    DATA period
    DATA nanims
    DATA loc
    DATA data1
    DATA data2
    DATA p
    DATA nexttic
    DATA lastdrawn
    DATA ctr
    DATA state
    METHOD New()
ENDCLASS
#include "doomdef.ch"
#include "d_player.ch"
#include "sounds.ch"
#include "z_zone.ch"
#include "i_video.ch"
#include "d_event.ch"
#include "i_swap.ch"
#include "doomstat.ch"

#define NUMEPISODES       4
#define NUMMAPS           9
#define WI_TITLEY         2
#define WI_SPACINGY       33
#define SP_STATSX         50
#define SP_STATSY         50
#define SP_TIMEX          16
#define SP_TIMEY          ( SCREENHEIGHT - 32 )
#define NG_STATSY         50
#define NG_SPACINGX       64
#define DM_MATRIXX        42
#define DM_MATRIXY        68
#define DM_SPACINGX       40
#define DM_TOTALSX        269
#define DM_KILLERSX       10
#define DM_KILLERSY       100
#define DM_VICTIMSX       5
#define DM_VICTIMSY       50
#define SHOWNEXTLOCDELAY  4


METHOD New() CLASS wi_point_t
    ::x := 0
    ::y := 0
RETURN Self

METHOD New() CLASS wi_anim_t
    ::type := ANIM_ALWAYS
    ::period := 0
    ::nanims := 0
    ::loc := wi_point_t():New()
    ::data1 := 0
    ::data2 := 0
    ::p := Array( 3 )
    ::nexttic := 0
    ::lastdrawn := 0
    ::ctr := 0
    ::state := 0
RETURN Self

STATIC FUNCTION Point( x, y )
    LOCAL p := wi_point_t():New()
    p:x := x
    p:y := y
RETURN p

STATIC FUNCTION Anim( type, period, nanims, x, y, data1 )
    LOCAL a := wi_anim_t():New()
    a:type := type
    a:period := period
    a:nanims := nanims
    a:loc := Point( x, y )
    a:data1 := data1
RETURN a

STATIC FUNCTION PatchWord( patch, offset )
    LOCAL n
    IF ValType( patch ) == "C"
        n := Asc( SubStr( patch, offset, 1 ) ) + ;
             Asc( SubStr( patch, offset + 1, 1 ) ) * 256
        RETURN iif( n >= 32768, n - 65536, n )
    ENDIF
RETURN 0

STATIC FUNCTION PatchW( patch )
    IF ValType( patch ) == "O"
        RETURN SHORT( patch:width )
    ENDIF
RETURN PatchWord( patch, 1 )

STATIC FUNCTION PatchH( patch )
    IF ValType( patch ) == "O"
        RETURN SHORT( patch:height )
    ENDIF
RETURN PatchWord( patch, 3 )

STATIC FUNCTION PatchLeft( patch )
    IF ValType( patch ) == "O"
        RETURN SHORT( patch:leftoffset )
    ENDIF
RETURN PatchWord( patch, 5 )

STATIC FUNCTION PatchTop( patch )
    IF ValType( patch ) == "O"
        RETURN SHORT( patch:topoffset )
    ENDIF
RETURN PatchWord( patch, 7 )

STATIC FUNCTION PercentValue( value, maximum )
RETURN Int( value * 100 / maximum )

PROCEDURE WI_slamBackground()
    V_DrawPatch( 0, 0, background )
RETURN

FUNCTION WI_Responder( ev )
    HB_SYMBOL_UNUSED( ev )
RETURN .F.

PROCEDURE WI_drawLF()
    LOCAL y := WI_TITLEY
    LOCAL patch
    MEMVAR gamemode

    IF gamemode != commercial .OR. wbs:last < NUMCMAPS
        patch := lnames[ wbs:last + 1 ]
        V_DrawPatch( Int( ( SCREENWIDTH - PatchW( patch ) ) / 2 ), y, patch )
        y += Int( 5 * PatchH( patch ) / 4 )
        V_DrawPatch( Int( ( SCREENWIDTH - PatchW( finished ) ) / 2 ), y, finished )
    ENDIF
RETURN

PROCEDURE WI_drawEL()
    LOCAL y := WI_TITLEY
    LOCAL patch := lnames[ wbs:next + 1 ]

    V_DrawPatch( Int( ( SCREENWIDTH - PatchW( entering ) ) / 2 ), y, entering )
    y += Int( 5 * PatchH( patch ) / 4 )
    V_DrawPatch( Int( ( SCREENWIDTH - PatchW( patch ) ) / 2 ), y, patch )
RETURN

PROCEDURE WI_drawOnLnode( n, patches )
    LOCAL i := 1
    LOCAL left
    LOCAL top
    LOCAL right
    LOCAL bottom
    LOCAL fits := .F.
    LOCAL node := lnodes[ wbs:epsd + 1 ][ n + 1 ]

    DO WHILE i <= 2 .AND. patches[ i ] != NIL .AND. ! fits
        left := node:x - PatchLeft( patches[ i ] )
        top := node:y - PatchTop( patches[ i ] )
        right := left + PatchW( patches[ i ] )
        bottom := top + PatchH( patches[ i ] )
        fits := left >= 0 .AND. right < SCREENWIDTH .AND. ;
                top >= 0 .AND. bottom < SCREENHEIGHT
        IF ! fits
            i++
        ENDIF
    ENDDO

    IF fits .AND. i <= 2
        V_DrawPatch( node:x, node:y, patches[ i ] )
    ELSE
        QOut( "Could not place patch on level " + LTrim( Str( n + 1 ) ) )
    ENDIF
RETURN

PROCEDURE WI_initAnimatedBack()
    LOCAL i
    LOCAL a
    MEMVAR gamemode

    IF gamemode == commercial .OR. wbs:epsd > 2
        RETURN
    ENDIF
    FOR i := 1 TO NUMANIMS[ wbs:epsd + 1 ]
        a := anims[ wbs:epsd + 1 ][ i ]
        a:ctr := -1
        DO CASE
        CASE a:type == ANIM_ALWAYS
            a:nexttic := bcnt + 1 + ( M_Random() % a:period )
        CASE a:type == ANIM_RANDOM
            a:nexttic := bcnt + 1 + a:data2 + ( M_Random() % a:data1 )
        CASE a:type == ANIM_LEVEL
            a:nexttic := bcnt + 1
        ENDCASE
    NEXT
RETURN

PROCEDURE WI_updateAnimatedBack()
    LOCAL i
    LOCAL a
    MEMVAR gamemode

    IF gamemode == commercial .OR. wbs:epsd > 2
        RETURN
    ENDIF
    FOR i := 1 TO NUMANIMS[ wbs:epsd + 1 ]
        a := anims[ wbs:epsd + 1 ][ i ]
        IF bcnt == a:nexttic
            DO CASE
            CASE a:type == ANIM_ALWAYS
                a:ctr++
                IF a:ctr >= a:nanims
                    a:ctr := 0
                ENDIF
                a:nexttic := bcnt + a:period
            CASE a:type == ANIM_RANDOM
                a:ctr++
                IF a:ctr == a:nanims
                    a:ctr := -1
                    a:nexttic := bcnt + a:data2 + ( M_Random() % a:data1 )
                ELSE
                    a:nexttic := bcnt + a:period
                ENDIF
            CASE a:type == ANIM_LEVEL
                IF ! ( state == StatCount .AND. i == 8 ) .AND. ;
                   wbs:next == a:data1
                    a:ctr++
                    IF a:ctr == a:nanims
                        a:ctr--
                    ENDIF
                    a:nexttic := bcnt + a:period
                ENDIF
            ENDCASE
        ENDIF
    NEXT
RETURN

PROCEDURE WI_drawAnimatedBack()
    LOCAL i
    LOCAL a
    MEMVAR gamemode

    IF gamemode == commercial .OR. wbs:epsd > 2
        RETURN
    ENDIF
    FOR i := 1 TO NUMANIMS[ wbs:epsd + 1 ]
        a := anims[ wbs:epsd + 1 ][ i ]
        IF a:ctr >= 0
            V_DrawPatch( a:loc:x, a:loc:y, a:p[ a:ctr + 1 ] )
        ENDIF
    NEXT
RETURN

FUNCTION WI_drawNum( x, y, n, digits )
    LOCAL fontwidth := PatchW( num[ 1 ] )
    LOCAL neg
    LOCAL temp

    IF digits < 0
        IF n == 0
            digits := 1
        ELSE
            digits := 0
            temp := n
            DO WHILE temp != 0
                temp := Int( temp / 10 )
                digits++
            ENDDO
        ENDIF
    ENDIF
    neg := n < 0
    IF neg
        n := -n
    ENDIF
    IF n == 1994
        RETURN 0
    ENDIF
    DO WHILE digits > 0
        digits--
        x -= fontwidth
        V_DrawPatch( x, y, num[ ( n % 10 ) + 1 ] )
        n := Int( n / 10 )
    ENDDO
    IF neg
        x -= 8
        V_DrawPatch( x, y, wiminus )
    ENDIF
RETURN x

PROCEDURE WI_drawPercent( x, y, value )
    IF value >= 0
        V_DrawPatch( x, y, percent )
        WI_drawNum( x, y, value, -1 )
    ENDIF
RETURN

PROCEDURE WI_drawTime( x, y, t )
    LOCAL div
    LOCAL n

    IF t < 0
        RETURN
    ENDIF
    IF t <= 61 * 59
        div := 1
        DO WHILE .T.
            n := ( Int( t / div ) % 60 )
            x := WI_drawNum( x, y, n, 2 ) - PatchW( colon )
            div *= 60
            IF div == 60 .OR. Int( t / div ) != 0
                V_DrawPatch( x, y, colon )
            ENDIF
            IF Int( t / div ) == 0
                EXIT
            ENDIF
        ENDDO
    ELSE
        V_DrawPatch( x - PatchW( sucks ), y, sucks )
    ENDIF
RETURN

PROCEDURE WI_End()
    WI_unloadData()
RETURN

PROCEDURE WI_initNoState()
    state := NoState
    accelerate := 0
    cnt := 10
RETURN

PROCEDURE WI_updateNoState()
    WI_updateAnimatedBack()
    cnt--
    IF cnt == 0
        G_WorldDone()
    ENDIF
RETURN

PROCEDURE WI_initShowNextLoc()
    state := ShowNextLoc
    accelerate := 0
    cnt := SHOWNEXTLOCDELAY * TICRATE
    WI_initAnimatedBack()
RETURN

PROCEDURE WI_updateShowNextLoc()
    WI_updateAnimatedBack()
    cnt--
    IF cnt == 0 .OR. accelerate != 0
        WI_initNoState()
    ELSE
        snl_pointeron := ( cnt & 31 ) < 20
    ENDIF
RETURN

PROCEDURE WI_drawShowNextLoc()
    LOCAL i
    LOCAL last
    MEMVAR gamemode

    WI_slamBackground()
    WI_drawAnimatedBack()
    IF gamemode != commercial
        IF wbs:epsd > 2
            WI_drawEL()
            RETURN
        ENDIF
        last := iif( wbs:last == 8, wbs:next - 1, wbs:last )
        FOR i := 0 TO last
            WI_drawOnLnode( i, patches[ "splat" ] )
        NEXT
        IF wbs:didsecret
            WI_drawOnLnode( 8, patches[ "splat" ] )
        ENDIF
        IF snl_pointeron
            WI_drawOnLnode( wbs:next, patches[ "yah" ] )
        ENDIF
    ENDIF
    IF gamemode != commercial .OR. wbs:next != 30
        WI_drawEL()
    ENDIF
RETURN

PROCEDURE WI_drawNoState()
    snl_pointeron := .T.
    WI_drawShowNextLoc()
RETURN

FUNCTION WI_fragSum( playernum )
    LOCAL i
    LOCAL fragsum := 0
    MEMVAR playeringame
    FOR i := 0 TO MAXPLAYERS - 1
        IF playeringame[ i + 1 ] .AND. i != playernum
            fragsum += plrs[ playernum + 1 ]:frags[ i + 1 ]
        ENDIF
    NEXT
    fragsum -= plrs[ playernum + 1 ]:frags[ playernum + 1 ]
RETURN fragsum

PROCEDURE WI_initDeathmatchStats()
    LOCAL i
    LOCAL j
    MEMVAR playeringame

    state := StatCount
    accelerate := 0
    dm_state := 1
    cnt_pause := TICRATE
    FOR i := 1 TO MAXPLAYERS
        IF playeringame[ i ]
            FOR j := 1 TO MAXPLAYERS
                IF playeringame[ j ]
                    dm_frags[ i ][ j ] := 0
                ENDIF
            NEXT
            dm_totals[ i ] := 0
        ENDIF
    NEXT
    WI_initAnimatedBack()
RETURN

PROCEDURE WI_updateDeathmatchStats()
    LOCAL i
    LOCAL j
    LOCAL stillticking
    MEMVAR gamemode
    MEMVAR playeringame

    WI_updateAnimatedBack()
    IF accelerate != 0 .AND. dm_state != 4
        accelerate := 0
        FOR i := 1 TO MAXPLAYERS
            IF playeringame[ i ]
                FOR j := 1 TO MAXPLAYERS
                    IF playeringame[ j ]
                        dm_frags[ i ][ j ] := plrs[ i ]:frags[ j ]
                    ENDIF
                NEXT
                dm_totals[ i ] := WI_fragSum( i - 1 )
            ENDIF
        NEXT
        S_StartSound( 0, sfx_barexp )
        dm_state := 4
    ENDIF

    DO CASE
    CASE dm_state == 2
        IF ( bcnt & 3 ) == 0
            S_StartSound( 0, sfx_pistol )
        ENDIF
        stillticking := .F.
        FOR i := 1 TO MAXPLAYERS
            IF playeringame[ i ]
                FOR j := 1 TO MAXPLAYERS
                    IF playeringame[ j ] .AND. dm_frags[ i ][ j ] != plrs[ i ]:frags[ j ]
                        dm_frags[ i ][ j ] += iif( plrs[ i ]:frags[ j ] < 0, -1, 1 )
                        dm_frags[ i ][ j ] := Max( -99, Min( 99, dm_frags[ i ][ j ] ) )
                        stillticking := .T.
                    ENDIF
                NEXT
                dm_totals[ i ] := Max( -99, Min( 99, WI_fragSum( i - 1 ) ) )
            ENDIF
        NEXT
        IF ! stillticking
            S_StartSound( 0, sfx_barexp )
            dm_state++
        ENDIF
    CASE dm_state == 4
        IF accelerate != 0
            S_StartSound( 0, sfx_slop )
            IF gamemode == commercial
                WI_initNoState()
            ELSE
                WI_initShowNextLoc()
            ENDIF
        ENDIF
    CASE ( dm_state & 1 ) != 0
        cnt_pause--
        IF cnt_pause == 0
            dm_state++
            cnt_pause := TICRATE
        ENDIF
    ENDCASE
RETURN

PROCEDURE WI_drawDeathmatchStats()
    LOCAL i
    LOCAL j
    LOCAL x := DM_MATRIXX + DM_SPACINGX
    LOCAL y := DM_MATRIXY
    LOCAL w
    MEMVAR playeringame

    WI_slamBackground()
    WI_drawAnimatedBack()
    WI_drawLF()
    V_DrawPatch( DM_TOTALSX - Int( PatchW( patches[ "total" ] ) / 2 ), ;
                 DM_MATRIXY - WI_SPACINGY + 10, patches[ "total" ] )
    V_DrawPatch( DM_KILLERSX, DM_KILLERSY, patches[ "killers" ] )
    V_DrawPatch( DM_VICTIMSX, DM_VICTIMSY, patches[ "victims" ] )
    FOR i := 1 TO MAXPLAYERS
        IF playeringame[ i ]
            V_DrawPatch( x - Int( PatchW( patches[ "p" ][ i ] ) / 2 ), ;
                         DM_MATRIXY - WI_SPACINGY, patches[ "p" ][ i ] )
            V_DrawPatch( DM_MATRIXX - Int( PatchW( patches[ "p" ][ i ] ) / 2 ), ;
                         y, patches[ "p" ][ i ] )
            IF i - 1 == me
                V_DrawPatch( x - Int( PatchW( patches[ "p" ][ i ] ) / 2 ), ;
                             DM_MATRIXY - WI_SPACINGY, patches[ "bstar" ] )
                V_DrawPatch( DM_MATRIXX - Int( PatchW( patches[ "p" ][ i ] ) / 2 ), ;
                             y, patches[ "star" ] )
            ENDIF
        ENDIF
        x += DM_SPACINGX
        y += WI_SPACINGY
    NEXT
    y := DM_MATRIXY + 10
    w := PatchW( num[ 1 ] )
    FOR i := 1 TO MAXPLAYERS
        x := DM_MATRIXX + DM_SPACINGX
        IF playeringame[ i ]
            FOR j := 1 TO MAXPLAYERS
                IF playeringame[ j ]
                    WI_drawNum( x + w, y, dm_frags[ i ][ j ], 2 )
                ENDIF
                x += DM_SPACINGX
            NEXT
            WI_drawNum( DM_TOTALSX + w, y, dm_totals[ i ], 2 )
        ENDIF
        y += WI_SPACINGY
    NEXT
RETURN

PROCEDURE WI_initNetgameStats()
    LOCAL i
    MEMVAR playeringame

    state := StatCount
    accelerate := 0
    ng_state := 1
    cnt_pause := TICRATE
    dofrags := 0
    FOR i := 1 TO MAXPLAYERS
        IF playeringame[ i ]
            cnt_kills[ i ] := 0
            cnt_items[ i ] := 0
            cnt_secret[ i ] := 0
            cnt_frags[ i ] := 0
            dofrags += WI_fragSum( i - 1 )
        ENDIF
    NEXT
    dofrags := iif( dofrags != 0, 1, 0 )
    WI_initAnimatedBack()
RETURN

PROCEDURE WI_updateNetgameStats()
    LOCAL i
    LOCAL fsum
    LOCAL stillticking
    MEMVAR gamemode
    MEMVAR playeringame

    WI_updateAnimatedBack()
    IF accelerate != 0 .AND. ng_state != 10
        accelerate := 0
        FOR i := 1 TO MAXPLAYERS
            IF playeringame[ i ]
                cnt_kills[ i ] := PercentValue( plrs[ i ]:skills, wbs:maxkills )
                cnt_items[ i ] := PercentValue( plrs[ i ]:sitems, wbs:maxitems )
                cnt_secret[ i ] := PercentValue( plrs[ i ]:ssecret, wbs:maxsecret )
                IF dofrags != 0
                    cnt_frags[ i ] := WI_fragSum( i - 1 )
                ENDIF
            ENDIF
        NEXT
        S_StartSound( 0, sfx_barexp )
        ng_state := 10
    ENDIF

    DO CASE
    CASE ng_state == 2
        IF ( bcnt & 3 ) == 0
            S_StartSound( 0, sfx_pistol )
        ENDIF
        stillticking := .F.
        FOR i := 1 TO MAXPLAYERS
            IF playeringame[ i ]
                cnt_kills[ i ] += 2
                IF cnt_kills[ i ] >= PercentValue( plrs[ i ]:skills, wbs:maxkills )
                    cnt_kills[ i ] := PercentValue( plrs[ i ]:skills, wbs:maxkills )
                ELSE
                    stillticking := .T.
                ENDIF
            ENDIF
        NEXT
        IF ! stillticking
            S_StartSound( 0, sfx_barexp )
            ng_state++
        ENDIF
    CASE ng_state == 4
        IF ( bcnt & 3 ) == 0
            S_StartSound( 0, sfx_pistol )
        ENDIF
        stillticking := .F.
        FOR i := 1 TO MAXPLAYERS
            IF playeringame[ i ]
                cnt_items[ i ] += 2
                IF cnt_items[ i ] >= PercentValue( plrs[ i ]:sitems, wbs:maxitems )
                    cnt_items[ i ] := PercentValue( plrs[ i ]:sitems, wbs:maxitems )
                ELSE
                    stillticking := .T.
                ENDIF
            ENDIF
        NEXT
        IF ! stillticking
            S_StartSound( 0, sfx_barexp )
            ng_state++
        ENDIF
    CASE ng_state == 6
        IF ( bcnt & 3 ) == 0
            S_StartSound( 0, sfx_pistol )
        ENDIF
        stillticking := .F.
        FOR i := 1 TO MAXPLAYERS
            IF playeringame[ i ]
                cnt_secret[ i ] += 2
                IF cnt_secret[ i ] >= PercentValue( plrs[ i ]:ssecret, wbs:maxsecret )
                    cnt_secret[ i ] := PercentValue( plrs[ i ]:ssecret, wbs:maxsecret )
                ELSE
                    stillticking := .T.
                ENDIF
            ENDIF
        NEXT
        IF ! stillticking
            S_StartSound( 0, sfx_barexp )
            ng_state += 1 + 2 * iif( dofrags == 0, 1, 0 )
        ENDIF
    CASE ng_state == 8
        IF ( bcnt & 3 ) == 0
            S_StartSound( 0, sfx_pistol )
        ENDIF
        stillticking := .F.
        FOR i := 1 TO MAXPLAYERS
            IF playeringame[ i ]
                cnt_frags[ i ]++
                fsum := WI_fragSum( i - 1 )
                IF cnt_frags[ i ] >= fsum
                    cnt_frags[ i ] := fsum
                ELSE
                    stillticking := .T.
                ENDIF
            ENDIF
        NEXT
        IF ! stillticking
            S_StartSound( 0, sfx_pldeth )
            ng_state++
        ENDIF
    CASE ng_state == 10
        IF accelerate != 0
            S_StartSound( 0, sfx_wpnup )
            IF gamemode == commercial
                WI_initNoState()
            ELSE
                WI_initShowNextLoc()
            ENDIF
        ENDIF
    CASE ( ng_state & 1 ) != 0
        cnt_pause--
        IF cnt_pause == 0
            ng_state++
            cnt_pause := TICRATE
        ENDIF
    ENDCASE
RETURN

PROCEDURE WI_drawNetgameStats()
    LOCAL i
    LOCAL x
    LOCAL y
    LOCAL pwidth := PatchW( percent )
    LOCAL statsx := 32 + Int( PatchW( patches[ "star" ] ) / 2 ) + ;
                    32 * iif( dofrags == 0, 1, 0 )
    MEMVAR playeringame

    WI_slamBackground()
    WI_drawAnimatedBack()
    WI_drawLF()
    V_DrawPatch( statsx + NG_SPACINGX - PatchW( patches[ "kills" ] ), ;
                 NG_STATSY, patches[ "kills" ] )
    V_DrawPatch( statsx + 2 * NG_SPACINGX - PatchW( patches[ "items" ] ), ;
                 NG_STATSY, patches[ "items" ] )
    V_DrawPatch( statsx + 3 * NG_SPACINGX - PatchW( patches[ "secret" ] ), ;
                 NG_STATSY, patches[ "secret" ] )
    IF dofrags != 0
        V_DrawPatch( statsx + 4 * NG_SPACINGX - PatchW( patches[ "frags" ] ), ;
                     NG_STATSY, patches[ "frags" ] )
    ENDIF
    y := NG_STATSY + PatchH( patches[ "kills" ] )
    FOR i := 1 TO MAXPLAYERS
        IF playeringame[ i ]
            x := statsx
            V_DrawPatch( x - PatchW( patches[ "p" ][ i ] ), y, patches[ "p" ][ i ] )
            IF i - 1 == me
                V_DrawPatch( x - PatchW( patches[ "p" ][ i ] ), y, patches[ "star" ] )
            ENDIF
            x += NG_SPACINGX
            WI_drawPercent( x - pwidth, y + 10, cnt_kills[ i ] )
            x += NG_SPACINGX
            WI_drawPercent( x - pwidth, y + 10, cnt_items[ i ] )
            x += NG_SPACINGX
            WI_drawPercent( x - pwidth, y + 10, cnt_secret[ i ] )
            x += NG_SPACINGX
            IF dofrags != 0
                WI_drawNum( x, y + 10, cnt_frags[ i ], -1 )
            ENDIF
        ENDIF
        y += WI_SPACINGY
    NEXT
RETURN

PROCEDURE WI_initStats()
    state := StatCount
    accelerate := 0
    sp_state := 1
    cnt_kills[ 1 ] := -1
    cnt_items[ 1 ] := -1
    cnt_secret[ 1 ] := -1
    cnt_time := -1
    cnt_par := -1
    cnt_pause := TICRATE
    WI_initAnimatedBack()
RETURN

PROCEDURE WI_updateStats()
    LOCAL player := plrs[ me + 1 ]
    MEMVAR gamemode

    WI_updateAnimatedBack()
    IF accelerate != 0 .AND. sp_state != 10
        accelerate := 0
        cnt_kills[ 1 ] := PercentValue( player:skills, wbs:maxkills )
        cnt_items[ 1 ] := PercentValue( player:sitems, wbs:maxitems )
        cnt_secret[ 1 ] := PercentValue( player:ssecret, wbs:maxsecret )
        cnt_time := Int( player:stime / TICRATE )
        cnt_par := Int( wbs:partime / TICRATE )
        S_StartSound( 0, sfx_barexp )
        sp_state := 10
    ENDIF

    DO CASE
    CASE sp_state == 2
        cnt_kills[ 1 ] += 2
        IF ( bcnt & 3 ) == 0
            S_StartSound( 0, sfx_pistol )
        ENDIF
        IF cnt_kills[ 1 ] >= PercentValue( player:skills, wbs:maxkills )
            cnt_kills[ 1 ] := PercentValue( player:skills, wbs:maxkills )
            S_StartSound( 0, sfx_barexp )
            sp_state++
        ENDIF
    CASE sp_state == 4
        cnt_items[ 1 ] += 2
        IF ( bcnt & 3 ) == 0
            S_StartSound( 0, sfx_pistol )
        ENDIF
        IF cnt_items[ 1 ] >= PercentValue( player:sitems, wbs:maxitems )
            cnt_items[ 1 ] := PercentValue( player:sitems, wbs:maxitems )
            S_StartSound( 0, sfx_barexp )
            sp_state++
        ENDIF
    CASE sp_state == 6
        cnt_secret[ 1 ] += 2
        IF ( bcnt & 3 ) == 0
            S_StartSound( 0, sfx_pistol )
        ENDIF
        IF cnt_secret[ 1 ] >= PercentValue( player:ssecret, wbs:maxsecret )
            cnt_secret[ 1 ] := PercentValue( player:ssecret, wbs:maxsecret )
            S_StartSound( 0, sfx_barexp )
            sp_state++
        ENDIF
    CASE sp_state == 8
        IF ( bcnt & 3 ) == 0
            S_StartSound( 0, sfx_pistol )
        ENDIF
        cnt_time += 3
        IF cnt_time >= Int( player:stime / TICRATE )
            cnt_time := Int( player:stime / TICRATE )
        ENDIF
        cnt_par += 3
        IF cnt_par >= Int( wbs:partime / TICRATE )
            cnt_par := Int( wbs:partime / TICRATE )
            IF cnt_time >= Int( player:stime / TICRATE )
                S_StartSound( 0, sfx_barexp )
                sp_state++
            ENDIF
        ENDIF
    CASE sp_state == 10
        IF accelerate != 0
            S_StartSound( 0, sfx_wpnup )
            IF gamemode == commercial
                WI_initNoState()
            ELSE
                WI_initShowNextLoc()
            ENDIF
        ENDIF
    CASE ( sp_state & 1 ) != 0
        cnt_pause--
        IF cnt_pause == 0
            sp_state++
            cnt_pause := TICRATE
        ENDIF
    ENDCASE
RETURN

PROCEDURE WI_drawStats()
    LOCAL lh := Int( 3 * PatchH( num[ 1 ] ) / 2 )

    WI_slamBackground()
    WI_drawAnimatedBack()
    WI_drawLF()
    V_DrawPatch( SP_STATSX, SP_STATSY, patches[ "kills" ] )
    WI_drawPercent( SCREENWIDTH - SP_STATSX, SP_STATSY, cnt_kills[ 1 ] )
    V_DrawPatch( SP_STATSX, SP_STATSY + lh, patches[ "items" ] )
    WI_drawPercent( SCREENWIDTH - SP_STATSX, SP_STATSY + lh, cnt_items[ 1 ] )
    V_DrawPatch( SP_STATSX, SP_STATSY + 2 * lh, patches[ "sp_secret" ] )
    WI_drawPercent( SCREENWIDTH - SP_STATSX, SP_STATSY + 2 * lh, cnt_secret[ 1 ] )
    V_DrawPatch( SP_TIMEX, SP_TIMEY, patches[ "time" ] )
    WI_drawTime( Int( SCREENWIDTH / 2 ) - SP_TIMEX, SP_TIMEY, cnt_time )
    IF wbs:epsd < 3
        V_DrawPatch( Int( SCREENWIDTH / 2 ) + SP_TIMEX, SP_TIMEY, patches[ "par" ] )
        WI_drawTime( SCREENWIDTH - SP_TIMEX, SP_TIMEY, cnt_par )
    ENDIF
RETURN

PROCEDURE WI_checkForAccelerate()
    LOCAL i
    LOCAL player
    MEMVAR playeringame
    MEMVAR players

    FOR i := 1 TO MAXPLAYERS
        IF playeringame[ i ]
            player := players[ i ]
            IF ( player:cmd:buttons & BT_ATTACK ) != 0
                IF ! player:attackdown
                    accelerate := 1
                ENDIF
                player:attackdown := .T.
            ELSE
                player:attackdown := .F.
            ENDIF
            IF ( player:cmd:buttons & BT_USE ) != 0
                IF ! player:usedown
                    accelerate := 1
                ENDIF
                player:usedown := .T.
            ELSE
                player:usedown := .F.
            ENDIF
        ENDIF
    NEXT
RETURN

PROCEDURE WI_Ticker()
    MEMVAR deathmatch
    MEMVAR gamemode
    MEMVAR netgame
    bcnt++
    IF bcnt == 1
        IF gamemode == commercial
            S_ChangeMusic( mus_dm2int, .T. )
        ELSE
            S_ChangeMusic( mus_inter, .T. )
        ENDIF
    ENDIF
    WI_checkForAccelerate()
    DO CASE
    CASE state == StatCount
        IF deathmatch != 0
            WI_updateDeathmatchStats()
        ELSEIF netgame
            WI_updateNetgameStats()
        ELSE
            WI_updateStats()
        ENDIF
    CASE state == ShowNextLoc
        WI_updateShowNextLoc()
    CASE state == NoState
        WI_updateNoState()
    ENDCASE
RETURN

PROCEDURE WI_loadData()
    LOCAL i
    LOCAL j
    LOCAL name
    LOCAL a
    MEMVAR deathmatch
    MEMVAR gamemode
    MEMVAR netgame

    NUMCMAPS := iif( gamemode == commercial, 32, 0 )
    lnames := Array( iif( gamemode == commercial, NUMCMAPS, NUMMAPS ) )
    IF gamemode == commercial
        FOR i := 0 TO NUMCMAPS - 1
            name := "CWILV" + StrZero( i, 2 )
            lnames[ i + 1 ] := W_CacheLumpName( name, PU_STATIC )
        NEXT
    ELSE
        FOR i := 0 TO NUMMAPS - 1
            name := "WILV" + LTrim( Str( wbs:epsd ) ) + LTrim( Str( i ) )
            lnames[ i + 1 ] := W_CacheLumpName( name, PU_STATIC )
        NEXT
        patches[ "yah" ][ 1 ] := W_CacheLumpName( DEH_String( "WIURH0" ), PU_STATIC )
        patches[ "yah" ][ 2 ] := W_CacheLumpName( DEH_String( "WIURH1" ), PU_STATIC )
        patches[ "splat" ][ 1 ] := W_CacheLumpName( DEH_String( "WISPLAT" ), PU_STATIC )
        IF wbs:epsd < 3
            FOR j := 0 TO NUMANIMS[ wbs:epsd + 1 ] - 1
                a := anims[ wbs:epsd + 1 ][ j + 1 ]
                FOR i := 0 TO a:nanims - 1
                    IF wbs:epsd != 1 .OR. j != 8
                        name := "WIA" + LTrim( Str( wbs:epsd ) ) + ;
                                StrZero( j, 2 ) + StrZero( i, 2 )
                        a:p[ i + 1 ] := W_CacheLumpName( name, PU_STATIC )
                    ELSE
                        a:p[ i + 1 ] := anims[ 2 ][ 5 ]:p[ i + 1 ]
                    ENDIF
                NEXT
            NEXT
        ENDIF
    ENDIF

    wiminus := W_CacheLumpName( DEH_String( "WIMINUS" ), PU_STATIC )
    FOR i := 0 TO 9
        num[ i + 1 ] := W_CacheLumpName( "WINUM" + LTrim( Str( i ) ), PU_STATIC )
    NEXT
    percent := W_CacheLumpName( DEH_String( "WIPCNT" ), PU_STATIC )
    finished := W_CacheLumpName( DEH_String( "WIF" ), PU_STATIC )
    entering := W_CacheLumpName( DEH_String( "WIENTER" ), PU_STATIC )
    patches[ "kills" ] := W_CacheLumpName( DEH_String( "WIOSTK" ), PU_STATIC )
    patches[ "secret" ] := W_CacheLumpName( DEH_String( "WIOSTS" ), PU_STATIC )
    patches[ "sp_secret" ] := W_CacheLumpName( DEH_String( "WISCRT2" ), PU_STATIC )
    IF W_CheckNumForName( DEH_String( "WIOBJ" ) ) >= 0 .AND. netgame .AND. deathmatch == 0
        patches[ "items" ] := W_CacheLumpName( DEH_String( "WIOBJ" ), PU_STATIC )
    ELSE
        patches[ "items" ] := W_CacheLumpName( DEH_String( "WIOSTI" ), PU_STATIC )
    ENDIF
    patches[ "frags" ] := W_CacheLumpName( DEH_String( "WIFRGS" ), PU_STATIC )
    colon := W_CacheLumpName( DEH_String( "WICOLON" ), PU_STATIC )
    patches[ "time" ] := W_CacheLumpName( DEH_String( "WITIME" ), PU_STATIC )
    sucks := W_CacheLumpName( DEH_String( "WISUCKS" ), PU_STATIC )
    patches[ "par" ] := W_CacheLumpName( DEH_String( "WIPAR" ), PU_STATIC )
    patches[ "killers" ] := W_CacheLumpName( DEH_String( "WIKILRS" ), PU_STATIC )
    patches[ "victims" ] := W_CacheLumpName( DEH_String( "WIVCTMS" ), PU_STATIC )
    patches[ "total" ] := W_CacheLumpName( DEH_String( "WIMSTT" ), PU_STATIC )
    FOR i := 0 TO MAXPLAYERS - 1
        patches[ "p" ][ i + 1 ] := W_CacheLumpName( "STPB" + LTrim( Str( i ) ), PU_STATIC )
        patches[ "bp" ][ i + 1 ] := W_CacheLumpName( "WIBP" + LTrim( Str( i + 1 ) ), PU_STATIC )
    NEXT
    IF gamemode == commercial .OR. ( gamemode == retail .AND. wbs:epsd == 3 )
        name := DEH_String( "INTERPIC" )
    ELSE
        name := "WIMAP" + LTrim( Str( wbs:epsd ) )
    ENDIF
    background := W_CacheLumpName( name, PU_STATIC )
    patches[ "star" ] := W_CacheLumpName( DEH_String( "STFST01" ), PU_STATIC )
    patches[ "bstar" ] := W_CacheLumpName( DEH_String( "STFDEAD0" ), PU_STATIC )
RETURN

PROCEDURE WI_unloadData()
    LOCAL i
    LOCAL j
    LOCAL name
    LOCAL a
    MEMVAR deathmatch
    MEMVAR gamemode
    MEMVAR netgame

    IF gamemode == commercial
        FOR i := 0 TO NUMCMAPS - 1
            name := "CWILV" + StrZero( i, 2 )
            W_ReleaseLumpName( name )
            lnames[ i + 1 ] := NIL
        NEXT
    ELSE
        FOR i := 0 TO NUMMAPS - 1
            name := "WILV" + LTrim( Str( wbs:epsd ) ) + LTrim( Str( i ) )
            W_ReleaseLumpName( name )
            lnames[ i + 1 ] := NIL
        NEXT
        W_ReleaseLumpName( DEH_String( "WIURH0" ) )
        W_ReleaseLumpName( DEH_String( "WIURH1" ) )
        W_ReleaseLumpName( DEH_String( "WISPLAT" ) )
        AFill( patches[ "yah" ], NIL )
        AFill( patches[ "splat" ], NIL )
        IF wbs:epsd < 3
            FOR j := 0 TO NUMANIMS[ wbs:epsd + 1 ] - 1
                a := anims[ wbs:epsd + 1 ][ j + 1 ]
                FOR i := 0 TO a:nanims - 1
                    IF wbs:epsd != 1 .OR. j != 8
                        name := "WIA" + LTrim( Str( wbs:epsd ) ) + ;
                                StrZero( j, 2 ) + StrZero( i, 2 )
                        W_ReleaseLumpName( name )
                    ENDIF
                    a:p[ i + 1 ] := NIL
                NEXT
            NEXT
        ENDIF
    ENDIF

    W_ReleaseLumpName( DEH_String( "WIMINUS" ) )
    FOR i := 0 TO 9
        W_ReleaseLumpName( "WINUM" + LTrim( Str( i ) ) )
        num[ i + 1 ] := NIL
    NEXT
    W_ReleaseLumpName( DEH_String( "WIPCNT" ) )
    W_ReleaseLumpName( DEH_String( "WIF" ) )
    W_ReleaseLumpName( DEH_String( "WIENTER" ) )
    W_ReleaseLumpName( DEH_String( "WIOSTK" ) )
    W_ReleaseLumpName( DEH_String( "WIOSTS" ) )
    W_ReleaseLumpName( DEH_String( "WISCRT2" ) )
    IF W_CheckNumForName( DEH_String( "WIOBJ" ) ) >= 0 .AND. netgame .AND. deathmatch == 0
        W_ReleaseLumpName( DEH_String( "WIOBJ" ) )
    ELSE
        W_ReleaseLumpName( DEH_String( "WIOSTI" ) )
    ENDIF
    W_ReleaseLumpName( DEH_String( "WIFRGS" ) )
    W_ReleaseLumpName( DEH_String( "WICOLON" ) )
    W_ReleaseLumpName( DEH_String( "WITIME" ) )
    W_ReleaseLumpName( DEH_String( "WISUCKS" ) )
    W_ReleaseLumpName( DEH_String( "WIPAR" ) )
    W_ReleaseLumpName( DEH_String( "WIKILRS" ) )
    W_ReleaseLumpName( DEH_String( "WIVCTMS" ) )
    W_ReleaseLumpName( DEH_String( "WIMSTT" ) )
    FOR i := 0 TO MAXPLAYERS - 1
        W_ReleaseLumpName( "STPB" + LTrim( Str( i ) ) )
        W_ReleaseLumpName( "WIBP" + LTrim( Str( i + 1 ) ) )
        patches[ "p" ][ i + 1 ] := NIL
        patches[ "bp" ][ i + 1 ] := NIL
    NEXT
    IF gamemode == commercial .OR. ( gamemode == retail .AND. wbs:epsd == 3 )
        name := DEH_String( "INTERPIC" )
    ELSE
        name := "WIMAP" + LTrim( Str( wbs:epsd ) )
    ENDIF
    W_ReleaseLumpName( name )
    wiminus := NIL
    percent := NIL
    finished := NIL
    entering := NIL
    colon := NIL
    sucks := NIL
    background := NIL
    patches[ "kills" ] := NIL
    patches[ "secret" ] := NIL
    patches[ "sp_secret" ] := NIL
    patches[ "items" ] := NIL
    patches[ "frags" ] := NIL
    patches[ "time" ] := NIL
    patches[ "par" ] := NIL
    patches[ "killers" ] := NIL
    patches[ "victims" ] := NIL
    patches[ "total" ] := NIL
RETURN

PROCEDURE WI_Drawer()
    MEMVAR deathmatch
    MEMVAR netgame
    DO CASE
    CASE state == StatCount
        IF deathmatch != 0
            WI_drawDeathmatchStats()
        ELSEIF netgame
            WI_drawNetgameStats()
        ELSE
            WI_drawStats()
        ENDIF
    CASE state == ShowNextLoc
        WI_drawShowNextLoc()
    CASE state == NoState
        WI_drawNoState()
    ENDCASE
RETURN

PROCEDURE WI_initVariables( wbstartstruct )
    MEMVAR gamemode
    wbs := wbstartstruct
    accelerate := 0
    cnt := 0
    bcnt := 0
    me := wbs:pnum
    plrs := wbs:plyr
    IF wbs:maxkills == 0
        wbs:maxkills := 1
    ENDIF
    IF wbs:maxitems == 0
        wbs:maxitems := 1
    ENDIF
    IF wbs:maxsecret == 0
        wbs:maxsecret := 1
    ENDIF
    IF gamemode != retail .AND. wbs:epsd > 2
        wbs:epsd -= 3
    ENDIF
RETURN

PROCEDURE WI_Start( wbstartstruct )
    MEMVAR deathmatch
    MEMVAR netgame
    WI_initVariables( wbstartstruct )
    WI_loadData()
    IF deathmatch != 0
        WI_initDeathmatchStats()
    ELSEIF netgame
        WI_initNetgameStats()
    ELSE
        WI_initStats()
    ENDIF
RETURN

INIT PROCEDURE init_wi_stuff
    LOCAL epsd0
    LOCAL epsd1
    LOCAL epsd2
    LOCAL i

    PUBLIC wbs, state, cnt, bcnt, me, plrs, accelerate, patches
    PUBLIC cnt_kills, cnt_items, cnt_secret, cnt_frags
    PUBLIC cnt_time, cnt_par, cnt_pause
    PUBLIC dofrags, snl_pointeron, sp_state, ng_state, dm_state
    PUBLIC dm_frags, dm_totals
    PUBLIC NUMCMAPS, lnames, num, wiminus, percent, colon, finished, entering
    PUBLIC sucks, background

    wbs := NIL
    state := NoState
    cnt := 0
    bcnt := 0
    me := 0
    plrs := {}
    accelerate := 0
    patches := { ;
        "yah" => Array( 3 ), "splat" => Array( 2 ), ;
        "kills" => NIL, "secret" => NIL, "sp_secret" => NIL, ;
        "items" => NIL, "frags" => NIL, "time" => NIL, "par" => NIL, ;
        "killers" => NIL, "victims" => NIL, "total" => NIL, ;
        "star" => NIL, "bstar" => NIL, ;
        "p" => Array( MAXPLAYERS ), "bp" => Array( MAXPLAYERS ) }

    lnodes := { ;
        { Point( 185, 164 ), Point( 148, 143 ), Point( 69, 122 ), ;
          Point( 209, 102 ), Point( 116, 89 ), Point( 166, 55 ), ;
          Point( 71, 56 ), Point( 135, 29 ), Point( 71, 24 ) }, ;
        { Point( 254, 25 ), Point( 97, 50 ), Point( 188, 64 ), ;
          Point( 128, 78 ), Point( 214, 92 ), Point( 133, 130 ), ;
          Point( 208, 136 ), Point( 148, 140 ), Point( 235, 158 ) }, ;
        { Point( 156, 168 ), Point( 48, 154 ), Point( 174, 95 ), ;
          Point( 265, 75 ), Point( 130, 48 ), Point( 279, 23 ), ;
          Point( 198, 48 ), Point( 140, 25 ), Point( 281, 136 ) }, ;
        Array( NUMMAPS ) }

    epsd0 := { ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 224, 104, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 184, 160, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 112, 136, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 72, 112, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 88, 96, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 64, 48, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 192, 40, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 136, 16, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 80, 16, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 64, 24, 0 ) }
    epsd1 := { ;
        Anim( ANIM_LEVEL, Int( TICRATE / 3 ), 1, 128, 136, 1 ), ;
        Anim( ANIM_LEVEL, Int( TICRATE / 3 ), 1, 128, 136, 2 ), ;
        Anim( ANIM_LEVEL, Int( TICRATE / 3 ), 1, 128, 136, 3 ), ;
        Anim( ANIM_LEVEL, Int( TICRATE / 3 ), 1, 128, 136, 4 ), ;
        Anim( ANIM_LEVEL, Int( TICRATE / 3 ), 1, 128, 136, 5 ), ;
        Anim( ANIM_LEVEL, Int( TICRATE / 3 ), 1, 128, 136, 6 ), ;
        Anim( ANIM_LEVEL, Int( TICRATE / 3 ), 1, 128, 136, 7 ), ;
        Anim( ANIM_LEVEL, Int( TICRATE / 3 ), 3, 192, 144, 8 ), ;
        Anim( ANIM_LEVEL, Int( TICRATE / 3 ), 1, 128, 136, 8 ) }
    epsd2 := { ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 104, 168, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 40, 136, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 160, 96, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 104, 80, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 3 ), 3, 120, 32, 0 ), ;
        Anim( ANIM_ALWAYS, Int( TICRATE / 4 ), 3, 40, 0, 0 ) }
    anims := { epsd0, epsd1, epsd2, {} }
    NUMANIMS := { Len( epsd0 ), Len( epsd1 ), Len( epsd2 ), 0 }

    cnt_kills := Array( MAXPLAYERS )
    cnt_items := Array( MAXPLAYERS )
    cnt_secret := Array( MAXPLAYERS )
    cnt_frags := Array( MAXPLAYERS )
    AFill( cnt_kills, 0 )
    AFill( cnt_items, 0 )
    AFill( cnt_secret, 0 )
    AFill( cnt_frags, 0 )
    cnt_time := 0
    cnt_par := 0
    cnt_pause := 0
    dofrags := 0
    snl_pointeron := .F.
    sp_state := 0
    ng_state := 0
    dm_state := 0
    dm_frags := Array( MAXPLAYERS )
    FOR i := 1 TO MAXPLAYERS
        dm_frags[ i ] := Array( MAXPLAYERS )
        AFill( dm_frags[ i ], 0 )
    NEXT
    dm_totals := Array( MAXPLAYERS )
    AFill( dm_totals, 0 )
    NUMCMAPS := 0
    lnames := {}
    num := Array( 10 )
    wiminus := NIL
    percent := NIL
    colon := NIL
    finished := NIL
    entering := NIL
    sucks := NIL
    background := NIL
RETURN
