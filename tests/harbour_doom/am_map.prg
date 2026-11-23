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

STATIC cheating
STATIC grid
STATIC leveljuststarted
STATIC finit_width
STATIC finit_height
STATIC f_x
STATIC f_y
STATIC f_w
STATIC f_h
STATIC lightlev
STATIC amclock
STATIC m_paninc
STATIC mtof_zoommul
STATIC ftom_zoommul
STATIC m_x
STATIC m_y
STATIC m_x2
STATIC m_y2
STATIC m_w
STATIC m_h
STATIC min_x
STATIC min_y
STATIC max_x
STATIC max_y
STATIC max_w
STATIC max_h
STATIC min_w
STATIC min_h
STATIC min_scale_mtof
STATIC max_scale_mtof
STATIC old_m_w
STATIC old_m_h
STATIC old_m_x
STATIC old_m_y
STATIC f_oldloc
STATIC scale_mtof
STATIC scale_ftom
STATIC plr
STATIC marknums
STATIC markpoints
STATIC markpointnum
STATIC followplayer
STATIC stopped
STATIC player_arrow
STATIC cheat_player_arrow
STATIC triangle_guy
STATIC thintriangle_guy
STATIC lastlevel
STATIC lastepisode
STATIC bigstate
STATIC nexttic
STATIC litelevels
STATIC litelevelscnt
STATIC fuck
STATIC clip_fl
STATIC cheat_amap

#include "am_map.ch"

CLASS fpoint_t
    DATA x
    DATA y
    METHOD New()
ENDCLASS
CLASS fline_t
    DATA a
    DATA b
    METHOD New()
ENDCLASS
CLASS mpoint_t
    DATA x
    DATA y
    METHOD New()
ENDCLASS
CLASS mline_t
    DATA a
    DATA b
    METHOD New()
ENDCLASS
CLASS islope_t
    DATA slp
    DATA islp
    METHOD New()
ENDCLASS
#include "doomdef.ch"
#include "doomstat.ch"
#include "doomdata.ch"
#include "d_event.ch"
#include "dstrings.ch"
#include "deh_main.ch"
#include "m_cheat.ch"
#include "m_controls.ch"
#include "m_fixed.ch"
#include "m_bbox.ch"
#include "p_local.ch"
#include "p_mobj.ch"
#include "st_stuff.ch"
#include "tables.ch"
#include "v_video.ch"
#include "z_zone.ch"
#include "i_video.ch"

#define REDS       ( 256 - 5 * 16 )
#define REDRANGE   16
#define BLUES      ( 256 - 4 * 16 + 8 )
#define BLUERANGE  8
#define GREENS     ( 7 * 16 )
#define GREENRANGE 16
#define GRAYS      ( 6 * 16 )
#define GRAYSRANGE 16
#define BROWNS     ( 4 * 16 )
#define BROWNRANGE 16
#define YELLOWS    ( 256 - 32 + 7 )
#define YELLOWRANGE 1
#define BLACK      0
#define WHITE      ( 256 - 47 )

#define BACKGROUND       BLACK
#define YOURCOLORS       WHITE
#define YOURRANGE        0
#define WALLCOLORS       REDS
#define WALLRANGE        REDRANGE
#define TSWALLCOLORS     GRAYS
#define TSWALLRANGE      GRAYSRANGE
#define FDWALLCOLORS     BROWNS
#define FDWALLRANGE      BROWNRANGE
#define CDWALLCOLORS     YELLOWS
#define CDWALLRANGE      YELLOWRANGE
#define THINGCOLORS      GREENS
#define THINGRANGE       GREENRANGE
#define SECRETWALLCOLORS WALLCOLORS
#define SECRETWALLRANGE  WALLRANGE
#define GRIDCOLORS       104
#define GRIDRANGE        0
#define XHAIRCOLORS      GRAYS

#define INITSCALEMTOF 13107
#define F_PANINC      4
#define M_ZOOMIN      66846
#define M_ZOOMOUT     64250

#define LINE_NEVERSEE ML_DONTDRAW

#define OC_LEFT   1
#define OC_RIGHT  2
#define OC_BOTTOM 4
#define OC_TOP    8



METHOD New() CLASS fpoint_t
    ::x := 0
    ::y := 0
RETURN Self

METHOD New() CLASS fline_t
    ::a := fpoint_t():New()
    ::b := fpoint_t():New()
RETURN Self

METHOD New() CLASS mpoint_t
    ::x := 0
    ::y := 0
RETURN Self

METHOD New() CLASS mline_t
    ::a := mpoint_t():New()
    ::b := mpoint_t():New()
RETURN Self

METHOD New() CLASS islope_t
    ::slp := 0
    ::islp := 0
RETURN Self

STATIC FUNCTION NewMLine( nAx, nAy, nBx, nBy )
    LOCAL ml := mline_t():New()
    ml:a:x := nAx
    ml:a:y := nAy
    ml:b:x := nBx
    ml:b:y := nBy
RETURN ml

STATIC FUNCTION FTOM( x )
RETURN FixedMul( x * FRACUNIT, scale_ftom )

STATIC FUNCTION MTOF( x )
RETURN Shar( FixedMul( x, scale_mtof ), 16 )

STATIC FUNCTION CXMTOF( x )
RETURN f_x + MTOF( x - m_x )

STATIC FUNCTION CYMTOF( y )
RETURN f_y + ( f_h - MTOF( y - m_y ) )

STATIC PROCEDURE PutDot( xx, yy, cc )
    MEMVAR I_VideoBuffer
    VidBSet( @I_VideoBuffer, yy * f_w + xx, cc )
RETURN

INIT PROCEDURE init_am_map
    LOCAL nR
    LOCAL i

    PUBLIC automapactive

    automapactive := .F.
    cheat_amap := CHEAT( "iddt", 0 )

    cheating := 0
    grid := 0
    leveljuststarted := 1
    finit_width := SCREENWIDTH
    finit_height := SCREENHEIGHT - 32
    f_x := 0
    f_y := 0
    f_w := 0
    f_h := 0
    lightlev := 0
    amclock := 0
    m_paninc := mpoint_t():New()
    mtof_zoommul := FRACUNIT
    ftom_zoommul := FRACUNIT
    m_x := 0
    m_y := 0
    m_x2 := 0
    m_y2 := 0
    m_w := 0
    m_h := 0
    min_x := 0
    min_y := 0
    max_x := 0
    max_y := 0
    max_w := 0
    max_h := 0
    min_w := 0
    min_h := 0
    min_scale_mtof := 0
    max_scale_mtof := 0
    old_m_w := 0
    old_m_h := 0
    old_m_x := 0
    old_m_y := 0
    f_oldloc := mpoint_t():New()
    scale_mtof := INITSCALEMTOF
    scale_ftom := 0
    plr := NIL
    marknums := Array( 10 )
    markpoints := {}
    FOR i := 1 TO AM_NUMMARKPOINTS
        AAdd( markpoints, mpoint_t():New() )
    NEXT
    markpointnum := 0
    followplayer := 1
    stopped := .T.
    lastlevel := -1
    lastepisode := -1
    bigstate := 0
    nexttic := 0
    litelevels := { 0, 4, 7, 10, 12, 14, 15, 15 }
    litelevelscnt := 0
    fuck := 0
    clip_fl := fline_t():New()

    nR := Int( ( 8 * PLAYERRADIUS ) / 7 )
    player_arrow := { ;
        NewMLine( -nR + Int( nR / 8 ), 0, nR, 0 ), ;
        NewMLine( nR, 0, nR - Int( nR / 2 ), Int( nR / 4 ) ), ;
        NewMLine( nR, 0, nR - Int( nR / 2 ), -Int( nR / 4 ) ), ;
        NewMLine( -nR + Int( nR / 8 ), 0, -nR - Int( nR / 8 ), Int( nR / 4 ) ), ;
        NewMLine( -nR + Int( nR / 8 ), 0, -nR - Int( nR / 8 ), -Int( nR / 4 ) ), ;
        NewMLine( -nR + 3 * Int( nR / 8 ), 0, -nR + Int( nR / 8 ), Int( nR / 4 ) ), ;
        NewMLine( -nR + 3 * Int( nR / 8 ), 0, -nR + Int( nR / 8 ), -Int( nR / 4 ) ) }

    cheat_player_arrow := { ;
        NewMLine( -nR + Int( nR / 8 ), 0, nR, 0 ), ;
        NewMLine( nR, 0, nR - Int( nR / 2 ), Int( nR / 6 ) ), ;
        NewMLine( nR, 0, nR - Int( nR / 2 ), -Int( nR / 6 ) ), ;
        NewMLine( -nR + Int( nR / 8 ), 0, -nR - Int( nR / 8 ), Int( nR / 6 ) ), ;
        NewMLine( -nR + Int( nR / 8 ), 0, -nR - Int( nR / 8 ), -Int( nR / 6 ) ), ;
        NewMLine( -nR + 3 * Int( nR / 8 ), 0, -nR + Int( nR / 8 ), Int( nR / 6 ) ), ;
        NewMLine( -nR + 3 * Int( nR / 8 ), 0, -nR + Int( nR / 8 ), -Int( nR / 6 ) ), ;
        NewMLine( -Int( nR / 2 ), 0, -Int( nR / 2 ), -Int( nR / 6 ) ), ;
        NewMLine( -Int( nR / 2 ), -Int( nR / 6 ), -Int( nR / 2 ) + Int( nR / 6 ), -Int( nR / 6 ) ), ;
        NewMLine( -Int( nR / 2 ) + Int( nR / 6 ), -Int( nR / 6 ), -Int( nR / 2 ) + Int( nR / 6 ), Int( nR / 4 ) ), ;
        NewMLine( -Int( nR / 6 ), 0, -Int( nR / 6 ), -Int( nR / 6 ) ), ;
        NewMLine( -Int( nR / 6 ), -Int( nR / 6 ), 0, -Int( nR / 6 ) ), ;
        NewMLine( 0, -Int( nR / 6 ), 0, Int( nR / 4 ) ), ;
        NewMLine( Int( nR / 6 ), Int( nR / 4 ), Int( nR / 6 ), -Int( nR / 7 ) ), ;
        NewMLine( Int( nR / 6 ), -Int( nR / 7 ), Int( nR / 6 ) + Int( nR / 32 ), -Int( nR / 7 ) - Int( nR / 32 ) ), ;
        NewMLine( Int( nR / 6 ) + Int( nR / 32 ), -Int( nR / 7 ) - Int( nR / 32 ), Int( nR / 6 ) + Int( nR / 10 ), -Int( nR / 7 ) ) }

    triangle_guy := { ;
        NewMLine( Int( -0.867 * FRACUNIT ), Int( -0.5 * FRACUNIT ), Int( 0.867 * FRACUNIT ), Int( -0.5 * FRACUNIT ) ), ;
        NewMLine( Int( 0.867 * FRACUNIT ), Int( -0.5 * FRACUNIT ), 0, FRACUNIT ), ;
        NewMLine( 0, FRACUNIT, Int( -0.867 * FRACUNIT ), Int( -0.5 * FRACUNIT ) ) }

    thintriangle_guy := { ;
        NewMLine( Int( -0.5 * FRACUNIT ), Int( -0.7 * FRACUNIT ), FRACUNIT, 0 ), ;
        NewMLine( FRACUNIT, 0, Int( -0.5 * FRACUNIT ), Int( 0.7 * FRACUNIT ) ), ;
        NewMLine( Int( -0.5 * FRACUNIT ), Int( 0.7 * FRACUNIT ), Int( -0.5 * FRACUNIT ), Int( -0.7 * FRACUNIT ) ) }
RETURN

PROCEDURE AM_getIslope( ml, is )
    LOCAL dx
    LOCAL dy

    dy := ml:a:y - ml:b:y
    dx := ml:b:x - ml:a:x
    IF dy == 0
        is:islp := iif( dx < 0, -INT_MAX, INT_MAX )
    ELSE
        is:islp := FixedDiv( dx, dy )
    ENDIF
    IF dx == 0
        is:slp := iif( dy < 0, -INT_MAX, INT_MAX )
    ELSE
        is:slp := FixedDiv( dy, dx )
    ENDIF
RETURN

PROCEDURE AM_activateNewScale()
    m_x += Int( m_w / 2 )
    m_y += Int( m_h / 2 )
    m_w := FTOM( f_w )
    m_h := FTOM( f_h )
    m_x -= Int( m_w / 2 )
    m_y -= Int( m_h / 2 )
    m_x2 := m_x + m_w
    m_y2 := m_y + m_h
RETURN

PROCEDURE AM_saveScaleAndLoc()
    old_m_x := m_x
    old_m_y := m_y
    old_m_w := m_w
    old_m_h := m_h
RETURN

PROCEDURE AM_restoreScaleAndLoc()
    m_w := old_m_w
    m_h := old_m_h
    IF followplayer == 0
        m_x := old_m_x
        m_y := old_m_y
    ELSE
        m_x := plr:mo:x - Int( m_w / 2 )
        m_y := plr:mo:y - Int( m_h / 2 )
    ENDIF
    m_x2 := m_x + m_w
    m_y2 := m_y + m_h
    scale_mtof := FixedDiv( f_w * FRACUNIT, m_w )
    scale_ftom := FixedDiv( FRACUNIT, scale_mtof )
RETURN

PROCEDURE AM_addMark()
    markpoints[ markpointnum + 1 ]:x := m_x + Int( m_w / 2 )
    markpoints[ markpointnum + 1 ]:y := m_y + Int( m_h / 2 )
    markpointnum := ( markpointnum + 1 ) % AM_NUMMARKPOINTS
RETURN

PROCEDURE AM_findMinMaxBoundaries()
    LOCAL i
    LOCAL a
    LOCAL b
    MEMVAR numvertexes
    MEMVAR vertexes

    min_x := INT_MAX
    min_y := INT_MAX
    max_x := -INT_MAX
    max_y := -INT_MAX

    FOR i := 0 TO numvertexes - 1
        IF vertexes[ i + 1 ]:x < min_x
            min_x := vertexes[ i + 1 ]:x
        ELSEIF vertexes[ i + 1 ]:x > max_x
            max_x := vertexes[ i + 1 ]:x
        ENDIF
        IF vertexes[ i + 1 ]:y < min_y
            min_y := vertexes[ i + 1 ]:y
        ELSEIF vertexes[ i + 1 ]:y > max_y
            max_y := vertexes[ i + 1 ]:y
        ENDIF
    NEXT

    max_w := max_x - min_x
    max_h := max_y - min_y
    min_w := 2 * PLAYERRADIUS
    min_h := 2 * PLAYERRADIUS

    a := FixedDiv( f_w * FRACUNIT, max_w )
    b := FixedDiv( f_h * FRACUNIT, max_h )
    min_scale_mtof := iif( a < b, a, b )
    max_scale_mtof := FixedDiv( f_h * FRACUNIT, 2 * PLAYERRADIUS )
RETURN

PROCEDURE AM_changeWindowLoc()
    IF m_paninc:x != 0 .OR. m_paninc:y != 0
        followplayer := 0
        f_oldloc:x := INT_MAX
    ENDIF

    m_x += m_paninc:x
    m_y += m_paninc:y

    IF m_x + Int( m_w / 2 ) > max_x
        m_x := max_x - Int( m_w / 2 )
    ELSEIF m_x + Int( m_w / 2 ) < min_x
        m_x := min_x - Int( m_w / 2 )
    ENDIF

    IF m_y + Int( m_h / 2 ) > max_y
        m_y := max_y - Int( m_h / 2 )
    ELSEIF m_y + Int( m_h / 2 ) < min_y
        m_y := min_y - Int( m_h / 2 )
    ENDIF

    m_x2 := m_x + m_w
    m_y2 := m_y + m_h
RETURN

PROCEDURE AM_initVariables()
    LOCAL pnum
    LOCAL st_notify
    MEMVAR automapactive
    MEMVAR consoleplayer
    MEMVAR playeringame
    MEMVAR players

    automapactive := .T.

    f_oldloc:x := INT_MAX
    amclock := 0
    lightlev := 0

    m_paninc:x := 0
    m_paninc:y := 0
    ftom_zoommul := FRACUNIT
    mtof_zoommul := FRACUNIT

    m_w := FTOM( f_w )
    m_h := FTOM( f_h )

    IF playeringame[ consoleplayer + 1 ]
        plr := players[ consoleplayer + 1 ]
    ELSE
        plr := players[ 1 ]
        FOR pnum := 0 TO MAXPLAYERS - 1
            IF playeringame[ pnum + 1 ]
                plr := players[ pnum + 1 ]
                EXIT
            ENDIF
        NEXT
    ENDIF

    m_x := plr:mo:x - Int( m_w / 2 )
    m_y := plr:mo:y - Int( m_h / 2 )
    AM_changeWindowLoc()

    old_m_x := m_x
    old_m_y := m_y
    old_m_w := m_w
    old_m_h := m_h

    st_notify := event_t():New()
    st_notify:type := ev_keyup
    st_notify:data1 := AM_MSGENTERED
    st_notify:data2 := 0
    st_notify:data3 := 0
    ST_Responder( st_notify )
RETURN

PROCEDURE AM_loadPics()
    LOCAL i
    LOCAL namebuf

    FOR i := 0 TO 9
        namebuf := "AMMNUM" + LTrim( Str( i ) )
        marknums[ i + 1 ] := W_CacheLumpName( namebuf, PU_STATIC )
    NEXT
RETURN

PROCEDURE AM_unloadPics()
    LOCAL i
    LOCAL namebuf

    FOR i := 0 TO 9
        namebuf := "AMMNUM" + LTrim( Str( i ) )
        W_ReleaseLumpName( namebuf )
    NEXT
RETURN

PROCEDURE AM_clearMarks()
    LOCAL i

    FOR i := 0 TO AM_NUMMARKPOINTS - 1
        markpoints[ i + 1 ]:x := -1
    NEXT
    markpointnum := 0
RETURN

PROCEDURE AM_LevelInit()
    leveljuststarted := 0
    f_x := 0
    f_y := 0
    f_w := finit_width
    f_h := finit_height
    AM_clearMarks()
    AM_findMinMaxBoundaries()
    scale_mtof := FixedDiv( min_scale_mtof, Int( 0.7 * FRACUNIT ) )
    IF scale_mtof > max_scale_mtof
        scale_mtof := min_scale_mtof
    ENDIF
    scale_ftom := FixedDiv( FRACUNIT, scale_mtof )
RETURN

PROCEDURE AM_Stop()
    LOCAL st_notify
    MEMVAR automapactive

    AM_unloadPics()
    automapactive := .F.
    st_notify := event_t():New()
    st_notify:type := 0
    st_notify:data1 := ev_keyup
    st_notify:data2 := AM_MSGEXITED
    st_notify:data3 := 0
    ST_Responder( st_notify )
    stopped := .T.
RETURN

PROCEDURE AM_Start()
    MEMVAR gameepisode
    MEMVAR gamemap
    IF ! stopped
        AM_Stop()
    ENDIF
    stopped := .F.
    IF lastlevel != gamemap .OR. lastepisode != gameepisode
        AM_LevelInit()
        lastlevel := gamemap
        lastepisode := gameepisode
    ENDIF
    AM_initVariables()
    AM_loadPics()
RETURN

PROCEDURE AM_minOutWindowScale()
    scale_mtof := min_scale_mtof
    scale_ftom := FixedDiv( FRACUNIT, scale_mtof )
    AM_activateNewScale()
RETURN

PROCEDURE AM_maxOutWindowScale()
    scale_mtof := max_scale_mtof
    scale_ftom := FixedDiv( FRACUNIT, scale_mtof )
    AM_activateNewScale()
RETURN

FUNCTION AM_Responder( ev )
    LOCAL rc
    LOCAL key
    LOCAL buffer
    MEMVAR automapactive
    MEMVAR deathmatch
    MEMVAR key_map_clearmark
    MEMVAR key_map_east
    MEMVAR key_map_follow
    MEMVAR key_map_grid
    MEMVAR key_map_mark
    MEMVAR key_map_maxzoom
    MEMVAR key_map_north
    MEMVAR key_map_south
    MEMVAR key_map_toggle
    MEMVAR key_map_west
    MEMVAR key_map_zoomin
    MEMVAR key_map_zoomout
    MEMVAR viewactive

    rc := .F.

    IF ! automapactive
        IF ev:type == ev_keydown .AND. ev:data1 == key_map_toggle
            AM_Start()
            viewactive := .F.
            rc := .T.
        ENDIF
    ELSEIF ev:type == ev_keydown
        rc := .T.
        key := ev:data1

        IF key == key_map_east
            IF followplayer == 0
                m_paninc:x := FTOM( F_PANINC )
            ELSE
                rc := .F.
            ENDIF
        ELSEIF key == key_map_west
            IF followplayer == 0
                m_paninc:x := -FTOM( F_PANINC )
            ELSE
                rc := .F.
            ENDIF
        ELSEIF key == key_map_north
            IF followplayer == 0
                m_paninc:y := FTOM( F_PANINC )
            ELSE
                rc := .F.
            ENDIF
        ELSEIF key == key_map_south
            IF followplayer == 0
                m_paninc:y := -FTOM( F_PANINC )
            ELSE
                rc := .F.
            ENDIF
        ELSEIF key == key_map_zoomout
            mtof_zoommul := M_ZOOMOUT
            ftom_zoommul := M_ZOOMIN
        ELSEIF key == key_map_zoomin
            mtof_zoommul := M_ZOOMIN
            ftom_zoommul := M_ZOOMOUT
        ELSEIF key == key_map_toggle
            bigstate := 0
            viewactive := .T.
            AM_Stop()
        ELSEIF key == key_map_maxzoom
            bigstate := iif( bigstate == 0, 1, 0 )
            IF bigstate != 0
                AM_saveScaleAndLoc()
                AM_minOutWindowScale()
            ELSE
                AM_restoreScaleAndLoc()
            ENDIF
        ELSEIF key == key_map_follow
            followplayer := iif( followplayer == 0, 1, 0 )
            f_oldloc:x := INT_MAX
            IF followplayer != 0
                plr:message := DEH_String( AMSTR_FOLLOWON )
            ELSE
                plr:message := DEH_String( AMSTR_FOLLOWOFF )
            ENDIF
        ELSEIF key == key_map_grid
            grid := iif( grid == 0, 1, 0 )
            IF grid != 0
                plr:message := DEH_String( AMSTR_GRIDON )
            ELSE
                plr:message := DEH_String( AMSTR_GRIDOFF )
            ENDIF
        ELSEIF key == key_map_mark
            buffer := ""
            M_snprintf( @buffer, 20, "%s %d", DEH_String( AMSTR_MARKEDSPOT ), markpointnum )
            plr:message := buffer
            AM_addMark()
        ELSEIF key == key_map_clearmark
            AM_clearMarks()
            plr:message := DEH_String( AMSTR_MARKSCLEARED )
        ELSE
            rc := .F.
        ENDIF

        IF deathmatch == 0 .AND. cht_CheckCheat( cheat_amap, ev:data2 )
            rc := .F.
            cheating := ( cheating + 1 ) % 3
        ENDIF
    ELSEIF ev:type == ev_keyup
        rc := .F.
        key := ev:data1
        IF key == key_map_east
            IF followplayer == 0
                m_paninc:x := 0
            ENDIF
        ELSEIF key == key_map_west
            IF followplayer == 0
                m_paninc:x := 0
            ENDIF
        ELSEIF key == key_map_north
            IF followplayer == 0
                m_paninc:y := 0
            ENDIF
        ELSEIF key == key_map_south
            IF followplayer == 0
                m_paninc:y := 0
            ENDIF
        ELSEIF key == key_map_zoomout .OR. key == key_map_zoomin
            mtof_zoommul := FRACUNIT
            ftom_zoommul := FRACUNIT
        ENDIF
    ENDIF
RETURN rc

PROCEDURE AM_changeWindowScale()
    scale_mtof := FixedMul( scale_mtof, mtof_zoommul )
    scale_ftom := FixedDiv( FRACUNIT, scale_mtof )
    IF scale_mtof < min_scale_mtof
        AM_minOutWindowScale()
    ELSEIF scale_mtof > max_scale_mtof
        AM_maxOutWindowScale()
    ELSE
        AM_activateNewScale()
    ENDIF
RETURN

PROCEDURE AM_doFollowPlayer()
    IF f_oldloc:x != plr:mo:x .OR. f_oldloc:y != plr:mo:y
        m_x := FTOM( MTOF( plr:mo:x ) ) - Int( m_w / 2 )
        m_y := FTOM( MTOF( plr:mo:y ) ) - Int( m_h / 2 )
        m_x2 := m_x + m_w
        m_y2 := m_y + m_h
        f_oldloc:x := plr:mo:x
        f_oldloc:y := plr:mo:y
    ENDIF
RETURN

PROCEDURE AM_updateLightLev()
    IF amclock > nexttic
        lightlev := litelevels[ litelevelscnt + 1 ]
        litelevelscnt++
        IF litelevelscnt == Len( litelevels )
            litelevelscnt := 0
        ENDIF
        nexttic := amclock + 6 - ( amclock % 6 )
    ENDIF
RETURN

PROCEDURE AM_Ticker()
    MEMVAR automapactive
    IF ! automapactive
        RETURN
    ENDIF
    amclock++
    IF followplayer != 0
        AM_doFollowPlayer()
    ENDIF
    IF ftom_zoommul != FRACUNIT
        AM_changeWindowScale()
    ENDIF
    IF m_paninc:x != 0 .OR. m_paninc:y != 0
        AM_changeWindowLoc()
    ENDIF
RETURN

PROCEDURE AM_clearFB( color )
    LOCAL nLen
    MEMVAR I_VideoBuffer
    nLen := f_w * f_h
    I_VideoBuffer := Stuff( I_VideoBuffer, 1, nLen, Replicate( Chr( color & 0xFF ), nLen ) )
RETURN

STATIC PROCEDURE DoOutCode( oc, mx, my )
    oc := 0
    IF my < 0
        oc := ( oc | OC_TOP )
    ELSEIF my >= f_h
        oc := ( oc | OC_BOTTOM )
    ENDIF
    IF mx < 0
        oc := ( oc | OC_LEFT )
    ELSEIF mx >= f_w
        oc := ( oc | OC_RIGHT )
    ENDIF
RETURN

FUNCTION AM_clipMline( ml, fl )
    LOCAL outcode1 := 0
    LOCAL outcode2 := 0
    LOCAL outside
    LOCAL tmp
    LOCAL dx
    LOCAL dy

    tmp := fpoint_t():New()

    IF ml:a:y > m_y2
        outcode1 := OC_TOP
    ELSEIF ml:a:y < m_y
        outcode1 := OC_BOTTOM
    ENDIF

    IF ml:b:y > m_y2
        outcode2 := OC_TOP
    ELSEIF ml:b:y < m_y
        outcode2 := OC_BOTTOM
    ENDIF

    IF ( outcode1 & outcode2 ) != 0
        RETURN .F.
    ENDIF

    IF ml:a:x < m_x
        outcode1 := ( outcode1 | OC_LEFT )
    ELSEIF ml:a:x > m_x2
        outcode1 := ( outcode1 | OC_RIGHT )
    ENDIF

    IF ml:b:x < m_x
        outcode2 := ( outcode2 | OC_LEFT )
    ELSEIF ml:b:x > m_x2
        outcode2 := ( outcode2 | OC_RIGHT )
    ENDIF

    IF ( outcode1 & outcode2 ) != 0
        RETURN .F.
    ENDIF

    fl:a:x := CXMTOF( ml:a:x )
    fl:a:y := CYMTOF( ml:a:y )
    fl:b:x := CXMTOF( ml:b:x )
    fl:b:y := CYMTOF( ml:b:y )

    DoOutCode( @outcode1, fl:a:x, fl:a:y )
    DoOutCode( @outcode2, fl:b:x, fl:b:y )

    IF ( outcode1 & outcode2 ) != 0
        RETURN .F.
    ENDIF

    DO WHILE ( outcode1 | outcode2 ) != 0
        IF outcode1 != 0
            outside := outcode1
        ELSE
            outside := outcode2
        ENDIF

        IF ( outside & OC_TOP ) != 0
            dy := fl:a:y - fl:b:y
            dx := fl:b:x - fl:a:x
            tmp:x := fl:a:x + Int( ( dx * fl:a:y ) / dy )
            tmp:y := 0
        ELSEIF ( outside & OC_BOTTOM ) != 0
            dy := fl:a:y - fl:b:y
            dx := fl:b:x - fl:a:x
            tmp:x := fl:a:x + Int( ( dx * ( fl:a:y - f_h ) ) / dy )
            tmp:y := f_h - 1
        ELSEIF ( outside & OC_RIGHT ) != 0
            dy := fl:b:y - fl:a:y
            dx := fl:b:x - fl:a:x
            tmp:y := fl:a:y + Int( ( dy * ( f_w - 1 - fl:a:x ) ) / dx )
            tmp:x := f_w - 1
        ELSEIF ( outside & OC_LEFT ) != 0
            dy := fl:b:y - fl:a:y
            dx := fl:b:x - fl:a:x
            tmp:y := fl:a:y + Int( ( dy * ( -fl:a:x ) ) / dx )
            tmp:x := 0
        ELSE
            tmp:x := 0
            tmp:y := 0
        ENDIF

        IF outside == outcode1
            fl:a:x := tmp:x
            fl:a:y := tmp:y
            DoOutCode( @outcode1, fl:a:x, fl:a:y )
        ELSE
            fl:b:x := tmp:x
            fl:b:y := tmp:y
            DoOutCode( @outcode2, fl:b:x, fl:b:y )
        ENDIF

        IF ( outcode1 & outcode2 ) != 0
            RETURN .F.
        ENDIF
    ENDDO
RETURN .T.

PROCEDURE AM_drawFline( fl, color )
    LOCAL x
    LOCAL y
    LOCAL dx
    LOCAL dy
    LOCAL sx
    LOCAL sy
    LOCAL ax
    LOCAL ay
    LOCAL d

    IF fl:a:x < 0 .OR. fl:a:x >= f_w ;
            .OR. fl:a:y < 0 .OR. fl:a:y >= f_h ;
            .OR. fl:b:x < 0 .OR. fl:b:x >= f_w ;
            .OR. fl:b:y < 0 .OR. fl:b:y >= f_h
        DEH_fprintf( NIL, "fuck " + LTrim( Str( fuck ) ) + " " + Chr( 13 ) )
        fuck++
        RETURN
    ENDIF

    dx := fl:b:x - fl:a:x
    ax := 2 * iif( dx < 0, -dx, dx )
    sx := iif( dx < 0, -1, 1 )

    dy := fl:b:y - fl:a:y
    ay := 2 * iif( dy < 0, -dy, dy )
    sy := iif( dy < 0, -1, 1 )

    x := fl:a:x
    y := fl:a:y

    IF ax > ay
        d := ay - Int( ax / 2 )
        DO WHILE .T.
            PutDot( x, y, color )
            IF x == fl:b:x
                RETURN
            ENDIF
            IF d >= 0
                y += sy
                d -= ax
            ENDIF
            x += sx
            d += ay
        ENDDO
    ELSE
        d := ax - Int( ay / 2 )
        DO WHILE .T.
            PutDot( x, y, color )
            IF y == fl:b:y
                RETURN
            ENDIF
            IF d >= 0
                x += sx
                d -= ay
            ENDIF
            y += sy
            d += ax
        ENDDO
    ENDIF
RETURN

PROCEDURE AM_drawMline( ml, color )
    IF AM_clipMline( ml, clip_fl )
        AM_drawFline( clip_fl, color )
    ENDIF
RETURN

PROCEDURE AM_drawGrid( color )
    LOCAL x
    LOCAL y
    LOCAL start
    LOCAL nEnd
    LOCAL ml
    LOCAL nBlock
    MEMVAR bmaporgx
    MEMVAR bmaporgy

    ml := mline_t():New()
    nBlock := MAPBLOCKUNITS * FRACUNIT

    start := m_x
    IF ( ( start - bmaporgx ) % nBlock ) != 0
        start += nBlock - ( ( start - bmaporgx ) % nBlock )
    ENDIF
    nEnd := m_x + m_w

    ml:a:y := m_y
    ml:b:y := m_y + m_h
    FOR x := start TO nEnd - 1 STEP nBlock
        ml:a:x := x
        ml:b:x := x
        AM_drawMline( ml, color )
    NEXT

    start := m_y
    IF ( ( start - bmaporgy ) % nBlock ) != 0
        start += nBlock - ( ( start - bmaporgy ) % nBlock )
    ENDIF
    nEnd := m_y + m_h

    ml:a:x := m_x
    ml:b:x := m_x + m_w
    FOR y := start TO nEnd - 1 STEP nBlock
        ml:a:y := y
        ml:b:y := y
        AM_drawMline( ml, color )
    NEXT
RETURN

PROCEDURE AM_drawWalls()
    LOCAL i
    LOCAL l
    LOCAL line
    MEMVAR lines
    MEMVAR numlines

    l := mline_t():New()

    FOR i := 0 TO numlines - 1
        line := lines[ i + 1 ]
        l:a:x := line:v1:x
        l:a:y := line:v1:y
        l:b:x := line:v2:x
        l:b:y := line:v2:y
        IF cheating != 0 .OR. ( line:flags & ML_MAPPED ) != 0
            IF ( line:flags & LINE_NEVERSEE ) != 0 .AND. cheating == 0
                LOOP
            ENDIF
            IF line:backsector == NIL
                AM_drawMline( l, WALLCOLORS + lightlev )
            ELSE
                IF line:special == 39
                    AM_drawMline( l, WALLCOLORS + Int( WALLRANGE / 2 ) )
                ELSEIF ( line:flags & ML_SECRET ) != 0
                    IF cheating != 0
                        AM_drawMline( l, SECRETWALLCOLORS + lightlev )
                    ELSE
                        AM_drawMline( l, WALLCOLORS + lightlev )
                    ENDIF
                ELSEIF line:backsector:floorheight != line:frontsector:floorheight
                    AM_drawMline( l, FDWALLCOLORS + lightlev )
                ELSEIF line:backsector:ceilingheight != line:frontsector:ceilingheight
                    AM_drawMline( l, CDWALLCOLORS + lightlev )
                ELSEIF cheating != 0
                    AM_drawMline( l, TSWALLCOLORS + lightlev )
                ENDIF
            ENDIF
        ELSEIF plr:powers[ pw_allmap + 1 ] != 0
            IF ( line:flags & LINE_NEVERSEE ) == 0
                AM_drawMline( l, GRAYS + 3 )
            ENDIF
        ENDIF
    NEXT
RETURN

PROCEDURE AM_rotate( x, y, a )
    LOCAL tmpx
    LOCAL nFine
    MEMVAR finecosine
    MEMVAR finesine

    nFine := UShr( a, ANGLETOFINESHIFT ) + 1
    tmpx := FixedMul( x, finecosine[ nFine ] ) - FixedMul( y, finesine[ nFine ] )
    y := FixedMul( x, finesine[ nFine ] ) + FixedMul( y, finecosine[ nFine ] )
    x := tmpx
RETURN

PROCEDURE AM_drawLineCharacter( lineguy, lineguylines, scale, angle, color, x, y )
    LOCAL i
    LOCAL l
    LOCAL nAx
    LOCAL nAy
    LOCAL nBx
    LOCAL nBy

    l := mline_t():New()

    FOR i := 0 TO lineguylines - 1
        nAx := lineguy[ i + 1 ]:a:x
        nAy := lineguy[ i + 1 ]:a:y
        IF scale != 0
            nAx := FixedMul( scale, nAx )
            nAy := FixedMul( scale, nAy )
        ENDIF
        IF angle != 0
            AM_rotate( @nAx, @nAy, angle )
        ENDIF
        l:a:x := nAx + x
        l:a:y := nAy + y

        nBx := lineguy[ i + 1 ]:b:x
        nBy := lineguy[ i + 1 ]:b:y
        IF scale != 0
            nBx := FixedMul( scale, nBx )
            nBy := FixedMul( scale, nBy )
        ENDIF
        IF angle != 0
            AM_rotate( @nBx, @nBy, angle )
        ENDIF
        l:b:x := nBx + x
        l:b:y := nBy + y

        AM_drawMline( l, color )
    NEXT
RETURN

PROCEDURE AM_drawPlayers()
    LOCAL i
    LOCAL p
    LOCAL their_colors
    LOCAL their_color
    LOCAL color
    MEMVAR deathmatch
    MEMVAR netgame
    MEMVAR playeringame
    MEMVAR players
    MEMVAR singledemo

    their_colors := { GREENS, GRAYS, BROWNS, REDS }
    their_color := -1

    IF ! netgame
        IF cheating != 0
            AM_drawLineCharacter( cheat_player_arrow, Len( cheat_player_arrow ), 0, ;
                plr:mo:angle, WHITE, plr:mo:x, plr:mo:y )
        ELSE
            AM_drawLineCharacter( player_arrow, Len( player_arrow ), 0, ;
                plr:mo:angle, WHITE, plr:mo:x, plr:mo:y )
        ENDIF
        RETURN
    ENDIF

    FOR i := 0 TO MAXPLAYERS - 1
        their_color++
        p := players[ i + 1 ]
        IF ( deathmatch != 0 .AND. ! singledemo ) .AND. p != plr
            LOOP
        ENDIF
        IF ! playeringame[ i + 1 ]
            LOOP
        ENDIF
        IF p:powers[ pw_invisibility + 1 ] != 0
            color := 246
        ELSE
            color := their_colors[ their_color + 1 ]
        ENDIF
        AM_drawLineCharacter( player_arrow, Len( player_arrow ), 0, p:mo:angle, ;
            color, p:mo:x, p:mo:y )
    NEXT
RETURN

PROCEDURE AM_drawThings( colors, colorrange )
    LOCAL i
    LOCAL t
    MEMVAR numsectors
    MEMVAR sectors

    HB_SYMBOL_UNUSED( colorrange )

    FOR i := 0 TO numsectors - 1
        t := sectors[ i + 1 ]:thinglist
        DO WHILE t != NIL
            AM_drawLineCharacter( thintriangle_guy, Len( thintriangle_guy ), ;
                16 * FRACUNIT, t:angle, colors + lightlev, t:x, t:y )
            t := t:snext
        ENDDO
    NEXT
RETURN

PROCEDURE AM_drawMarks()
    LOCAL i
    LOCAL fx
    LOCAL fy
    LOCAL w
    LOCAL h

    FOR i := 0 TO AM_NUMMARKPOINTS - 1
        IF markpoints[ i + 1 ]:x != -1
            w := 5
            h := 6
            fx := CXMTOF( markpoints[ i + 1 ]:x )
            fy := CYMTOF( markpoints[ i + 1 ]:y )
            IF fx >= f_x .AND. fx <= f_w - w .AND. fy >= f_y .AND. fy <= f_h - h
                V_DrawPatch( fx, fy, marknums[ i + 1 ] )
            ENDIF
        ENDIF
    NEXT
RETURN

PROCEDURE AM_drawCrosshair( color )
    LOCAL nOff
    MEMVAR I_VideoBuffer
    nOff := Int( ( f_w * ( f_h + 1 ) ) / 2 )
    VidBSet( @I_VideoBuffer, nOff, color )
RETURN

PROCEDURE AM_Drawer()
    MEMVAR automapactive
    IF ! automapactive
        RETURN
    ENDIF
    AM_clearFB( BACKGROUND )
    IF grid != 0
        AM_drawGrid( GRIDCOLORS )
    ENDIF
    AM_drawWalls()
    AM_drawPlayers()
    IF cheating == 2
        AM_drawThings( THINGCOLORS, THINGRANGE )
    ENDIF
    AM_drawCrosshair( XHAIRCOLORS )
    AM_drawMarks()
    V_MarkRect( f_x, f_y, f_w, f_h )
RETURN

FUNCTION AM_DehCheatAmap()
RETURN cheat_amap
