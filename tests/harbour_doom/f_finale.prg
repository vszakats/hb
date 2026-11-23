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

STATIC textscreens := {}
STATIC castorder := {}
STATIC laststage := 0
STATIC castnum
STATIC casttics
STATIC caststate
STATIC castdeath
STATIC castframes
STATIC castonmelee
STATIC castattacking
STATIC finalestage
STATIC finalecount
STATIC finaletext
STATIC finaleflat

#include "deh_main.ch"
#include "d_englsh.ch"
#include "dstrings.ch"
#include "doomstat.ch"
#include "f_finale.ch"

CLASS textscreen_t
    DATA mission
    DATA episode
    DATA level
    DATA background
    DATA text
    METHOD New()
ENDCLASS
CLASS castinfo_t
    DATA name
    DATA type
    METHOD New()
ENDCLASS

#ifndef SCREENWIDTH
#define SCREENWIDTH  320
#define SCREENHEIGHT 200
#endif

#ifndef PU_CACHE
#define PU_STATIC 1
#define PU_LEVEL  5
#define PU_CACHE  8
#endif

#ifndef HU_FONTSTART
#define HU_FONTSTART 33
#define HU_FONTSIZE  63
#endif

#ifndef FF_FRAMEMASK
#define FF_FRAMEMASK 32767
#endif

#ifndef SHORT
#define SHORT( x ) ( x )
#define LONG( x )  ( x )
#endif

#ifndef mus_bunny
#define mus_bunny   30
#define mus_victor  31
#define mus_evil    63
#define mus_read_m  65
#endif

#ifndef sfx_pistol
#define sfx_pistol  1
#define sfx_shotgn  2
#define sfx_dshtgn  4
#define sfx_plasma  8
#define sfx_rlaunc  14
#define sfx_firsht  16
#define sfx_sklatk  51
#define sfx_sgtatk  52
#define sfx_skepch  53
#define sfx_vilatk  54
#define sfx_claw    55
#define sfx_skeswg  56
#define sfx_skeatk  107
#endif

#ifndef S_NULL
#define S_NULL        0
#define S_PLAY_ATK1   154
#define S_POSS_ATK2   185
#define S_SPOS_ATK2   218
#define S_VILE_ATK2   256
#define S_SKEL_FIST2  336
#define S_SKEL_FIST4  338
#define S_SKEL_MISS2  340
#define S_FATT_ATK2   377
#define S_FATT_ATK5   380
#define S_FATT_ATK8   383
#define S_CPOS_ATK2   417
#define S_CPOS_ATK3   418
#define S_CPOS_ATK4   419
#define S_TROO_ATK3   454
#define S_SARG_ATK2   486
#define S_HEAD_ATK2   505
#define S_BOSS_ATK2   538
#define S_BOS2_ATK2   567
#define S_SKULL_ATK2  590
#define S_SPID_ATK2   616
#define S_SPID_ATK3   617
#define S_BSPI_ATK2   648
#define S_CYBER_ATK2  685
#define S_CYBER_ATK4  687
#define S_CYBER_ATK6  689
#define S_PAIN_ATK3   710
#endif

#ifndef MT_PLAYER
#define MT_PLAYER     0
#define MT_POSSESSED  1
#define MT_SHOTGUY    2
#define MT_VILE       3
#define MT_UNDEAD     5
#define MT_FATSO      8
#define MT_CHAINGUY   10
#define MT_TROOP      11
#define MT_SERGEANT   12
#define MT_HEAD       14
#define MT_BRUISER    15
#define MT_KNIGHT     17
#define MT_SKULL      18
#define MT_SPIDER     19
#define MT_BABY       20
#define MT_CYBORG     21
#define MT_PAIN       22
#endif



METHOD New( mission, episode, level, background, text ) CLASS textscreen_t
    ::mission    := iif( mission == NIL, 0, mission )
    ::episode    := iif( episode == NIL, 0, episode )
    ::level      := iif( level == NIL, 0, level )
    ::background := iif( background == NIL, "", background )
    ::text       := iif( text == NIL, "", text )
RETURN Self

METHOD New( name, type ) CLASS castinfo_t
    ::name := name
    ::type := iif( type == NIL, 0, type )
RETURN Self

INIT PROCEDURE init_f_finale


    finalestage := F_STAGE_TEXT
    finalecount := 0
    finaletext := NIL
    finaleflat := NIL

    textscreens := {}
    AAdd( textscreens, textscreen_t():New( doom,      1, 8,  "FLOOR4_8", E1TEXT ) )
    AAdd( textscreens, textscreen_t():New( doom,      2, 8,  "SFLR6_1",  E2TEXT ) )
    AAdd( textscreens, textscreen_t():New( doom,      3, 8,  "MFLR8_4",  E3TEXT ) )
    AAdd( textscreens, textscreen_t():New( doom,      4, 8,  "MFLR8_3",  E4TEXT ) )
    AAdd( textscreens, textscreen_t():New( doom2,     1, 6,  "SLIME16",  C1TEXT ) )
    AAdd( textscreens, textscreen_t():New( doom2,     1, 11, "RROCK14",  C2TEXT ) )
    AAdd( textscreens, textscreen_t():New( doom2,     1, 20, "RROCK07",  C3TEXT ) )
    AAdd( textscreens, textscreen_t():New( doom2,     1, 30, "RROCK17",  C4TEXT ) )
    AAdd( textscreens, textscreen_t():New( doom2,     1, 15, "RROCK13",  C5TEXT ) )
    AAdd( textscreens, textscreen_t():New( doom2,     1, 31, "RROCK19",  C6TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_tnt,  1, 6,  "SLIME16",  T1TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_tnt,  1, 11, "RROCK14",  T2TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_tnt,  1, 20, "RROCK07",  T3TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_tnt,  1, 30, "RROCK17",  T4TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_tnt,  1, 15, "RROCK13",  T5TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_tnt,  1, 31, "RROCK19",  T6TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_plut, 1, 6,  "SLIME16",  P1TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_plut, 1, 11, "RROCK14",  P2TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_plut, 1, 20, "RROCK07",  P3TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_plut, 1, 30, "RROCK17",  P4TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_plut, 1, 15, "RROCK13",  P5TEXT ) )
    AAdd( textscreens, textscreen_t():New( pack_plut, 1, 31, "RROCK19",  P6TEXT ) )

    castorder := {}
    AAdd( castorder, castinfo_t():New( CC_ZOMBIE, MT_POSSESSED ) )
    AAdd( castorder, castinfo_t():New( CC_SHOTGUN, MT_SHOTGUY ) )
    AAdd( castorder, castinfo_t():New( CC_HEAVY, MT_CHAINGUY ) )
    AAdd( castorder, castinfo_t():New( CC_IMP, MT_TROOP ) )
    AAdd( castorder, castinfo_t():New( CC_DEMON, MT_SERGEANT ) )
    AAdd( castorder, castinfo_t():New( CC_LOST, MT_SKULL ) )
    AAdd( castorder, castinfo_t():New( CC_CACO, MT_HEAD ) )
    AAdd( castorder, castinfo_t():New( CC_HELL, MT_KNIGHT ) )
    AAdd( castorder, castinfo_t():New( CC_BARON, MT_BRUISER ) )
    AAdd( castorder, castinfo_t():New( CC_ARACH, MT_BABY ) )
    AAdd( castorder, castinfo_t():New( CC_PAIN, MT_PAIN ) )
    AAdd( castorder, castinfo_t():New( CC_REVEN, MT_UNDEAD ) )
    AAdd( castorder, castinfo_t():New( CC_MANCU, MT_FATSO ) )
    AAdd( castorder, castinfo_t():New( CC_ARCH, MT_VILE ) )
    AAdd( castorder, castinfo_t():New( CC_SPIDER, MT_SPIDER ) )
    AAdd( castorder, castinfo_t():New( CC_CYBER, MT_CYBORG ) )
    AAdd( castorder, castinfo_t():New( CC_HERO, MT_PLAYER ) )
    AAdd( castorder, castinfo_t():New( NIL, 0 ) )

    laststage := 0
    castnum := 0
    casttics := 0
    caststate := S_NULL
    castdeath := .F.
    castframes := 0
    castonmelee := 0
    castattacking := .F.
RETURN

STATIC FUNCTION CastType()
RETURN castorder[ castnum + 1 ]:type

STATIC FUNCTION CastInfo()
    MEMVAR mobjinfo
RETURN mobjinfo[ CastType() + 1 ]

STATIC FUNCTION CastState()
    MEMVAR states
RETURN states[ caststate + 1 ]

STATIC PROCEDURE CastStopAttack()
    castattacking := .F.
    castframes := 0
    caststate := CastInfo():seestate
RETURN

FUNCTION F_StartFinale()
    LOCAL i
    LOCAL screen
    MEMVAR automapactive
    MEMVAR gameaction
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gamestate
    MEMVAR gameversion
    MEMVAR viewactive

    gameaction := ga_nothing
    gamestate := GS_FINALE
    viewactive := .F.
    automapactive := .F.

    IF LogicalGameMission() == doom
        S_ChangeMusic( mus_victor, .T. )
    ELSE
        S_ChangeMusic( mus_read_m, .T. )
    ENDIF

    FOR i := 0 TO Len( textscreens ) - 1
        screen := textscreens[ i + 1 ]

        IF gameversion == exe_chex .AND. screen:mission == doom
            screen:level := 5
        ENDIF

        IF LogicalGameMission() == screen:mission ;
             .AND. ( LogicalGameMission() != doom .OR. gameepisode == screen:episode ) ;
             .AND. gamemap == screen:level
            finaletext := screen:text
            finaleflat := screen:background
        ENDIF
    NEXT

    finaletext := DEH_String( finaletext )
    finaleflat := DEH_String( finaleflat )

    finalestage := F_STAGE_TEXT
    finalecount := 0
RETURN NIL

FUNCTION F_Responder( event )
    IF finalestage == F_STAGE_CAST
        RETURN F_CastResponder( event )
    ENDIF
RETURN .F.

FUNCTION F_Ticker()
    LOCAL i
    MEMVAR gameaction
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gamemode
    MEMVAR players
    MEMVAR wipegamestate

    IF gamemode == commercial .AND. finalecount > 50
        FOR i := 0 TO MAXPLAYERS - 1
            IF players[ i + 1 ]:cmd:buttons != 0
                EXIT
            ENDIF
        NEXT

        IF i < MAXPLAYERS
            IF gamemap == 30
                F_StartCast()
            ELSE
                gameaction := ga_worlddone
            ENDIF
        ENDIF
    ENDIF

    finalecount := finalecount + 1

    IF finalestage == F_STAGE_CAST
        F_CastTicker()
        RETURN NIL
    ENDIF

    IF gamemode == commercial
        RETURN NIL
    ENDIF

    IF finalestage == F_STAGE_TEXT ;
         .AND. finalecount > Len( finaletext ) * TEXTSPEED + TEXTWAIT
        finalecount := 0
        finalestage := F_STAGE_ARTSCREEN
        wipegamestate := -1
        IF gameepisode == 3
            S_StartMusic( mus_bunny )
        ENDIF
    ENDIF
RETURN NIL

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

STATIC FUNCTION PeekLong( cBuf, nPos )
    IF ValType( cBuf ) != "C" .OR. nPos < 1 .OR. nPos + 3 > Len( cBuf )
        RETURN 0
    ENDIF
RETURN Asc( SubStr( cBuf, nPos, 1 ) ) ;
    + Asc( SubStr( cBuf, nPos + 1, 1 ) ) * 256 ;
    + Asc( SubStr( cBuf, nPos + 2, 1 ) ) * 65536 ;
    + Asc( SubStr( cBuf, nPos + 3, 1 ) ) * 16777216

STATIC FUNCTION PatchWidth( patch )
    IF ValType( patch ) == "O"
        RETURN SHORT( patch:width )
    ENDIF
    IF ValType( patch ) == "C"
        RETURN SHORT( PeekShort( patch, 1 ) )
    ENDIF
RETURN 0

STATIC PROCEDURE VidPoke( nOff1, cByte )
    MEMVAR I_VideoBuffer
    IF ValType( I_VideoBuffer ) == "C" .AND. nOff1 >= 1 .AND. nOff1 <= Len( I_VideoBuffer )
        I_VideoBuffer := Stuff( I_VideoBuffer, nOff1, 1, cByte )
    ENDIF
RETURN

STATIC PROCEDURE F_TileFlat( src )
    LOCAL y
    LOCAL x
    LOCAL nSrcOff
    LOCAL nDestOff
    LOCAL nTail
    LOCAL nRow
    MEMVAR I_VideoBuffer

    IF ValType( I_VideoBuffer ) != "C" .OR. ValType( src ) != "C"
        RETURN
    ENDIF

    nDestOff := 1
    FOR y := 0 TO SCREENHEIGHT - 1
        nSrcOff := ( ( y & 63 ) * 64 ) + 1
        nRow := SubStr( src, nSrcOff, 64 )
        FOR x := 0 TO Int( SCREENWIDTH / 64 ) - 1
            I_VideoBuffer := Stuff( I_VideoBuffer, nDestOff, 64, nRow )
            nDestOff := nDestOff + 64
        NEXT
        nTail := ( SCREENWIDTH & 63 )
        IF nTail != 0
            I_VideoBuffer := Stuff( I_VideoBuffer, nDestOff, nTail, Left( nRow, nTail ) )
            nDestOff := nDestOff + nTail
        ENDIF
    NEXT
RETURN

FUNCTION F_TextWrite()
    LOCAL src
    LOCAL nCount
    LOCAL nPos
    LOCAL c
    LOCAL cx
    LOCAL cy
    LOCAL w
    LOCAL patch
    MEMVAR hu_font

    src := W_CacheLumpName( finaleflat, PU_CACHE )
    F_TileFlat( src )

    V_MarkRect( 0, 0, SCREENWIDTH, SCREENHEIGHT )

    cx := 10
    cy := 10
    nPos := 1
    nCount := Int( ( finalecount - 10 ) / TEXTSPEED )
    IF nCount < 0
        nCount := 0
    ENDIF

    DO WHILE nCount > 0
        nCount := nCount - 1
        IF nPos > Len( finaletext )
            EXIT
        ENDIF
        c := Asc( SubStr( finaletext, nPos, 1 ) )
        nPos := nPos + 1
        IF c == 0
            EXIT
        ENDIF
        IF c == 10
            cx := 10
            cy := cy + 11
            LOOP
        ENDIF

        c := Asc( Upper( Chr( c ) ) ) - HU_FONTSTART
        IF c < 0 .OR. c > HU_FONTSIZE
            cx := cx + 4
            LOOP
        ENDIF

        patch := hu_font[ c + 1 ]
        w := PatchWidth( patch )
        IF cx + w > SCREENWIDTH
            EXIT
        ENDIF
        V_DrawPatch( cx, cy, patch )
        cx := cx + w
    ENDDO
RETURN NIL

FUNCTION F_StartCast()
    MEMVAR wipegamestate
    wipegamestate := -1
    castnum := 0
    caststate := CastInfo():seestate
    casttics := CastState():tics
    castdeath := .F.
    finalestage := F_STAGE_CAST
    castframes := 0
    castonmelee := 0
    castattacking := .F.
    S_ChangeMusic( mus_evil, .T. )
RETURN NIL

FUNCTION F_CastTicker()
    LOCAL st
    LOCAL sfx
    LOCAL oInfo

    casttics := casttics - 1
    IF casttics > 0
        RETURN NIL
    ENDIF

    IF CastState():tics == -1 .OR. CastState():nextstate == S_NULL
        castnum := castnum + 1
        castdeath := .F.
        IF castorder[ castnum + 1 ]:name == NIL
            castnum := 0
        ENDIF
        oInfo := CastInfo()
        IF oInfo:seesound != 0
            S_StartSound( NIL, oInfo:seesound )
        ENDIF
        caststate := oInfo:seestate
        castframes := 0
    ELSE
        IF caststate == S_PLAY_ATK1
            CastStopAttack()
        ELSE
            st := CastState():nextstate
            caststate := st
            castframes := castframes + 1

            SWITCH st
            CASE S_PLAY_ATK1
                sfx := sfx_dshtgn
                EXIT
            CASE S_POSS_ATK2
                sfx := sfx_pistol
                EXIT
            CASE S_SPOS_ATK2
                sfx := sfx_shotgn
                EXIT
            CASE S_VILE_ATK2
                sfx := sfx_vilatk
                EXIT
            CASE S_SKEL_FIST2
                sfx := sfx_skeswg
                EXIT
            CASE S_SKEL_FIST4
                sfx := sfx_skepch
                EXIT
            CASE S_SKEL_MISS2
                sfx := sfx_skeatk
                EXIT
            CASE S_FATT_ATK8
            CASE S_FATT_ATK5
            CASE S_FATT_ATK2
                sfx := sfx_firsht
                EXIT
            CASE S_CPOS_ATK2
            CASE S_CPOS_ATK3
            CASE S_CPOS_ATK4
                sfx := sfx_shotgn
                EXIT
            CASE S_TROO_ATK3
                sfx := sfx_claw
                EXIT
            CASE S_SARG_ATK2
                sfx := sfx_sgtatk
                EXIT
            CASE S_BOSS_ATK2
            CASE S_BOS2_ATK2
            CASE S_HEAD_ATK2
                sfx := sfx_firsht
                EXIT
            CASE S_SKULL_ATK2
                sfx := sfx_sklatk
                EXIT
            CASE S_SPID_ATK2
            CASE S_SPID_ATK3
                sfx := sfx_shotgn
                EXIT
            CASE S_BSPI_ATK2
                sfx := sfx_plasma
                EXIT
            CASE S_CYBER_ATK2
            CASE S_CYBER_ATK4
            CASE S_CYBER_ATK6
                sfx := sfx_rlaunc
                EXIT
            CASE S_PAIN_ATK3
                sfx := sfx_sklatk
                EXIT
            OTHERWISE
                sfx := 0
            ENDSWITCH

            IF sfx != 0
                S_StartSound( NIL, sfx )
            ENDIF
        ENDIF
    ENDIF

    IF castframes == 12
        castattacking := .T.
        oInfo := CastInfo()
        IF castonmelee != 0
            caststate := oInfo:meleestate
        ELSE
            caststate := oInfo:missilestate
        ENDIF
        castonmelee := ( castonmelee ^^ 1 )
        IF caststate == S_NULL
            IF castonmelee != 0
                caststate := oInfo:meleestate
            ELSE
                caststate := oInfo:missilestate
            ENDIF
        ENDIF
    ENDIF

    IF castattacking
        IF castframes == 24 .OR. caststate == CastInfo():seestate
            CastStopAttack()
        ENDIF
    ENDIF

    casttics := CastState():tics
    IF casttics == -1
        casttics := 15
    ENDIF
RETURN NIL

FUNCTION F_CastResponder( ev )
    IF ev:type != ev_keydown
        RETURN .F.
    ENDIF

    IF castdeath
        RETURN .T.
    ENDIF

    castdeath := .T.
    caststate := CastInfo():deathstate
    casttics := CastState():tics
    castframes := 0
    castattacking := .F.
    IF CastInfo():deathsound != 0
        S_StartSound( NIL, CastInfo():deathsound )
    ENDIF
RETURN .T.

FUNCTION F_CastPrint( text )
    LOCAL nPos
    LOCAL c
    LOCAL cx
    LOCAL w
    LOCAL nWidth
    LOCAL patch
    MEMVAR hu_font

    IF text == NIL
        RETURN NIL
    ENDIF

    nWidth := 0
    nPos := 1
    DO WHILE nPos <= Len( text )
        c := Asc( SubStr( text, nPos, 1 ) )
        nPos := nPos + 1
        IF c == 0
            EXIT
        ENDIF
        c := Asc( Upper( Chr( c ) ) ) - HU_FONTSTART
        IF c < 0 .OR. c > HU_FONTSIZE
            nWidth := nWidth + 4
            LOOP
        ENDIF
        nWidth := nWidth + PatchWidth( hu_font[ c + 1 ] )
    ENDDO

    cx := 160 - Int( nWidth / 2 )
    nPos := 1
    DO WHILE nPos <= Len( text )
        c := Asc( SubStr( text, nPos, 1 ) )
        nPos := nPos + 1
        IF c == 0
            EXIT
        ENDIF
        c := Asc( Upper( Chr( c ) ) ) - HU_FONTSTART
        IF c < 0 .OR. c > HU_FONTSIZE
            cx := cx + 4
            LOOP
        ENDIF
        patch := hu_font[ c + 1 ]
        w := PatchWidth( patch )
        V_DrawPatch( cx, 180, patch )
        cx := cx + w
    ENDDO
RETURN NIL

FUNCTION F_CastDrawer()
    LOCAL sprdef
    LOCAL sprframe
    LOCAL lump
    LOCAL flip
    LOCAL patch
    LOCAL nFrame
    MEMVAR firstspritelump
    MEMVAR sprites

    V_DrawPatch( 0, 0, W_CacheLumpName( DEH_String( "BOSSBACK" ), PU_CACHE ) )

    F_CastPrint( DEH_String( castorder[ castnum + 1 ]:name ) )

    sprdef := sprites[ CastState():sprite + 1 ]
    nFrame := ( CastState():frame & FF_FRAMEMASK )
    sprframe := sprdef:spriteframes[ nFrame + 1 ]
    lump := sprframe:lump[ 1 ]
    flip := ( sprframe:flip[ 1 ] != 0 )

    patch := W_CacheLumpNum( lump + firstspritelump, PU_CACHE )
    IF flip
        V_DrawPatchFlipped( 160, 170, patch )
    ELSE
        V_DrawPatch( 160, 170, patch )
    ENDIF
RETURN NIL

FUNCTION F_DrawPatchCol( x, patch, col )
    LOCAL cData
    LOCAL nCol
    LOCAL nDest
    LOCAL nSrc
    LOCAL nTop
    LOCAL nLen
    LOCAL i

    IF ValType( patch ) == "O"
        cData := patch:data
        nCol := LONG( patch:columnofs[ col + 1 ] ) + 1
    ELSEIF ValType( patch ) == "C"
        cData := patch
        nCol := LONG( PeekLong( patch, 8 + col * 4 + 1 ) ) + 1
    ELSE
        RETURN NIL
    ENDIF

    IF ValType( cData ) != "C"
        RETURN NIL
    ENDIF

    DO WHILE nCol <= Len( cData ) .AND. Asc( SubStr( cData, nCol, 1 ) ) != 255
        nTop := Asc( SubStr( cData, nCol, 1 ) )
        nLen := Asc( SubStr( cData, nCol + 1, 1 ) )
        nSrc := nCol + 3
        nDest := x + 1 + nTop * SCREENWIDTH
        FOR i := 1 TO nLen
            VidPoke( nDest, SubStr( cData, nSrc, 1 ) )
            nSrc := nSrc + 1
            nDest := nDest + SCREENWIDTH
        NEXT
        nCol := nCol + nLen + 4
    ENDDO
RETURN NIL

FUNCTION F_BunnyScroll()
    LOCAL scrolled
    LOCAL x
    LOCAL p1
    LOCAL p2
    LOCAL name
    LOCAL stage

    p1 := W_CacheLumpName( DEH_String( "PFUB2" ), PU_LEVEL )
    p2 := W_CacheLumpName( DEH_String( "PFUB1" ), PU_LEVEL )

    V_MarkRect( 0, 0, SCREENWIDTH, SCREENHEIGHT )

    scrolled := ( 320 - Int( ( finalecount - 230 ) / 2 ) )
    IF scrolled > 320
        scrolled := 320
    ENDIF
    IF scrolled < 0
        scrolled := 0
    ENDIF

    FOR x := 0 TO SCREENWIDTH - 1
        IF x + scrolled < 320
            F_DrawPatchCol( x, p1, x + scrolled )
        ELSE
            F_DrawPatchCol( x, p2, x + scrolled - 320 )
        ENDIF
    NEXT

    IF finalecount < 1130
        RETURN NIL
    ENDIF
    IF finalecount < 1180
        V_DrawPatch( Int( ( SCREENWIDTH - 13 * 8 ) / 2 ), ;
            Int( ( SCREENHEIGHT - 8 * 8 ) / 2 ), ;
            W_CacheLumpName( DEH_String( "END0" ), PU_CACHE ) )
        laststage := 0
        RETURN NIL
    ENDIF

    stage := Int( ( finalecount - 1180 ) / 5 )
    IF stage > 6
        stage := 6
    ENDIF
    IF stage > laststage
        S_StartSound( NIL, sfx_pistol )
        laststage := stage
    ENDIF

    name := ""
    DEH_snprintf( @name, 10, "END" + hb_ntos( stage ) )
    V_DrawPatch( Int( ( SCREENWIDTH - 13 * 8 ) / 2 ), ;
        Int( ( SCREENHEIGHT - 8 * 8 ) / 2 ), ;
        W_CacheLumpName( name, PU_CACHE ) )
RETURN NIL

STATIC PROCEDURE F_ArtScreenDrawer()
    LOCAL lumpname
    MEMVAR gameepisode
    MEMVAR gamemode

    IF gameepisode == 3
        F_BunnyScroll()
        RETURN
    ENDIF

    SWITCH gameepisode
    CASE 1
        IF gamemode == retail
            lumpname := "CREDIT"
        ELSE
            lumpname := "HELP2"
        ENDIF
        EXIT
    CASE 2
        lumpname := "VICTORY2"
        EXIT
    CASE 4
        lumpname := "ENDPIC"
        EXIT
    OTHERWISE
        RETURN
    ENDSWITCH

    lumpname := DEH_String( lumpname )
    V_DrawPatch( 0, 0, W_CacheLumpName( lumpname, PU_CACHE ) )
RETURN

FUNCTION F_Drawer()
    SWITCH finalestage
    CASE F_STAGE_CAST
        F_CastDrawer()
        EXIT
    CASE F_STAGE_TEXT
        F_TextWrite()
        EXIT
    CASE F_STAGE_ARTSCREEN
        F_ArtScreenDrawer()
        EXIT
    ENDSWITCH
RETURN NIL
