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

STATIC itemOn := 0
STATIC skullAnimCounter := 0
STATIC whichSkull := 0
STATIC currentMenu := NIL
STATIC messageRoutine := NIL
STATIC saveStringEnter := 0
STATIC saveSlot := 0
STATIC saveCharIndex := 0
STATIC saveOldString := ""
STATIC endstring := ""
STATIC tempstring := ""
STATIC epi := 0
STATIC gammamsg := {}
STATIC skullName := {}
STATIC MainMenu := {}
STATIC EpisodeMenu := {}
STATIC NewGameMenu := {}
STATIC OptionsMenu := {}
STATIC ReadMenu1 := {}
STATIC ReadMenu2 := {}
STATIC SoundMenu := {}
STATIC LoadMenu := {}
STATIC SaveMenu := {}
STATIC MainDef := NIL
STATIC EpiDef := NIL
STATIC NewDef := NIL
STATIC OptionsDef := NIL
STATIC ReadDef1 := NIL
STATIC ReadDef2 := NIL
STATIC SoundDef := NIL
STATIC LoadDef := NIL
STATIC SaveDef := NIL
STATIC detailNames := {}
STATIC msgNames := {}
STATIC quitsounds := {}
STATIC quitsounds2 := {}
STATIC joywait := 0
STATIC mousewait := 0
STATIC mousey := 0
STATIC lasty := 0
STATIC mousex := 0
STATIC lastx := 0
STATIC nDrawX := 0
STATIC nDrawY := 0
STATIC screenSize
STATIC quickSaveSlot
STATIC messageToPrint
STATIC messageString
STATIC messx
STATIC messy
STATIC messageLastMenuActive
STATIC messageNeedsInput
STATIC savegamestrings

#include "doomdef.ch"
#include "doomkeys.ch"
#include "dstrings.ch"
#include "d_mode.ch"
#include "doomstat.ch"
#include "deh_str.ch"
#include "d_event.ch"
#include "hu_stuff.ch"
#include "i_video.ch"
#include "i_swap.ch"
#include "i_input.ch"
#include "m_controls.ch"
#include "m_menu.ch"

CLASS menuitem_t
    DATA status
    DATA name
    DATA routine
    DATA alphaKey
    METHOD New()
ENDCLASS
CLASS menu_t
    DATA numitems
    DATA prevMenu
    DATA menuitems
    DATA routine
    DATA x
    DATA y
    DATA lastOn
    METHOD New()
ENDCLASS

#ifndef PU_CACHE
#define PU_CACHE 8
#endif

#define readthis  4
#define quitdoom  5
#define main_end  6
#define ep1       0
#define ep_end    4
#define hurtme    2
#define menu_nightmare 4
#define newg_end  5
#define menu_messages  1
#define menu_detail    2
#define menu_scrnsize  3
#define menu_mousesens 5
#define opt_end        8
#define read1_end      1
#define read2_end      1
#define sfx_vol        0
#define music_vol      2
#define sound_end      4
#define load_end       6

#ifndef sfx_pistol
#define sfx_pistol  1
#define sfx_pstop   19
#define sfx_stnmov  22
#define sfx_swtchn  23
#define sfx_swtchx  24
#define sfx_dmpain  26
#define sfx_popain  27
#define sfx_slop    31
#define sfx_oof     34
#define sfx_telept  35
#define sfx_posit1  36
#define sfx_posit3  38
#define sfx_sgtatk  52
#define sfx_skeswg  56
#define sfx_pldeth  57
#define sfx_kntdth  72
#define sfx_bspact  78
#define sfx_vilact  80
#define sfx_getpow  93
#define sfx_boscub  95
#endif



METHOD New() CLASS menuitem_t
    ::status   := 0
    ::name     := ""
    ::routine  := NIL
    ::alphaKey := 0
RETURN Self

METHOD New() CLASS menu_t
    ::numitems  := 0
    ::prevMenu  := NIL
    ::menuitems := {}
    ::routine   := NIL
    ::x         := 0
    ::y         := 0
    ::lastOn    := 0
RETURN Self

STATIC FUNCTION IfaceCall( xFun, x1 )
    IF xFun == NIL
        RETURN NIL
    ENDIF
    IF ValType( xFun ) == "B"
        IF PCount() < 2
            RETURN Eval( xFun )
        ENDIF
        RETURN Eval( xFun, x1 )
    ENDIF
    IF ValType( xFun ) == "C"
        IF PCount() < 2
            RETURN hb_ExecFromArray( { xFun } )
        ENDIF
        RETURN hb_ExecFromArray( { xFun, x1 } )
    ENDIF
RETURN NIL

STATIC FUNCTION MkItem( nStatus, cName, cRoutine, nAlpha )
    LOCAL o

    o := menuitem_t():New()
    o:status   := nStatus
    o:name     := cName
    o:routine  := cRoutine
    o:alphaKey := nAlpha
RETURN o

STATIC FUNCTION MkMenu( nItems, oPrev, aItems, cRoutine, nX, nY, nLast )
    LOCAL o

    o := menu_t():New()
    o:numitems  := nItems
    o:prevMenu  := oPrev
    o:menuitems := aItems
    o:routine   := cRoutine
    o:x         := nX
    o:y         := nY
    o:lastOn    := nLast
RETURN o

STATIC FUNCTION CopyItem( oDst, oSrc )
    oDst:status   := oSrc:status
    oDst:name     := oSrc:name
    oDst:routine  := oSrc:routine
    oDst:alphaKey := oSrc:alphaKey
RETURN NIL

INIT PROCEDURE init_m_menu
    LOCAL i

    PUBLIC mouseSensitivity
    PUBLIC showMessages
    PUBLIC detailLevel
    PUBLIC screenblocks
    PUBLIC inhelpscreens
    PUBLIC menuactive

    mouseSensitivity := 5
    showMessages := 1
    detailLevel := 0
    screenblocks := 10
    screenSize := 0
    quickSaveSlot := 0
    messageToPrint := 0
    messageString := NIL
    messx := 0
    messy := 0
    messageLastMenuActive := 0
    messageNeedsInput := .F.
    inhelpscreens := .F.
    menuactive := 0
    saveStringEnter := 0
    saveSlot := 0
    saveCharIndex := 0
    saveOldString := ""
    endstring := ""
    tempstring := ""
    epi := 0
    itemOn := 0
    skullAnimCounter := 0
    whichSkull := 0
    messageRoutine := NIL
    joywait := 0
    mousewait := 0
    mousey := 0
    lasty := 0
    mousex := 0
    lastx := 0

    gammamsg := { GAMMALVL0, GAMMALVL1, GAMMALVL2, GAMMALVL3, GAMMALVL4 }
    skullName := { "M_SKULL1", "M_SKULL2" }
    detailNames := { "M_GDHIGH", "M_GDLOW" }
    msgNames := { "M_MSGOFF", "M_MSGON" }

    savegamestrings := {}
    FOR i := 1 TO 10
        AAdd( savegamestrings, "" )
    NEXT

    MainMenu := {}
    AAdd( MainMenu, MkItem( 1, "M_NGAME",  "M_NewGame",    110 ) )
    AAdd( MainMenu, MkItem( 1, "M_OPTION", "M_Options",    111 ) )
    AAdd( MainMenu, MkItem( 1, "M_LOADG",  "M_LoadGame",   108 ) )
    AAdd( MainMenu, MkItem( 1, "M_SAVEG",  "M_SaveGame",   115 ) )
    AAdd( MainMenu, MkItem( 1, "M_RDTHIS", "M_ReadThis",   114 ) )
    AAdd( MainMenu, MkItem( 1, "M_QUITG",  "M_QuitDOOM",   113 ) )

    EpisodeMenu := {}
    AAdd( EpisodeMenu, MkItem( 1, "M_EPI1", "M_Episode", 107 ) )
    AAdd( EpisodeMenu, MkItem( 1, "M_EPI2", "M_Episode", 116 ) )
    AAdd( EpisodeMenu, MkItem( 1, "M_EPI3", "M_Episode", 105 ) )
    AAdd( EpisodeMenu, MkItem( 1, "M_EPI4", "M_Episode", 116 ) )

    NewGameMenu := {}
    AAdd( NewGameMenu, MkItem( 1, "M_JKILL", "M_ChooseSkill", 105 ) )
    AAdd( NewGameMenu, MkItem( 1, "M_ROUGH", "M_ChooseSkill", 104 ) )
    AAdd( NewGameMenu, MkItem( 1, "M_HURT",  "M_ChooseSkill", 104 ) )
    AAdd( NewGameMenu, MkItem( 1, "M_ULTRA", "M_ChooseSkill", 117 ) )
    AAdd( NewGameMenu, MkItem( 1, "M_NMARE", "M_ChooseSkill", 110 ) )

    OptionsMenu := {}
    AAdd( OptionsMenu, MkItem( 1, "M_ENDGAM", "M_EndGame",            101 ) )
    AAdd( OptionsMenu, MkItem( 1, "M_MESSG",  "M_ChangeMessages",     109 ) )
    AAdd( OptionsMenu, MkItem( 1, "M_DETAIL", "M_ChangeDetail",       103 ) )
    AAdd( OptionsMenu, MkItem( 2, "M_SCRNSZ", "M_SizeDisplay",        115 ) )
    AAdd( OptionsMenu, MkItem( -1, "",        NIL,                      0 ) )
    AAdd( OptionsMenu, MkItem( 2, "M_MSENS",  "M_ChangeSensitivity",  109 ) )
    AAdd( OptionsMenu, MkItem( -1, "",        NIL,                      0 ) )
    AAdd( OptionsMenu, MkItem( 1, "M_SVOL",   "M_Sound",              115 ) )

    ReadMenu1 := {}
    AAdd( ReadMenu1, MkItem( 1, "", "M_ReadThis2", 0 ) )
    ReadMenu2 := {}
    AAdd( ReadMenu2, MkItem( 1, "", "M_FinishReadThis", 0 ) )

    SoundMenu := {}
    AAdd( SoundMenu, MkItem( 2, "M_SFXVOL", "M_SfxVol",   115 ) )
    AAdd( SoundMenu, MkItem( -1, "",       NIL,             0 ) )
    AAdd( SoundMenu, MkItem( 2, "M_MUSVOL", "M_MusicVol", 109 ) )
    AAdd( SoundMenu, MkItem( -1, "",       NIL,             0 ) )

    LoadMenu := {}
    SaveMenu := {}
    AAdd( LoadMenu, MkItem( 1, "", "M_LoadSelect", 49 ) )
    AAdd( LoadMenu, MkItem( 1, "", "M_LoadSelect", 50 ) )
    AAdd( LoadMenu, MkItem( 1, "", "M_LoadSelect", 51 ) )
    AAdd( LoadMenu, MkItem( 1, "", "M_LoadSelect", 52 ) )
    AAdd( LoadMenu, MkItem( 1, "", "M_LoadSelect", 53 ) )
    AAdd( LoadMenu, MkItem( 1, "", "M_LoadSelect", 54 ) )
    AAdd( SaveMenu, MkItem( 1, "", "M_SaveSelect", 49 ) )
    AAdd( SaveMenu, MkItem( 1, "", "M_SaveSelect", 50 ) )
    AAdd( SaveMenu, MkItem( 1, "", "M_SaveSelect", 51 ) )
    AAdd( SaveMenu, MkItem( 1, "", "M_SaveSelect", 52 ) )
    AAdd( SaveMenu, MkItem( 1, "", "M_SaveSelect", 53 ) )
    AAdd( SaveMenu, MkItem( 1, "", "M_SaveSelect", 54 ) )

    MainDef    := MkMenu( main_end, NIL,     MainMenu,    "M_DrawMainMenu",  97, 64, 0 )
    EpiDef     := MkMenu( ep_end,   NIL,     EpisodeMenu, "M_DrawEpisode",   48, 63, ep1 )
    NewDef     := MkMenu( newg_end, NIL,     NewGameMenu, "M_DrawNewGame",   48, 63, hurtme )
    OptionsDef := MkMenu( opt_end,  NIL,     OptionsMenu, "M_DrawOptions",   60, 37, 0 )
    ReadDef1   := MkMenu( read1_end, NIL,    ReadMenu1,   "M_DrawReadThis1", 280, 185, 0 )
    ReadDef2   := MkMenu( read2_end, NIL,    ReadMenu2,   "M_DrawReadThis2", 330, 175, 0 )
    SoundDef   := MkMenu( sound_end, NIL,    SoundMenu,   "M_DrawSound",     80, 64, 0 )
    LoadDef    := MkMenu( load_end, NIL,     LoadMenu,    "M_DrawLoad",      80, 54, 0 )
    SaveDef    := MkMenu( load_end, NIL,     SaveMenu,    "M_DrawSave",      80, 54, 0 )

    EpiDef:prevMenu     := MainDef
    NewDef:prevMenu     := EpiDef
    OptionsDef:prevMenu := MainDef
    ReadDef1:prevMenu   := MainDef
    ReadDef2:prevMenu   := ReadDef1
    SoundDef:prevMenu   := OptionsDef
    LoadDef:prevMenu    := MainDef
    SaveDef:prevMenu    := MainDef

    currentMenu := MainDef

    quitsounds := { sfx_pldeth, sfx_dmpain, sfx_popain, sfx_slop, sfx_telept, sfx_posit1, sfx_posit3, sfx_sgtatk }
    quitsounds2 := { sfx_vilact, sfx_getpow, sfx_boscub, sfx_slop, sfx_skeswg, sfx_kntdth, sfx_bspact, sfx_sgtatk }
RETURN

FUNCTION M_ReadSaveStrings()
    LOCAL i
    LOCAL cName
    LOCAL cData
    LOCAL nZ

    FOR i := 0 TO load_end - 1
        cName := P_SaveGameFile( i )
        IF ! hb_FileExists( cName )
            savegamestrings[ i + 1 ] := EMPTYSTRING
            LoadMenu[ i + 1 ]:status := 0
            LOOP
        ENDIF
        cData := Left( hb_MemoRead( cName ), SAVESTRINGSIZE )
        /* o cabecalho completa o nome com Chr( 0 ); em C a string termina no primeiro zero */
        nZ := At( Chr( 0 ), cData )
        IF nZ > 0
            cData := Left( cData, nZ - 1 )
        ENDIF
        savegamestrings[ i + 1 ] := cData
        LoadMenu[ i + 1 ]:status := 1
    NEXT
RETURN NIL

FUNCTION M_DrawLoad()
    LOCAL i

    V_DrawPatchDirect( 72, 28, W_CacheLumpName( DEH_String( "M_LOADG" ), PU_CACHE ) )
    FOR i := 0 TO load_end - 1
        M_DrawSaveLoadBorder( LoadDef:x, LoadDef:y + LINEHEIGHT * i )
        M_WriteText( LoadDef:x, LoadDef:y + LINEHEIGHT * i, savegamestrings[ i + 1 ] )
    NEXT
RETURN NIL

FUNCTION M_DrawSaveLoadBorder( x, y )
    LOCAL i

    V_DrawPatchDirect( x - 8, y + 7, W_CacheLumpName( DEH_String( "M_LSLEFT" ), PU_CACHE ) )
    FOR i := 0 TO 23
        V_DrawPatchDirect( x, y + 7, W_CacheLumpName( DEH_String( "M_LSCNTR" ), PU_CACHE ) )
        x := x + 8
    NEXT
    V_DrawPatchDirect( x, y + 7, W_CacheLumpName( DEH_String( "M_LSRGHT" ), PU_CACHE ) )
RETURN NIL

FUNCTION M_LoadSelect( choice )
    LOCAL cName

    cName := P_SaveGameFile( choice )
    G_LoadGame( cName )
    M_ClearMenus()
RETURN NIL

FUNCTION M_LoadGame( choice )
    MEMVAR netgame
    HB_SYMBOL_UNUSED( choice )
    IF netgame
        M_StartMessage( DEH_String( LOADNET ), NIL, .F. )
        RETURN NIL
    ENDIF
    M_SetupNextMenu( LoadDef )
    M_ReadSaveStrings()
RETURN NIL

FUNCTION M_DrawSave()
    LOCAL i
    LOCAL nW

    V_DrawPatchDirect( 72, 28, W_CacheLumpName( DEH_String( "M_SAVEG" ), PU_CACHE ) )
    FOR i := 0 TO load_end - 1
        M_DrawSaveLoadBorder( LoadDef:x, LoadDef:y + LINEHEIGHT * i )
        M_WriteText( LoadDef:x, LoadDef:y + LINEHEIGHT * i, savegamestrings[ i + 1 ] )
    NEXT
    IF saveStringEnter != 0
        nW := M_StringWidth( savegamestrings[ saveSlot + 1 ] )
        M_WriteText( LoadDef:x + nW, LoadDef:y + LINEHEIGHT * saveSlot, "_" )
    ENDIF
RETURN NIL

FUNCTION M_DoSave( slot )
    G_SaveGame( slot, savegamestrings[ slot + 1 ] )
    M_ClearMenus()
    IF quickSaveSlot == -2
        quickSaveSlot := slot
    ENDIF
RETURN NIL

FUNCTION M_SaveSelect( choice )
    saveStringEnter := 1
    saveSlot := choice
    saveOldString := Left( savegamestrings[ choice + 1 ], SAVESTRINGSIZE )
    IF savegamestrings[ choice + 1 ] == EMPTYSTRING
        savegamestrings[ choice + 1 ] := ""
    ENDIF
    saveCharIndex := Len( savegamestrings[ choice + 1 ] )
RETURN NIL

FUNCTION M_SaveGame( choice )
    MEMVAR gamestate
    MEMVAR usergame
    HB_SYMBOL_UNUSED( choice )
    IF ! usergame
        M_StartMessage( DEH_String( SAVEDEAD ), NIL, .F. )
        RETURN NIL
    ENDIF
    IF gamestate != GS_LEVEL
        RETURN NIL
    ENDIF
    M_SetupNextMenu( SaveDef )
    M_ReadSaveStrings()
RETURN NIL

FUNCTION M_QuickSaveResponse( key )
    MEMVAR key_menu_confirm
    IF key == key_menu_confirm
        M_DoSave( quickSaveSlot )
        S_StartSound( NIL, sfx_swtchx )
    ENDIF
RETURN NIL

FUNCTION M_QuickSave()
    MEMVAR gamestate
    MEMVAR usergame
    IF ! usergame
        S_StartSound( NIL, sfx_oof )
        RETURN NIL
    ENDIF
    IF gamestate != GS_LEVEL
        RETURN NIL
    ENDIF
    IF quickSaveSlot < 0
        M_StartControlPanel()
        M_ReadSaveStrings()
        M_SetupNextMenu( SaveDef )
        quickSaveSlot := -2
        RETURN NIL
    ENDIF
    tempstring := StrTran( DEH_String( QSPROMPT ), "%s", savegamestrings[ quickSaveSlot + 1 ] )
    M_StartMessage( tempstring, "M_QuickSaveResponse", .T. )
RETURN NIL

FUNCTION M_QuickLoadResponse( key )
    MEMVAR key_menu_confirm
    IF key == key_menu_confirm
        M_LoadSelect( quickSaveSlot )
        S_StartSound( NIL, sfx_swtchx )
    ENDIF
RETURN NIL

FUNCTION M_QuickLoad()
    MEMVAR netgame
    IF netgame
        M_StartMessage( DEH_String( QLOADNET ), NIL, .F. )
        RETURN NIL
    ENDIF
    IF quickSaveSlot < 0
        M_StartMessage( DEH_String( QSAVESPOT ), NIL, .F. )
        RETURN NIL
    ENDIF
    tempstring := StrTran( DEH_String( QLPROMPT ), "%s", savegamestrings[ quickSaveSlot + 1 ] )
    M_StartMessage( tempstring, "M_QuickLoadResponse", .T. )
RETURN NIL

FUNCTION M_DrawReadThis1()
    LOCAL lumpname := "CREDIT"
    LOCAL skullx := 330
    LOCAL skully := 175
    MEMVAR gamemode
    MEMVAR gameversion
    MEMVAR inhelpscreens

    inhelpscreens := .T.

    IF gameversion == exe_doom_1_666 .OR. gameversion == exe_doom_1_7 ;
         .OR. gameversion == exe_doom_1_8 .OR. gameversion == exe_doom_1_9 ;
         .OR. gameversion == exe_hacx
        IF gamemode == commercial
            lumpname := "HELP"
            skullx := 330
            skully := 165
        ELSE
            lumpname := "HELP2"
            skullx := 280
            skully := 185
        ENDIF
    ELSEIF gameversion == exe_ultimate .OR. gameversion == exe_chex
        lumpname := "HELP1"
    ELSEIF gameversion == exe_final .OR. gameversion == exe_final2
        lumpname := "HELP"
    ELSE
        I_Error( "Unhandled game version" )
    ENDIF

    lumpname := DEH_String( lumpname )
    V_DrawPatchDirect( 0, 0, W_CacheLumpName( lumpname, PU_CACHE ) )
    ReadDef1:x := skullx
    ReadDef1:y := skully
RETURN NIL

FUNCTION M_DrawReadThis2()
    MEMVAR inhelpscreens
    inhelpscreens := .T.
    V_DrawPatchDirect( 0, 0, W_CacheLumpName( DEH_String( "HELP1" ), PU_CACHE ) )
RETURN NIL

FUNCTION M_DrawSound()
    MEMVAR musicVolume
    MEMVAR sfxVolume
    V_DrawPatchDirect( 60, 38, W_CacheLumpName( DEH_String( "M_SVOL" ), PU_CACHE ) )
    M_DrawThermo( SoundDef:x, SoundDef:y + LINEHEIGHT * ( sfx_vol + 1 ), 16, sfxVolume )
    M_DrawThermo( SoundDef:x, SoundDef:y + LINEHEIGHT * ( music_vol + 1 ), 16, musicVolume )
RETURN NIL

FUNCTION M_Sound( choice )
    HB_SYMBOL_UNUSED( choice )
    M_SetupNextMenu( SoundDef )
RETURN NIL

FUNCTION M_SfxVol( choice )
    MEMVAR sfxVolume
    SWITCH choice
    CASE 0
        IF sfxVolume != 0
            sfxVolume := sfxVolume - 1
        ENDIF
        EXIT
    CASE 1
        IF sfxVolume < 15
            sfxVolume := sfxVolume + 1
        ENDIF
        EXIT
    ENDSWITCH
    S_SetSfxVolume( sfxVolume * 8 )
RETURN NIL

FUNCTION M_MusicVol( choice )
    MEMVAR musicVolume
    SWITCH choice
    CASE 0
        IF musicVolume != 0
            musicVolume := musicVolume - 1
        ENDIF
        EXIT
    CASE 1
        IF musicVolume < 15
            musicVolume := musicVolume + 1
        ENDIF
        EXIT
    ENDSWITCH
    S_SetMusicVolume( musicVolume * 8 )
RETURN NIL

FUNCTION M_DrawMainMenu()
    V_DrawPatchDirect( 94, 2, W_CacheLumpName( DEH_String( "M_DOOM" ), PU_CACHE ) )
RETURN NIL

FUNCTION M_DrawNewGame()
    V_DrawPatchDirect( 96, 14, W_CacheLumpName( DEH_String( "M_NEWG" ), PU_CACHE ) )
    V_DrawPatchDirect( 54, 38, W_CacheLumpName( DEH_String( "M_SKILL" ), PU_CACHE ) )
RETURN NIL

FUNCTION M_NewGame( choice )
    MEMVAR demoplayback
    MEMVAR gamemode
    MEMVAR gameversion
    MEMVAR netgame
    HB_SYMBOL_UNUSED( choice )
    IF netgame .AND. ! demoplayback
        M_StartMessage( DEH_String( NEWGAME ), NIL, .F. )
        RETURN NIL
    ENDIF
    IF gamemode == commercial .OR. gameversion == exe_chex
        M_SetupNextMenu( NewDef )
    ELSE
        M_SetupNextMenu( EpiDef )
    ENDIF
RETURN NIL

FUNCTION M_DrawEpisode()
    V_DrawPatchDirect( 54, 38, W_CacheLumpName( DEH_String( "M_EPISOD" ), PU_CACHE ) )
RETURN NIL

FUNCTION M_VerifyNightmare( key )
    MEMVAR key_menu_confirm
    IF key != key_menu_confirm
        RETURN NIL
    ENDIF
    G_DeferedInitNew( menu_nightmare, epi + 1, 1 )
    M_ClearMenus()
RETURN NIL

FUNCTION M_ChooseSkill( choice )
    IF choice == menu_nightmare
        M_StartMessage( DEH_String( NIGHTMARE ), "M_VerifyNightmare", .T. )
        RETURN NIL
    ENDIF
    G_DeferedInitNew( choice, epi + 1, 1 )
    M_ClearMenus()
RETURN NIL

FUNCTION M_Episode( choice )
    MEMVAR gamemode
    IF gamemode == shareware .AND. choice != 0
        M_StartMessage( DEH_String( SWSTRING ), NIL, .F. )
        M_SetupNextMenu( ReadDef1 )
        RETURN NIL
    ENDIF
    IF gamemode == registered .AND. choice > 2
        OutErr( "M_Episode: 4th episode requires UltimateDOOM" + hb_eol() )
        choice := 0
    ENDIF
    epi := choice
    M_SetupNextMenu( NewDef )
RETURN NIL

FUNCTION M_DrawOptions()
    MEMVAR detailLevel
    MEMVAR mouseSensitivity
    MEMVAR showMessages
    V_DrawPatchDirect( 108, 15, W_CacheLumpName( DEH_String( "M_OPTTTL" ), PU_CACHE ) )
    V_DrawPatchDirect( OptionsDef:x + 175, OptionsDef:y + LINEHEIGHT * menu_detail, ;
        W_CacheLumpName( DEH_String( detailNames[ detailLevel + 1 ] ), PU_CACHE ) )
    V_DrawPatchDirect( OptionsDef:x + 120, OptionsDef:y + LINEHEIGHT * menu_messages, ;
        W_CacheLumpName( DEH_String( msgNames[ showMessages + 1 ] ), PU_CACHE ) )
    M_DrawThermo( OptionsDef:x, OptionsDef:y + LINEHEIGHT * ( menu_mousesens + 1 ), 10, mouseSensitivity )
    M_DrawThermo( OptionsDef:x, OptionsDef:y + LINEHEIGHT * ( menu_scrnsize + 1 ), 9, screenSize )
RETURN NIL

FUNCTION M_Options( choice )
    HB_SYMBOL_UNUSED( choice )
    M_SetupNextMenu( OptionsDef )
RETURN NIL

FUNCTION M_ChangeMessages( choice )
    MEMVAR consoleplayer
    MEMVAR message_dontfuckwithme
    MEMVAR players
    MEMVAR showMessages
    choice := 0
    HB_SYMBOL_UNUSED( choice )
    showMessages := 1 - showMessages
    IF showMessages == 0
        players[ consoleplayer + 1 ]:message := DEH_String( MSGOFF )
    ELSE
        players[ consoleplayer + 1 ]:message := DEH_String( MSGON )
    ENDIF
    message_dontfuckwithme := .T.
RETURN NIL

FUNCTION M_EndGameResponse( key )
    MEMVAR key_menu_confirm
    IF key != key_menu_confirm
        RETURN NIL
    ENDIF
    currentMenu:lastOn := itemOn
    M_ClearMenus()
    D_StartTitle()
RETURN NIL

FUNCTION M_EndGame( choice )
    MEMVAR netgame
    MEMVAR usergame
    choice := 0
    HB_SYMBOL_UNUSED( choice )
    IF ! usergame
        S_StartSound( NIL, sfx_oof )
        RETURN NIL
    ENDIF
    IF netgame
        M_StartMessage( DEH_String( NETEND ), NIL, .F. )
        RETURN NIL
    ENDIF
    M_StartMessage( DEH_String( ENDGAME ), "M_EndGameResponse", .T. )
RETURN NIL

FUNCTION M_ReadThis( choice )
    choice := 0
    HB_SYMBOL_UNUSED( choice )
    M_SetupNextMenu( ReadDef1 )
RETURN NIL

FUNCTION M_ReadThis2( choice )
    MEMVAR gamemode
    MEMVAR gameversion
    IF gameversion <= exe_doom_1_9 .AND. gamemode != commercial
        choice := 0
        HB_SYMBOL_UNUSED( choice )
        M_SetupNextMenu( ReadDef2 )
    ELSE
        M_FinishReadThis( 0 )
    ENDIF
RETURN NIL

FUNCTION M_FinishReadThis( choice )
    choice := 0
    HB_SYMBOL_UNUSED( choice )
    M_SetupNextMenu( MainDef )
RETURN NIL

FUNCTION M_QuitResponse( key )
    LOCAL nIdx
    MEMVAR gamemode
    MEMVAR gametic
    MEMVAR key_menu_confirm
    MEMVAR netgame

    IF key != key_menu_confirm
        RETURN NIL
    ENDIF
    IF ! netgame
        nIdx := ( ( Int( gametic / 4 ) & 7 ) ) + 1
        IF gamemode == commercial
            S_StartSound( NIL, quitsounds2[ nIdx ] )
        ELSE
            S_StartSound( NIL, quitsounds[ nIdx ] )
        ENDIF
        I_WaitVBL( 105 )
    ENDIF
    I_Quit()
RETURN NIL

STATIC FUNCTION M_SelectEndMessage()
    LOCAL aMsg
    MEMVAR doom1_endmsg
    MEMVAR doom2_endmsg
    MEMVAR gametic

    IF logical_gamemission == doom
        aMsg := doom1_endmsg
    ELSE
        aMsg := doom2_endmsg
    ENDIF
RETURN aMsg[ ( gametic % NUM_QUITMESSAGES ) + 1 ]

FUNCTION M_QuitDOOM( choice )
    HB_SYMBOL_UNUSED( choice )
    endstring := DEH_String( M_SelectEndMessage() ) + e"\n\n" + DOSY
    M_StartMessage( endstring, "M_QuitResponse", .T. )
RETURN NIL

FUNCTION M_ChangeSensitivity( choice )
    MEMVAR mouseSensitivity
    SWITCH choice
    CASE 0
        IF mouseSensitivity != 0
            mouseSensitivity := mouseSensitivity - 1
        ENDIF
        EXIT
    CASE 1
        IF mouseSensitivity < 9
            mouseSensitivity := mouseSensitivity + 1
        ENDIF
        EXIT
    ENDSWITCH
RETURN NIL

FUNCTION M_ChangeDetail( choice )
    MEMVAR consoleplayer
    MEMVAR detailLevel
    MEMVAR players
    MEMVAR screenblocks
    choice := 0
    HB_SYMBOL_UNUSED( choice )
    detailLevel := 1 - detailLevel
    R_SetViewSize( screenblocks, detailLevel )
    IF detailLevel == 0
        players[ consoleplayer + 1 ]:message := DEH_String( DETAILHI )
    ELSE
        players[ consoleplayer + 1 ]:message := DEH_String( DETAILLO )
    ENDIF
RETURN NIL

FUNCTION M_SizeDisplay( choice )
    MEMVAR detailLevel
    MEMVAR screenblocks
    SWITCH choice
    CASE 0
        IF screenSize > 0
            screenblocks := screenblocks - 1
            screenSize := screenSize - 1
        ENDIF
        EXIT
    CASE 1
        IF screenSize < 8
            screenblocks := screenblocks + 1
            screenSize := screenSize + 1
        ENDIF
        EXIT
    ENDSWITCH
    R_SetViewSize( screenblocks, detailLevel )
RETURN NIL

FUNCTION M_DrawThermo( x, y, thermWidth, thermDot )
    LOCAL xx
    LOCAL i

    xx := x
    V_DrawPatchDirect( xx, y, W_CacheLumpName( DEH_String( "M_THERML" ), PU_CACHE ) )
    xx := xx + 8
    FOR i := 0 TO thermWidth - 1
        V_DrawPatchDirect( xx, y, W_CacheLumpName( DEH_String( "M_THERMM" ), PU_CACHE ) )
        xx := xx + 8
    NEXT
    V_DrawPatchDirect( xx, y, W_CacheLumpName( DEH_String( "M_THERMR" ), PU_CACHE ) )
    V_DrawPatchDirect( ( x + 8 ) + thermDot * 8, y, W_CacheLumpName( DEH_String( "M_THERMO" ), PU_CACHE ) )
RETURN NIL

FUNCTION M_DrawEmptyCell( menu, item )
    V_DrawPatchDirect( menu:x - 10, menu:y + item * LINEHEIGHT - 1, ;
        W_CacheLumpName( DEH_String( "M_CELL1" ), PU_CACHE ) )
RETURN NIL

FUNCTION M_DrawSelCell( menu, item )
    V_DrawPatchDirect( menu:x - 10, menu:y + item * LINEHEIGHT - 1, ;
        W_CacheLumpName( DEH_String( "M_CELL2" ), PU_CACHE ) )
RETURN NIL

FUNCTION M_StartMessage( string, routine, input )
    MEMVAR menuactive
    messageLastMenuActive := menuactive
    messageToPrint := 1
    messageString := string
    messageRoutine := routine
    messageNeedsInput := input
    menuactive := 1
RETURN NIL

FUNCTION M_StopMessage()
    MEMVAR menuactive
    menuactive := messageLastMenuActive
    messageToPrint := 0
RETURN NIL

STATIC FUNCTION MenuPatchWord( patch, nOff )
    LOCAL n
    IF ValType( patch ) != "C" .OR. nOff + 1 > Len( patch )
        RETURN 0
    ENDIF
    n := Asc( SubStr( patch, nOff, 1 ) ) + Asc( SubStr( patch, nOff + 1, 1 ) ) * 256
RETURN iif( n >= 32768, n - 65536, n )

STATIC FUNCTION MenuPatchW( patch )
    IF ValType( patch ) == "O"
        RETURN SHORT( patch:width )
    ENDIF
    IF ValType( patch ) == "C"
        RETURN MenuPatchWord( patch, 1 )
    ENDIF
RETURN 0

STATIC FUNCTION MenuPatchH( patch )
    IF ValType( patch ) == "O"
        RETURN SHORT( patch:height )
    ENDIF
    IF ValType( patch ) == "C"
        RETURN MenuPatchWord( patch, 3 )
    ENDIF
RETURN 0

FUNCTION M_StringWidth( string )
    LOCAL i
    LOCAL w := 0
    LOCAL c
    LOCAL oFont
    MEMVAR hu_font

    FOR i := 0 TO Len( string ) - 1
        c := Asc( Upper( SubStr( string, i + 1, 1 ) ) ) - HU_FONTSTART
        IF c < 0 .OR. c >= HU_FONTSIZE
            w := w + 4
        ELSE
            oFont := hu_font[ c + 1 ]
            w := w + MenuPatchW( oFont )
        ENDIF
    NEXT
RETURN w

FUNCTION M_StringHeight( string )
    LOCAL i
    LOCAL h
    LOCAL height
    MEMVAR hu_font

    height := MenuPatchH( hu_font[ 1 ] )
    h := height
    FOR i := 0 TO Len( string ) - 1
        IF SubStr( string, i + 1, 1 ) == Chr( 10 )
            h := h + height
        ENDIF
    NEXT
RETURN h

FUNCTION M_WriteText( x, y, string )
    LOCAL w
    LOCAL nPos
    LOCAL c
    LOCAL cx
    LOCAL cy
    LOCAL oFont
    MEMVAR hu_font

    nPos := 1
    cx := x
    cy := y
    DO WHILE .T.
        IF nPos > Len( string )
            EXIT
        ENDIF
        c := Asc( SubStr( string, nPos, 1 ) )
        nPos := nPos + 1
        IF c == 0
            EXIT
        ENDIF
        IF c == 10
            cx := x
            cy := cy + 12
            LOOP
        ENDIF
        c := Asc( Upper( Chr( c ) ) ) - HU_FONTSTART
        IF c < 0 .OR. c >= HU_FONTSIZE
            cx := cx + 4
            LOOP
        ENDIF
        oFont := hu_font[ c + 1 ]
        w := MenuPatchW( oFont )
        IF cx + w > SCREENWIDTH
            EXIT
        ENDIF
        V_DrawPatchDirect( cx, cy, oFont )
        cx := cx + w
    ENDDO
RETURN NIL

STATIC FUNCTION IsNullKey( key )
RETURN key == KEY_PAUSE .OR. key == KEY_CAPSLOCK .OR. key == KEY_SCRLCK .OR. key == KEY_NUMLOCK

STATIC FUNCTION MenuItem( oMenu, nIdx )
RETURN oMenu:menuitems[ nIdx + 1 ]

STATIC FUNCTION BitShl( n, nBits )
    LOCAL i

    n := Int( n )
    FOR i := 1 TO nBits
        n := n * 2
    NEXT
RETURN n

FUNCTION M_Responder( ev )
    LOCAL ch
    LOCAL key
    LOCAL i
    LOCAL oItem
    MEMVAR automapactive
    MEMVAR chat_on
    MEMVAR consoleplayer
    MEMVAR devparm
    MEMVAR gamemode
    MEMVAR joybmenu
    MEMVAR key_menu_abort
    MEMVAR key_menu_activate
    MEMVAR key_menu_back
    MEMVAR key_menu_confirm
    MEMVAR key_menu_decscreen
    MEMVAR key_menu_detail
    MEMVAR key_menu_down
    MEMVAR key_menu_endgame
    MEMVAR key_menu_forward
    MEMVAR key_menu_gamma
    MEMVAR key_menu_help
    MEMVAR key_menu_incscreen
    MEMVAR key_menu_left
    MEMVAR key_menu_load
    MEMVAR key_menu_messages
    MEMVAR key_menu_qload
    MEMVAR key_menu_qsave
    MEMVAR key_menu_quit
    MEMVAR key_menu_right
    MEMVAR key_menu_save
    MEMVAR key_menu_screenshot
    MEMVAR key_menu_up
    MEMVAR key_menu_volume
    MEMVAR menuactive
    MEMVAR players
    MEMVAR testcontrols
    MEMVAR usegamma
    MEMVAR vanilla_keyboard_mapping

    IF testcontrols
        IF ev:type == ev_quit ;
             .OR. ( ev:type == ev_keydown .AND. ( ev:data1 == key_menu_activate .OR. ev:data1 == key_menu_quit ) )
            I_Quit()
            RETURN .T.
        ENDIF
        RETURN .F.
    ENDIF

    IF ev:type == ev_quit
        IF menuactive != 0 .AND. messageToPrint != 0 .AND. messageRoutine == "M_QuitResponse"
            M_QuitResponse( key_menu_confirm )
        ELSE
            S_StartSound( NIL, sfx_swtchn )
            M_QuitDOOM( 0 )
        ENDIF
        RETURN .T.
    ENDIF

    ch := 0
    key := -1

    IF ev:type == ev_joystick .AND. joywait < I_GetTime()
        IF ev:data3 < 0
            key := key_menu_up
            joywait := I_GetTime() + 5
        ELSEIF ev:data3 > 0
            key := key_menu_down
            joywait := I_GetTime() + 5
        ENDIF
        IF ev:data2 < 0
            key := key_menu_left
            joywait := I_GetTime() + 2
        ELSEIF ev:data2 > 0
            key := key_menu_right
            joywait := I_GetTime() + 2
        ENDIF
        IF ( ev:data1 & 1 ) != 0
            key := key_menu_forward
            joywait := I_GetTime() + 5
        ENDIF
        IF ( ev:data1 & 2 ) != 0
            key := key_menu_back
            joywait := I_GetTime() + 5
        ENDIF
        IF joybmenu >= 0 .AND. ( ev:data1 & BitShl( 1, joybmenu ) ) != 0
            key := key_menu_activate
            joywait := I_GetTime() + 5
        ENDIF
    ELSEIF ev:type == ev_mouse .AND. mousewait < I_GetTime()
        mousey := mousey + ev:data3
        IF mousey < lasty - 30
            key := key_menu_down
            mousewait := I_GetTime() + 5
            lasty := lasty - 30
            mousey := lasty
        ELSEIF mousey > lasty + 30
            key := key_menu_up
            mousewait := I_GetTime() + 5
            lasty := lasty + 30
            mousey := lasty
        ENDIF
        mousex := mousex + ev:data2
        IF mousex < lastx - 30
            key := key_menu_left
            mousewait := I_GetTime() + 5
            lastx := lastx - 30
            mousex := lastx
        ELSEIF mousex > lastx + 30
            key := key_menu_right
            mousewait := I_GetTime() + 5
            lastx := lastx + 30
            mousex := lastx
        ENDIF
        IF ( ev:data1 & 1 ) != 0
            key := key_menu_forward
            mousewait := I_GetTime() + 15
        ENDIF
        IF ( ev:data1 & 2 ) != 0
            key := key_menu_back
            mousewait := I_GetTime() + 15
        ENDIF
    ELSEIF ev:type == ev_keydown
        key := ev:data1
        ch := ev:data2
    ENDIF

    IF key == -1
        RETURN .F.
    ENDIF

    IF saveStringEnter != 0
        key := Int( key )
        ch := iif( ValType( ch ) == "N", Int( ch ), 0 )
        IF ValType( saveCharIndex ) != "N"
            saveCharIndex := Len( savegamestrings[ saveSlot + 1 ] )
        ENDIF
        IF key == KEY_BACKSPACE
            IF saveCharIndex > 0
                saveCharIndex := saveCharIndex - 1
                savegamestrings[ saveSlot + 1 ] := Left( savegamestrings[ saveSlot + 1 ], saveCharIndex )
            ENDIF
        ELSEIF key == KEY_ESCAPE
            saveStringEnter := 0
            savegamestrings[ saveSlot + 1 ] := Left( saveOldString, SAVESTRINGSIZE )
        ELSEIF key == KEY_ENTER
            saveStringEnter := 0
            IF ! Empty( savegamestrings[ saveSlot + 1 ] )
                M_DoSave( saveSlot )
            ENDIF
        ELSE
            IF ! Empty( vanilla_keyboard_mapping )
                ch := key
            ENDIF
            ch := Asc( Upper( Chr( ch ) ) )
            IF ch != 32 .AND. ( ch - HU_FONTSTART < 0 .OR. ch - HU_FONTSTART >= HU_FONTSIZE )
                RETURN .T.
            ENDIF
            IF ch >= 32 .AND. ch <= 127 ;
                 .AND. saveCharIndex < SAVESTRINGSIZE - 1 ;
                 .AND. M_StringWidth( savegamestrings[ saveSlot + 1 ] ) < ( SAVESTRINGSIZE - 2 ) * 8
                savegamestrings[ saveSlot + 1 ] := Left( savegamestrings[ saveSlot + 1 ], saveCharIndex ) + Chr( ch )
                saveCharIndex := saveCharIndex + 1
            ENDIF
        ENDIF
        RETURN .T.
    ENDIF

    IF messageToPrint != 0
        IF messageNeedsInput
            IF key != 32 .AND. key != KEY_ESCAPE .AND. key != key_menu_confirm .AND. key != key_menu_abort
                RETURN .F.
            ENDIF
        ENDIF
        menuactive := messageLastMenuActive
        messageToPrint := 0
        IF messageRoutine != NIL
            IfaceCall( messageRoutine, key )
        ENDIF
        menuactive := 0
        S_StartSound( NIL, sfx_swtchx )
        RETURN .T.
    ENDIF

    IF ( devparm .AND. key == key_menu_help ) .OR. ( key != 0 .AND. key == key_menu_screenshot )
        G_ScreenShot()
        RETURN .T.
    ENDIF

    IF menuactive == 0
        IF key == key_menu_decscreen
            IF automapactive .OR. chat_on
                RETURN .F.
            ENDIF
            M_SizeDisplay( 0 )
            S_StartSound( NIL, sfx_stnmov )
            RETURN .T.
        ELSEIF key == key_menu_incscreen
            IF automapactive .OR. chat_on
                RETURN .F.
            ENDIF
            M_SizeDisplay( 1 )
            S_StartSound( NIL, sfx_stnmov )
            RETURN .T.
        ELSEIF key == key_menu_help
            M_StartControlPanel()
            IF gamemode == retail
                currentMenu := ReadDef2
            ELSE
                currentMenu := ReadDef1
            ENDIF
            itemOn := 0
            S_StartSound( NIL, sfx_swtchn )
            RETURN .T.
        ELSEIF key == key_menu_save
            M_StartControlPanel()
            S_StartSound( NIL, sfx_swtchn )
            M_SaveGame( 0 )
            RETURN .T.
        ELSEIF key == key_menu_load
            M_StartControlPanel()
            S_StartSound( NIL, sfx_swtchn )
            M_LoadGame( 0 )
            RETURN .T.
        ELSEIF key == key_menu_volume
            M_StartControlPanel()
            currentMenu := SoundDef
            itemOn := sfx_vol
            S_StartSound( NIL, sfx_swtchn )
            RETURN .T.
        ELSEIF key == key_menu_detail
            M_ChangeDetail( 0 )
            S_StartSound( NIL, sfx_swtchn )
            RETURN .T.
        ELSEIF key == key_menu_qsave
            S_StartSound( NIL, sfx_swtchn )
            M_QuickSave()
            RETURN .T.
        ELSEIF key == key_menu_endgame
            S_StartSound( NIL, sfx_swtchn )
            M_EndGame( 0 )
            RETURN .T.
        ELSEIF key == key_menu_messages
            M_ChangeMessages( 0 )
            S_StartSound( NIL, sfx_swtchn )
            RETURN .T.
        ELSEIF key == key_menu_qload
            S_StartSound( NIL, sfx_swtchn )
            M_QuickLoad()
            RETURN .T.
        ELSEIF key == key_menu_quit
            S_StartSound( NIL, sfx_swtchn )
            M_QuitDOOM( 0 )
            RETURN .T.
        ELSEIF key == key_menu_gamma
            usegamma := usegamma + 1
            IF usegamma > 4
                usegamma := 0
            ENDIF
            players[ consoleplayer + 1 ]:message := DEH_String( gammamsg[ usegamma + 1 ] )
            I_SetPalette( W_CacheLumpName( DEH_String( "PLAYPAL" ), PU_CACHE ) )
            RETURN .T.
        ENDIF
    ENDIF

    IF menuactive == 0
        IF key == key_menu_activate
            M_StartControlPanel()
            S_StartSound( NIL, sfx_swtchn )
            RETURN .T.
        ENDIF
        RETURN .F.
    ENDIF

    IF key == key_menu_down
        DO WHILE .T.
            IF itemOn + 1 > currentMenu:numitems - 1
                itemOn := 0
            ELSE
                itemOn := itemOn + 1
            ENDIF
            S_StartSound( NIL, sfx_pstop )
            IF MenuItem( currentMenu, itemOn ):status != -1
                EXIT
            ENDIF
        ENDDO
        RETURN .T.
    ELSEIF key == key_menu_up
        DO WHILE .T.
            IF itemOn == 0
                itemOn := currentMenu:numitems - 1
            ELSE
                itemOn := itemOn - 1
            ENDIF
            S_StartSound( NIL, sfx_pstop )
            IF MenuItem( currentMenu, itemOn ):status != -1
                EXIT
            ENDIF
        ENDDO
        RETURN .T.
    ELSEIF key == key_menu_left
        oItem := MenuItem( currentMenu, itemOn )
        IF oItem:routine != NIL .AND. oItem:status == 2
            S_StartSound( NIL, sfx_stnmov )
            IfaceCall( oItem:routine, 0 )
        ENDIF
        RETURN .T.
    ELSEIF key == key_menu_right
        oItem := MenuItem( currentMenu, itemOn )
        IF oItem:routine != NIL .AND. oItem:status == 2
            S_StartSound( NIL, sfx_stnmov )
            IfaceCall( oItem:routine, 1 )
        ENDIF
        RETURN .T.
    ELSEIF key == key_menu_forward
        oItem := MenuItem( currentMenu, itemOn )
        IF oItem:routine != NIL .AND. oItem:status != 0
            currentMenu:lastOn := itemOn
            IF oItem:status == 2
                IfaceCall( oItem:routine, 1 )
                S_StartSound( NIL, sfx_stnmov )
            ELSE
                IfaceCall( oItem:routine, itemOn )
                S_StartSound( NIL, sfx_pistol )
            ENDIF
        ENDIF
        RETURN .T.
    ELSEIF key == key_menu_activate
        currentMenu:lastOn := itemOn
        M_ClearMenus()
        S_StartSound( NIL, sfx_swtchx )
        RETURN .T.
    ELSEIF key == key_menu_back
        currentMenu:lastOn := itemOn
        IF currentMenu:prevMenu != NIL
            currentMenu := currentMenu:prevMenu
            itemOn := currentMenu:lastOn
            S_StartSound( NIL, sfx_swtchn )
        ENDIF
        RETURN .T.
    ELSEIF ch != 0 .OR. IsNullKey( key )
        FOR i := itemOn + 1 TO currentMenu:numitems - 1
            IF MenuItem( currentMenu, i ):alphaKey == ch
                itemOn := i
                S_StartSound( NIL, sfx_pstop )
                RETURN .T.
            ENDIF
        NEXT
        FOR i := 0 TO itemOn
            IF MenuItem( currentMenu, i ):alphaKey == ch
                itemOn := i
                S_StartSound( NIL, sfx_pstop )
                RETURN .T.
            ENDIF
        NEXT
    ENDIF
RETURN .F.

FUNCTION M_StartControlPanel()
    MEMVAR menuactive
    IF menuactive != 0
        RETURN NIL
    ENDIF
    menuactive := 1
    currentMenu := MainDef
    itemOn := currentMenu:lastOn
RETURN NIL

FUNCTION M_Drawer()
    LOCAL i
    LOCAL nMax
    LOCAL cString
    LOCAL cName
    LOCAL nStart
    LOCAL nFound
    LOCAL cRest
    LOCAL nI
    MEMVAR hu_font
    MEMVAR inhelpscreens
    MEMVAR menuactive

    inhelpscreens := .F.

    IF messageToPrint != 0
        nStart := 0
        nDrawY := Int( SCREENHEIGHT / 2 ) - Int( M_StringHeight( messageString ) / 2 )
        DO WHILE nStart < Len( messageString )
            nFound := 0
            cRest := SubStr( messageString, nStart + 1 )
            FOR nI := 0 TO Len( cRest ) - 1
                IF SubStr( cRest, nI + 1, 1 ) == Chr( 10 )
                    cString := Left( cRest, nI )
                    nFound := 1
                    nStart := nStart + nI + 1
                    EXIT
                ENDIF
            NEXT
            IF nFound == 0
                cString := Left( cRest, 79 )
                nStart := nStart + Len( cString )
            ENDIF
            nDrawX := Int( SCREENWIDTH / 2 ) - Int( M_StringWidth( cString ) / 2 )
            M_WriteText( nDrawX, nDrawY, cString )
            nDrawY := nDrawY + MenuPatchH( hu_font[ 1 ] )
        ENDDO
        RETURN NIL
    ENDIF

    IF menuactive == 0
        RETURN NIL
    ENDIF

    IF currentMenu:routine != NIL
        IfaceCall( currentMenu:routine )
    ENDIF

    nDrawX := currentMenu:x
    nDrawY := currentMenu:y
    nMax := currentMenu:numitems

    FOR i := 0 TO nMax - 1
        cName := DEH_String( MenuItem( currentMenu, i ):name )
        IF ! Empty( cName )
            V_DrawPatchDirect( nDrawX, nDrawY, W_CacheLumpName( cName, PU_CACHE ) )
        ENDIF
        nDrawY := nDrawY + LINEHEIGHT
    NEXT

    V_DrawPatchDirect( nDrawX + SKULLXOFF, currentMenu:y - 5 + itemOn * LINEHEIGHT, ;
        W_CacheLumpName( DEH_String( skullName[ whichSkull + 1 ] ), PU_CACHE ) )
RETURN NIL

FUNCTION M_ClearMenus()
    MEMVAR menuactive
    menuactive := 0
RETURN NIL

FUNCTION M_SetupNextMenu( menudef )
    currentMenu := menudef
    itemOn := currentMenu:lastOn
RETURN NIL

FUNCTION M_Ticker()
    skullAnimCounter := skullAnimCounter - 1
    IF skullAnimCounter <= 0
        whichSkull := ( whichSkull ^^ 1 )
        skullAnimCounter := 8
    ENDIF
RETURN NIL

FUNCTION M_Init()
    MEMVAR gamemode
    MEMVAR gameversion
    MEMVAR menuactive
    MEMVAR screenblocks
    currentMenu := MainDef
    menuactive := 0
    itemOn := currentMenu:lastOn
    whichSkull := 0
    skullAnimCounter := 10
    screenSize := screenblocks - 3
    messageToPrint := 0
    messageString := NIL
    messageLastMenuActive := menuactive
    quickSaveSlot := -1

    SWITCH gamemode
    CASE commercial
        CopyItem( MainMenu[ readthis + 1 ], MainMenu[ quitdoom + 1 ] )
        MainDef:numitems := MainDef:numitems - 1
        MainDef:y := MainDef:y + 8
        NewDef:prevMenu := MainDef
        EXIT
    CASE shareware
        EXIT
    CASE registered
        EXIT
    CASE retail
        EXIT
    ENDSWITCH

    IF gameversion < exe_ultimate
        EpiDef:numitems := EpiDef:numitems - 1
    ENDIF
RETURN NIL


