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

#include "boot.ch"
#include "doomgeneric.ch"
#include "doomstat.ch"
#include "doomdef.ch"
#include "d_mode.ch"
#include "d_iwad.ch"
#include "z_zone.ch"
#include "config.ch"


INIT PROCEDURE init_boot
RETURN

STATIC FUNCTION FindFixedIwad()
    LOCAL candidates := { ;
        MINIAL_IWAD, "doom1.wad", "Doom1.wad", ;
        "doom.wad", "DOOM.WAD", ;
        "doom2.wad", "DOOM2.WAD", ;
        "plutonia.wad", "tnt.wad", ;
        "freedoom1.wad", "freedoom2.wad" }
    LOCAL i
    FOR i := 1 TO Len( candidates )
        IF M_FileExists( candidates[ i ] )
            RETURN candidates[ i ]
        ENDIF
    NEXT
RETURN NIL

STATIC FUNCTION FindCommandIwad()
    LOCAL n
    LOCAL i
    LOCAL cArg
    MEMVAR myargc
    MEMVAR myargv

    n := M_CheckParmWithArgs( "-iwad", 1 )
    IF n != 0
        RETURN myargv[ n + 1 + 1 ]
    ENDIF

    FOR i := 1 TO myargc - 1
        cArg := myargv[ i + 1 ]
        IF Empty( cArg ) .OR. Left( cArg, 1 ) == "-"
            LOOP
        ENDIF
        IF Lower( Right( cArg, 4 ) ) == ".wad"
            RETURN cArg
        ENDIF
    NEXT
RETURN NIL

STATIC FUNCTION ResolveIwad()
    LOCAL cName
    LOCAL cFound
    MEMVAR gamemission

    cName := FindCommandIwad()
    IF ! Empty( cName )
        cFound := D_FindWADByName( cName )
        IF cFound == NIL
            I_Error( "IWAD '" + cName + "' nao encontrado." + hb_eol() + ;
                "Use: Doom_hb.exe -iwad arquivo.wad" + hb_eol() )
        ENDIF
        RETURN cFound
    ENDIF

    cFound := D_FindIWAD( IWAD_MASK_DOOM, @gamemission )
    IF ! Empty( cFound )
        RETURN cFound
    ENDIF
RETURN FindFixedIwad()

STATIC PROCEDURE InitGameVersionMinimal()
    MEMVAR gamemission
    MEMVAR gamemode
    MEMVAR gameversion
    IF gamemode == shareware .OR. gamemode == registered
        gameversion := exe_doom_1_9
    ELSEIF gamemode == retail
        gameversion := exe_ultimate
    ELSEIF gamemode == commercial
        gameversion := iif( gamemission == doom2, exe_doom_1_9, exe_final )
    ENDIF
    IF gameversion < exe_ultimate .AND. gamemode == retail
        gamemode := registered
    ENDIF
RETURN

STATIC PROCEDURE D_DoomMainMinimal()
    LOCAL iwad
    LOCAL wad
    MEMVAR devparm
    MEMVAR fastparm
    MEMVAR gamemission
    MEMVAR modifiedgame
    MEMVAR musicVolume
    MEMVAR nomonsters
    MEMVAR respawnparm
    MEMVAR savegamedir
    MEMVAR sfxVolume

    Z_Init()

    nomonsters := .F.
    respawnparm := .F.
    fastparm := .F.
    devparm := .F.
    modifiedgame := .F.

    M_SetConfigDir( NIL )
    V_Init()

    M_SetConfigFilenames( "default.cfg", PROGRAM_PREFIX + "doom.cfg" )
    D_BindVariables()
    M_LoadDefaults()
    I_AtExit( {|| M_SaveDefaults() }, .F. )

    iwad := ResolveIwad()
    IF iwad == NIL
        I_Error( "Nenhum IWAD encontrado. Coloque doom1.wad / doom.wad / doom2.wad" + hb_eol() + ;
            "nesta pasta ou use: Doom_hb.exe -iwad arquivo.wad" + hb_eol() )
    ENDIF

    gamemission := none

    OutStd( "IWAD: " + iwad + hb_eol() )
    wad := W_AddFile( iwad )
    IF wad == NIL
        I_Error( "doom_hb: falha ao abrir " + iwad )
    ENDIF

    W_CheckCorrectIWAD( doom )
    D_IdentifyVersion()
    InitGameVersionMinimal()
    W_GenerateHashTable()
    D_SetGameDescription()

    savegamedir := M_GetSaveGameDir( D_SaveGameIWADName( gamemission ) )

    I_CheckIsScreensaver()
    I_InitTimer()
    I_InitJoystick()
    I_InitSound( .T. )
    I_InitMusic()

    D_ConnectNetGame()

    M_Init()
    R_Init()
    P_Init()
    S_Init( sfxVolume * 8, musicVolume * 8 )
    D_CheckNetGame()
    HU_Init()
    ST_Init()

    D_StartTitle()
    D_DoomLoop()
RETURN

PROCEDURE doom_hb_Create()
    LOCAL i
    MEMVAR myargc
    MEMVAR myargv

    IF ValType( myargv ) != "A" .OR. Len( myargv ) == 0
        myargv := {}
        FOR i := 0 TO hb_argc()
            AAdd( myargv, hb_argv( i ) )
        NEXT
    ENDIF
    myargc := Len( myargv )

    DG_Init()
    D_DoomMain()
RETURN
