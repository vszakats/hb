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

STATIC iwads := {}
STATIC iwad_dirs := {}
STATIC iwad_dirs_built
STATIC num_iwad_dirs
STATIC software_key
STATIC uninstall_values := {}
STATIC collectors_edition_value
STATIC collectors_edition_subdirs := {}
STATIC steam_install_location
STATIC steam_install_subdirs := {}

/* Original: config.h, deh_str.h, doomkeys.h, i_system.h, m_argv.h,
   m_config.h, m_misc.h, w_wad.h, z_zone.h — converter para .ch quando existirem */
#include "d_iwad.ch"

CLASS iwad_t
    DATA name
    DATA mission
    DATA mode
    DATA description
    METHOD New()
ENDCLASS

#include "deh_str.ch"

#ifndef FILES_DIR
#define FILES_DIR "."
#endif

#ifndef MAX_IWAD_DIRS
#define MAX_IWAD_DIRS 128
#endif

#define DIR_SEPARATOR   hb_ps()
#define DIR_SEPARATOR_S hb_ps()
#define PATH_SEPARATOR  ";"

#define UNINSTALLER_STRING "\uninstl.exe /S "
#define HKEY_LOCAL_MACHINE 0x80000002
#define STEAM_BFG_GUS_PATCHES "steamapps\common\DOOM 3 BFG Edition\base\classicmusic\instruments"


#ifdef __PLATFORM__WINDOWS

CLASS registry_value_t
    DATA root
    DATA path
    DATA value
    METHOD New()
ENDCLASS
#endif

METHOD New( name, mission, mode, description ) CLASS iwad_t
    ::name        := iif( name == NIL, "", name )
    ::mission     := iif( mission == NIL, 0, mission )
    ::mode        := iif( mode == NIL, 0, mode )
    ::description := iif( description == NIL, "", description )
RETURN Self

#ifdef __PLATFORM__WINDOWS
METHOD New( root, path, value ) CLASS registry_value_t
    ::root  := iif( root == NIL, 0, root )
    ::path  := iif( path == NIL, "", path )
    ::value := iif( value == NIL, "", value )
RETURN Self
#endif

INIT PROCEDURE init_d_iwad

    iwads := {}
    AAdd( iwads, iwad_t():New( "doom2.wad",     doom2,     commercial, "Doom II" ) )
    AAdd( iwads, iwad_t():New( "plutonia.wad",  pack_plut, commercial, "Final Doom: Plutonia Experiment" ) )
    AAdd( iwads, iwad_t():New( "tnt.wad",       pack_tnt,  commercial, "Final Doom: TNT: Evilution" ) )
    AAdd( iwads, iwad_t():New( "doom.wad",      doom,      retail,     "Doom" ) )
    AAdd( iwads, iwad_t():New( "doom1.wad",     doom,      shareware,  "Doom Shareware" ) )
    AAdd( iwads, iwad_t():New( "chex.wad",      pack_chex, shareware,  "Chex Quest" ) )
    AAdd( iwads, iwad_t():New( "hacx.wad",      pack_hacx, commercial, "Hacx" ) )
    AAdd( iwads, iwad_t():New( "freedm.wad",    doom2,     commercial, "FreeDM" ) )
    AAdd( iwads, iwad_t():New( "freedoom2.wad", doom2,     commercial, "Freedoom: Phase 2" ) )
    AAdd( iwads, iwad_t():New( "freedoom1.wad", doom,      retail,     "Freedoom: Phase 1" ) )
    AAdd( iwads, iwad_t():New( "heretic.wad",   heretic,   retail,     "Heretic" ) )
    AAdd( iwads, iwad_t():New( "heretic1.wad",  heretic,   shareware,  "Heretic Shareware" ) )
    AAdd( iwads, iwad_t():New( "hexen.wad",     hexen,     commercial, "Hexen" ) )
    AAdd( iwads, iwad_t():New( "strife1.wad",   strife,    commercial, "Strife" ) )

    iwad_dirs := {}
    iwad_dirs_built := .F.
    num_iwad_dirs := 0

#ifdef __PLATFORM__WINDOWS
#ifdef __WIN64__
    software_key := "Software\Wow6432Node"
#else
    software_key := "Software"
#endif

    uninstall_values := {}
    AAdd( uninstall_values, registry_value_t():New( HKEY_LOCAL_MACHINE, ;
        software_key + "\Microsoft\Windows\CurrentVersion\Uninstall\Ultimate Doom for Windows 95", ;
        "UninstallString" ) )
    AAdd( uninstall_values, registry_value_t():New( HKEY_LOCAL_MACHINE, ;
        software_key + "\Microsoft\Windows\CurrentVersion\Uninstall\Doom II for Windows 95", ;
        "UninstallString" ) )
    AAdd( uninstall_values, registry_value_t():New( HKEY_LOCAL_MACHINE, ;
        software_key + "\Microsoft\Windows\CurrentVersion\Uninstall\Final Doom for Windows 95", ;
        "UninstallString" ) )
    AAdd( uninstall_values, registry_value_t():New( HKEY_LOCAL_MACHINE, ;
        software_key + "\Microsoft\Windows\CurrentVersion\Uninstall\Doom Shareware for Windows 95", ;
        "UninstallString" ) )

    collectors_edition_value := registry_value_t():New( HKEY_LOCAL_MACHINE, ;
        software_key + "\Activision\DOOM Collector's Edition\v1.0", ;
        "INSTALLPATH" )

    collectors_edition_subdirs := {}
    AAdd( collectors_edition_subdirs, "Doom2" )
    AAdd( collectors_edition_subdirs, "Final Doom" )
    AAdd( collectors_edition_subdirs, "Ultimate Doom" )

    steam_install_location := registry_value_t():New( HKEY_LOCAL_MACHINE, ;
        software_key + "\Valve\Steam", ;
        "InstallPath" )

    steam_install_subdirs := {}
    AAdd( steam_install_subdirs, "steamapps\common\doom 2\base" )
    AAdd( steam_install_subdirs, "steamapps\common\final doom\base" )
    AAdd( steam_install_subdirs, "steamapps\common\ultimate doom\base" )
    AAdd( steam_install_subdirs, "steamapps\common\heretic shadow of the serpent riders\base" )
    AAdd( steam_install_subdirs, "steamapps\common\hexen\base" )
    AAdd( steam_install_subdirs, "steamapps\common\hexen deathkings of the dark citadel\base" )
    AAdd( steam_install_subdirs, "steamapps\common\DOOM 3 BFG Edition\base\wads" )
#endif

RETURN

STATIC FUNCTION MissionInMask( mission, mask )
RETURN ( ( Int( 2 ^ mission ) & mask ) != 0 )

STATIC FUNCTION AddIWADDir( dir )
    IF num_iwad_dirs < MAX_IWAD_DIRS
        AAdd( iwad_dirs, dir )
        num_iwad_dirs := ( num_iwad_dirs + 1 )
    ENDIF
RETURN NIL

STATIC FUNCTION DirIsFile( path, filename )
    LOCAL path_len
    LOCAL filename_len

    path_len := Len( path )
    filename_len := Len( filename )

    IF path_len >= filename_len + 1 ;
         .AND. SubStr( path, path_len - filename_len, 1 ) == DIR_SEPARATOR ;
         .AND. hb_stricmp( SubStr( path, path_len - filename_len + 1 ), filename ) == 0
        RETURN .T.
    ENDIF
RETURN .F.

STATIC FUNCTION CheckDirectoryHasIWAD( dir, iwadname )
    LOCAL filename

    IF DirIsFile( dir, iwadname ) .AND. M_FileExists( dir )
        RETURN dir
    ENDIF

    IF dir == "."
        filename := iwadname
    ELSE
        filename := M_StringJoin( dir, DIR_SEPARATOR_S, iwadname )
    ENDIF

    OutStd( "Trying IWAD file:" + filename + hb_eol() )

    IF M_FileExists( filename )
        RETURN filename
    ENDIF
RETURN NIL

STATIC FUNCTION SearchDirectoryForIWAD( dir, mask, mission )
    LOCAL filename
    LOCAL i

    /* doom1.wad e o padrao quando os dois IWADs estao na mesma pasta. */
    IF MissionInMask( doom, mask )
        filename := CheckDirectoryHasIWAD( dir, "doom1.wad" )
        IF filename != NIL
            mission := doom
            RETURN filename
        ENDIF
    ENDIF

    FOR i := 0 TO Len( iwads ) - 1
        IF ! MissionInMask( iwads[ i + 1 ]:mission, mask )
            LOOP
        ENDIF

        filename := CheckDirectoryHasIWAD( dir, DEH_String( iwads[ i + 1 ]:name ) )

        IF filename != NIL
            mission := iwads[ i + 1 ]:mission
            RETURN filename
        ENDIF
    NEXT
RETURN NIL

STATIC FUNCTION IdentifyIWADByName( name, mask )
    LOCAL i
    LOCAL mission
    LOCAL p

    p := RAt( DIR_SEPARATOR, name )
    IF p > 0
        name := SubStr( name, p + 1 )
    ENDIF

    mission := none

    FOR i := 0 TO Len( iwads ) - 1
        IF ! MissionInMask( iwads[ i + 1 ]:mission, mask )
            LOOP
        ENDIF

        IF hb_stricmp( name, iwads[ i + 1 ]:name ) == 0
            mission := iwads[ i + 1 ]:mission
            EXIT
        ENDIF
    NEXT
RETURN mission

#ifdef ORIGCODE
STATIC FUNCTION AddDoomWadPath()
    LOCAL doomwadpath
    LOCAL aParts
    LOCAL p

    doomwadpath := GetEnv( "DOOMWADPATH" )
    IF Empty( doomwadpath )
        RETURN NIL
    ENDIF

    aParts := hb_ATokens( doomwadpath, PATH_SEPARATOR )
    FOR EACH p IN aParts
        IF ! Empty( p )
            AddIWADDir( p )
        ENDIF
    NEXT
RETURN NIL
#endif

#ifdef __PLATFORM__WINDOWS
STATIC FUNCTION GetRegistryString( reg_val )
RETURN GetRegistryStringC( reg_val:root, reg_val:path, reg_val:value )

STATIC FUNCTION CheckUninstallStrings()
    LOCAL i
    LOCAL val
    LOCAL path
    LOCAL nPos

    FOR i := 0 TO Len( uninstall_values ) - 1
        val := GetRegistryString( uninstall_values[ i + 1 ] )
        IF val == NIL
            LOOP
        ENDIF

        nPos := At( UNINSTALLER_STRING, val )
        IF nPos == 0
            LOOP
        ENDIF

        path := SubStr( val, nPos + Len( UNINSTALLER_STRING ) )
        AddIWADDir( path )
    NEXT
RETURN NIL

STATIC FUNCTION CheckCollectorsEdition()
    LOCAL install_path
    LOCAL subpath
    LOCAL i

    install_path := GetRegistryString( collectors_edition_value )
    IF install_path == NIL
        RETURN NIL
    ENDIF

    FOR i := 0 TO Len( collectors_edition_subdirs ) - 1
        subpath := M_StringJoin( install_path, DIR_SEPARATOR_S, collectors_edition_subdirs[ i + 1 ] )
        AddIWADDir( subpath )
    NEXT
RETURN NIL

STATIC FUNCTION CheckSteamEdition()
    LOCAL install_path
    LOCAL subpath
    LOCAL i

    install_path := GetRegistryString( steam_install_location )
    IF install_path == NIL
        RETURN NIL
    ENDIF

    FOR i := 0 TO Len( steam_install_subdirs ) - 1
        subpath := M_StringJoin( install_path, DIR_SEPARATOR_S, steam_install_subdirs[ i + 1 ] )
        AddIWADDir( subpath )
    NEXT
RETURN NIL

STATIC FUNCTION CheckSteamGUSPatches()
    LOCAL current_path
    LOCAL install_path
    LOCAL patch_path

    current_path := M_GetStrVariable( "gus_patch_path" )
    IF current_path != NIL .AND. Len( current_path ) > 0
        RETURN NIL
    ENDIF

    install_path := GetRegistryString( steam_install_location )
    IF install_path == NIL
        RETURN NIL
    ENDIF

    patch_path := install_path + DIR_SEPARATOR_S + STEAM_BFG_GUS_PATCHES + DIR_SEPARATOR_S + "ACBASS.PAT"

    IF M_FileExists( patch_path )
        patch_path := install_path + DIR_SEPARATOR_S + STEAM_BFG_GUS_PATCHES
        M_SetVariable( "gus_patch_path", patch_path )
    ENDIF
RETURN NIL

STATIC FUNCTION CheckDOSDefaults()
    AddIWADDir( "\doom2" )
    AddIWADDir( "\plutonia" )
    AddIWADDir( "\tnt" )
    AddIWADDir( "\doom_se" )
    AddIWADDir( "\doom" )
    AddIWADDir( "\dooms" )
    AddIWADDir( "\doomsw" )
    AddIWADDir( "\heretic" )
    AddIWADDir( "\hrtic_se" )
    AddIWADDir( "\hexen" )
    AddIWADDir( "\hexendk" )
    AddIWADDir( "\strife" )
RETURN NIL
#endif

STATIC FUNCTION BuildIWADDirList()
#ifdef ORIGCODE
    LOCAL doomwaddir

    IF iwad_dirs_built
        RETURN NIL
    ENDIF

    AddIWADDir( "." )

    doomwaddir := GetEnv( "DOOMWADDIR" )
    IF ! Empty( doomwaddir )
        AddIWADDir( doomwaddir )
    ENDIF

    AddDoomWadPath()

#ifdef __PLATFORM__WINDOWS
    CheckUninstallStrings()
    CheckCollectorsEdition()
    CheckSteamEdition()
    CheckDOSDefaults()
    CheckSteamGUSPatches()
#else
    AddIWADDir( "/usr/share/games/doom" )
    AddIWADDir( "/usr/local/share/games/doom" )
#endif
#else
    AddIWADDir( FILES_DIR )
    iwad_dirs_built := .T.
#endif
RETURN NIL

FUNCTION D_FindWADByName( name )
    LOCAL path
    LOCAL i

    IF M_FileExists( name )
        RETURN name
    ENDIF

    BuildIWADDirList()

    FOR i := 0 TO num_iwad_dirs - 1
        IF DirIsFile( iwad_dirs[ i + 1 ], name ) .AND. M_FileExists( iwad_dirs[ i + 1 ] )
            RETURN iwad_dirs[ i + 1 ]
        ENDIF

        path := M_StringJoin( iwad_dirs[ i + 1 ], DIR_SEPARATOR_S, name )

        IF M_FileExists( path )
            RETURN path
        ENDIF
    NEXT
RETURN NIL

FUNCTION D_TryFindWADByName( filename )
    LOCAL result

    result := D_FindWADByName( filename )
    IF result != NIL
        RETURN result
    ENDIF
RETURN filename

FUNCTION D_FindIWAD( mask, mission )
    LOCAL result
    LOCAL iwadfile
    LOCAL iwadparm
    LOCAL i
    MEMVAR myargv

    iwadparm := M_CheckParmWithArgs( "-iwad", 1 )

    IF iwadparm != 0
        iwadfile := myargv[ iwadparm + 1 + 1 ]
        result := D_FindWADByName( iwadfile )

        IF result == NIL
            I_Error( "IWAD file '" + iwadfile + "' not found!" )
        ENDIF

        mission := IdentifyIWADByName( result, mask )
    ELSE
        OutStd( "-iwad not specified, trying a few iwad names" + hb_eol() )

        result := NIL
        BuildIWADDirList()

        FOR i := 0 TO num_iwad_dirs - 1
            IF result != NIL
                EXIT
            ENDIF
            result := SearchDirectoryForIWAD( iwad_dirs[ i + 1 ], mask, @mission )
        NEXT
    ENDIF
RETURN result

FUNCTION D_FindAllIWADs( mask )
    LOCAL result
    LOCAL filename
    LOCAL i

    result := {}

    FOR i := 0 TO Len( iwads ) - 1
        IF ! MissionInMask( iwads[ i + 1 ]:mission, mask )
            LOOP
        ENDIF

        filename := D_FindWADByName( iwads[ i + 1 ]:name )
        IF filename != NIL
            AAdd( result, iwads[ i + 1 ] )
        ENDIF
    NEXT

    AAdd( result, NIL )
RETURN result

FUNCTION D_SaveGameIWADName( gamemission )
    LOCAL i

    FOR i := 0 TO Len( iwads ) - 1
        IF gamemission == iwads[ i + 1 ]:mission
            RETURN iwads[ i + 1 ]:name
        ENDIF
    NEXT
RETURN "unknown.wad"

FUNCTION D_SuggestIWADName( mission, mode )
    LOCAL i

    FOR i := 0 TO Len( iwads ) - 1
        IF iwads[ i + 1 ]:mission == mission .AND. iwads[ i + 1 ]:mode == mode
            RETURN iwads[ i + 1 ]:name
        ENDIF
    NEXT
RETURN "unknown.wad"

FUNCTION D_SuggestGameName( mission, mode )
    LOCAL i

    FOR i := 0 TO Len( iwads ) - 1
        IF iwads[ i + 1 ]:mission == mission ;
             .AND. ( mode == indetermined .OR. iwads[ i + 1 ]:mode == mode )
            RETURN iwads[ i + 1 ]:description
        ENDIF
    NEXT
RETURN "Unknown game?"

/* Declarada em d_iwad.h, mas nao ha implementacao neste d_iwad.c */
FUNCTION D_CheckCorrectIWAD( mission )
    HB_SYMBOL_UNUSED( mission )
RETURN NIL

#pragma BEGINDUMP
#include "hbapi.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

HB_FUNC( GETREGISTRYSTRINGC )
{
   HKEY root;
   const char *path;
   const char *value;
   HKEY key;
   DWORD len;
   DWORD valtype;
   char *result;

   root = (HKEY) ( HB_PTRUINT ) hb_parnint( 1 );
   path = hb_parc( 2 );
   value = hb_parc( 3 );

   if( path == NULL || value == NULL )
   {
      hb_ret();
      return;
   }

   if( RegOpenKeyEx( root, path, 0, KEY_READ, &key ) != ERROR_SUCCESS )
   {
      hb_ret();
      return;
   }

   result = NULL;

   if( RegQueryValueEx( key, value, NULL, &valtype, NULL, &len ) == ERROR_SUCCESS
    && valtype == REG_SZ )
   {
      result = ( char * ) hb_xgrab( len + 1 );

      if( RegQueryValueEx( key, value, NULL, &valtype, ( LPBYTE ) result, &len ) != ERROR_SUCCESS )
      {
         hb_xfree( result );
         result = NULL;
      }
      else
      {
         result[ len ] = '\0';
      }
   }

   RegCloseKey( key );

   if( result != NULL )
   {
      hb_retc( result );
      hb_xfree( result );
   }
   else
   {
      hb_ret();
   }
}
#else
HB_FUNC( GETREGISTRYSTRINGC )
{
   hb_ret();
}
#endif
#pragma ENDDUMP
