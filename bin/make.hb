#!/usr/bin/env hbmk2
/*
 * When started in a project (contrib) directory, this script will build
 * that single project along with its dependencies in the same project
 * store. When started in a project store root and passed "." as a parameter,
 * it will rebuild _all_ projects. Build order is determined according to
 * the dependency graph of involved projects.
 *
 * Copyright 2010-2017 Viktor Szakats (vszakats.net/harbour)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their website at https://www.gnu.org/).
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"

#define _ACT_INC_CLEAN          1
#define _ACT_INC                2
#define _ACT_INC_INST           3
#define _ACT_INC_REBUILD        4
#define _ACT_INC_REBUILD_INST   5

STATIC sc_hActions := { ;
   _ACT_INC_CLEAN        => "clean", ;
   _ACT_INC              => "build", ;
   _ACT_INC_INST         => "build and install", ;
   _ACT_INC_REBUILD      => "rebuild", ;
   _ACT_INC_REBUILD_INST => "rebuild and install" }

STATIC s_cRoot    /* source tree root directory */
STATIC s_cHome    /* project store root directory (f.e. 'contrib/') */
STATIC s_cBinDir  /* directory where the hbmk2 executing this script resides */

STATIC s_lCoreBuild
STATIC s_lAddon

#define AScanL( aArray, cString )  hb_AScanI( aArray, cString,,, .T. )

PROCEDURE Main( ... )

   LOCAL hProjectList := { => }
   LOCAL aParams := hb_AParams()

   LOCAL nCount, tmp

   hb_cdpSelect( "UTF8" )

#if defined( __HBSCRIPT__HBSHELL )
   s_cBinDir := hbshell_DirBase()
#else
   s_cBinDir := hb_DirBase()
#endif
   s_cBinDir := hb_PathNormalize( s_cBinDir )  /* For *nixes */

   /* Find source tree root.
      Check current up-level dirs, then start again from hbmk2's directory */
   FOR EACH tmp IN { "", s_cBinDir }
      nCount := 5
      DO WHILE nCount-- > 0 .AND. ! hb_vfExists( ( s_cRoot := hb_DirSepToOS( tmp + Replicate( "../", nCount ) ) ) + "include" + hb_ps() + "hbvm.h" )
      ENDDO
      IF nCount > -1
         EXIT
      ELSEIF tmp:__enumIsLast()
         OutErr( hb_StrFormat( "! Could not determine Harbour source root directory" ) + hb_eol() )
         RETURN
      ENDIF
   NEXT

   s_cRoot := StrTran( hb_PathNormalize( s_cRoot ), "\", "/" )

   /* Project store root */
   IF AScanL( aParams, "." ) > 0
      s_cHome := "./"   /* Current directory in all-project mode */
   ELSE
      s_cHome := "../"  /* Assume it to be one level up when run for a single-project (default) */
   ENDIF

   /* Running as part of the core GNU Make build process? */
   s_lCoreBuild := ! GetEnv( "HB_HOST_BIN_DIR" ) == ""
   /* Is this an add-on (= not a core contrib) run? */
   s_lAddon := ! hb_FileMatch( ;
      hb_PathNormalize( hb_PathJoin( hb_cwd(), hb_DirSepToOS( s_cHome ) ) ), ;
      hb_PathNormalize( hb_PathJoin( hb_cwd(), hb_DirSepToOS( s_cRoot + "/contrib/" ) ) ) )

   OutStd( hb_StrFormat( "! Harbour root: '%1$s'  Project store: '%2$s'  Core build: %3$s  Addon: %4$s", ;
      s_cRoot, s_cHome, iif( s_lCoreBuild, "yes", "no" ), iif( s_lAddon, "yes", "no" ) ) + hb_eol() )

   /* Load list of projects */

   LoadProjectListAutomatic( hProjectList )
   LoadProjectListFromString( hProjectList, GetEnv( "HB_BUILD_ADDONS" ) )

   IF AScanL( aParams, "verbose" ) > 0
      hb_SetEnv( "HB_BUILD_VERBOSE", "yes" )
   ENDIF

   /* Build */
   IF AScanL( aParams, "." ) > 0
      BuildAll( aParams, hProjectList )
   ELSE
      BuildSingle( aParams, hProjectList )
   ENDIF

   RETURN

/* Workflow translation in single-project mode:

      GNU Make       parameter      nAction                hbmk2 options
   -- -------------- -------------- ---------------------- -------------------------
   #1 clean          clean          _ACT_INC_CLEAN         -inc -clean
   #2                               _ACT_INC               -inc
   #3 clean all      clean all      _ACT_INC_REBUILD       -inc -rebuildall
   #4 install        install        _ACT_INC_INST          -inc -instpath=
   #5 clean install  clean install  _ACT_INC_REBUILD_INST  -inc -rebuildall -instpath=
 */
STATIC PROCEDURE BuildSingle( aParams, hProjectList )

   LOCAL hProjectReqList

   LOCAL cOptionsUser

   LOCAL nAction
   LOCAL tmp

   LOCAL lPassThrough_hbmk2 := .F.

   /* Processing cmdline options */

   DO CASE
   CASE AScanL( aParams, "clean" ) > 0 .AND. ;
        AScanL( aParams, "all" ) > 0 .AND. ;
        AScanL( aParams, "all" ) > AScanL( aParams, "clean" )
      nAction := _ACT_INC_REBUILD
   CASE AScanL( aParams, "rebuild" ) > 0
      nAction := _ACT_INC_REBUILD
   CASE AScanL( aParams, "clean" ) > 0 .AND. ;
        AScanL( aParams, "install" ) > 0 .AND. ;
        AScanL( aParams, "install" ) > AScanL( aParams, "clean" )
      nAction := _ACT_INC_REBUILD_INST
   CASE AScanL( aParams, "clean" ) > 0
      nAction := _ACT_INC_CLEAN
   CASE AScanL( aParams, "install" ) > 0
      nAction := _ACT_INC_INST
   OTHERWISE
      nAction := _ACT_INC
   ENDCASE

   /* Processing user options */

   cOptionsUser := ""
   FOR EACH tmp IN aParams
      IF ! Lower( tmp ) == "install" .AND. ;
         ! Lower( tmp ) == "clean" .AND. ;
         ! Lower( tmp ) == "all" .AND. ;
         ! Lower( tmp ) == "first" .AND. ;
         ! Lower( tmp ) == "rebuild" .AND. ;
         ! Lower( tmp ) == "verbose"

         cOptionsUser += " " + tmp

         /* If anything else is passed than options or GNU Make keywords,
            switch to hbmk2 pass-through mode, f.e. in tests */
         IF ! hb_LeftEq( tmp, "-" )
            lPassThrough_hbmk2 := .T.
         ENDIF
      ENDIF
   NEXT

   /* Assemble list of primary targets (registered projects in or under
      current directory) */

   hProjectReqList := { => }

   IF ! lPassThrough_hbmk2
      /* Find out which projects are in current dir, these will be our
         primary targets */
      FOR EACH tmp IN hProjectList
         IF hb_LeftEq( hb_DirSepToOS( tmp:__enumKey() ) + hb_ps(), hb_FNameNameExt( hb_DirSepDel( hb_cwd() ) ) )  /* Not ultimate solution */
            hProjectReqList[ tmp:__enumKey() ] := tmp:__enumKey()
         ENDIF
      NEXT
      IF Empty( hProjectReqList )
         lPassThrough_hbmk2 := .T.
      ELSE
         OutStd( hb_StrFormat( "! Initiating %1$s... %2$d project(s)", sc_hActions[ nAction ], Len( hProjectReqList ) ) + hb_eol() )
      ENDIF
   ENDIF

   IF lPassThrough_hbmk2  /* pass request to hbmk2 as-is */
      mk_hb_processRun( s_cBinDir + "hbmk2" + cOptionsUser )
      RETURN
   ENDIF

   /* Start building */

   build_projects( nAction, hProjectList, hProjectReqList, cOptionsUser )

   RETURN

/* Workflow translation from GNU Make to hbmk2:

      GNU Make       parameter  HB_MAKECMDGOALS  nAction                hbmk2 options
   -- -------------- ---------- ---------------- ---------------------- -------------------------
   #1 clean          clean      clean            _ACT_INC_CLEAN         -inc -clean
   #2                all                         _ACT_INC               -inc
   #3 install        install    install          _ACT_INC_INST          -inc -instpath=
   #4 clean all      clean      clean all        _ACT_INC_CLEAN         -inc -clean
                     first      clean all        _ACT_INC_REBUILD       -inc -rebuildall
   #5 clean install  clean      clean install    _ACT_INC_CLEAN         -inc -clean
                     install    clean install    _ACT_INC_REBUILD_INST  -inc -rebuildall -instpath=
   #6 install clean  install    install clean    _ACT_INC_INST          -inc -instpath=
                     clean      install clean    _ACT_INC_CLEAN         -inc -clean
 */
STATIC PROCEDURE BuildAll( aParams, hProjectList )

   LOCAL cProject
   LOCAL hProjectReqList

   LOCAL cFilter
   LOCAL aFilter
   LOCAL lFilterNegative

   LOCAL aGNUMakeParams
   LOCAL nAction
   LOCAL tmp

   IF s_lCoreBuild

      /* All GNU Make requests, not just the one being executed right now */
      aGNUMakeParams := hb_ATokens( GetEnv( "HB_MAKECMDGOALS" ) )

      /* Check requirements when running as part of the core GNU Make build */

      IF Empty( GetEnv( "HB_PLATFORM" ) ) .OR. ;
         Empty( GetEnv( "HB_COMPILER" ) )
         ErrorLevel( 9 )
         RETURN
      ENDIF
   ELSE
      aGNUMakeParams := {}
   ENDIF

   /* Determine the mode of operation */

   DO CASE
   CASE AScanL( aParams, "clean" ) > 0
      nAction := _ACT_INC_CLEAN
   CASE AScanL( aParams, "install" ) > 0
      IF AScanL( aGNUMakeParams, "clean" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "install" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "install" ) > AScanL( aGNUMakeParams, "clean" )
         /* Use rebuild mode. This is needed because the clean phase might not
            have been called previously by core GNU Make, f.e. because hbrun
            or hbmk2 wasn't available. -rebuildall is costless, so we do it to
            make sure to build cleanly. [vszakats] */
         nAction := _ACT_INC_REBUILD_INST
      ELSE
         nAction := _ACT_INC_INST
      ENDIF
   CASE AScanL( aParams, "first" ) > 0
      IF AScanL( aGNUMakeParams, "clean" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "all" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "all" ) > AScanL( aGNUMakeParams, "clean" )
         nAction := _ACT_INC_REBUILD
      ELSE
         nAction := _ACT_INC
      ENDIF
   CASE AScanL( aParams, "rebuild" ) > 0
      nAction := _ACT_INC_REBUILD
   OTHERWISE
      nAction := _ACT_INC
   ENDCASE

   /* Assemble list of projects to be built */

   IF ! Empty( cFilter := GetEnv( "HB_BUILD_CONTRIBS" ) )
      OutStd( hb_StrFormat( "! HB_BUILD_CONTRIBS: %1$s", cFilter ) + hb_eol() )
   ENDIF

   IF cFilter == "no"
      RETURN
   ENDIF

   aFilter := iif( Empty( cFilter ), {}, hb_ATokens( cFilter,, .T. ) )
   IF Len( aFilter ) >= 1 .AND. aFilter[ 1 ] == "no"
      hb_ADel( aFilter, 1, .T. )
      lFilterNegative := .T.
   ELSE
      lFilterNegative := .F.
   ENDIF

   hProjectReqList := { => }

   FOR EACH tmp IN hProjectList
      hProjectReqList[ tmp:__enumKey() ] := tmp:__enumKey()
   NEXT

   IF ! Empty( aFilter )
      IF ! lFilterNegative
         hProjectReqList := { => }
      ENDIF
      FOR EACH cProject IN aFilter
         FOR EACH tmp IN hProjectList
            IF hb_FileMatch( hb_DirSepToOS( cProject ), hb_DirSepToOS( tmp:__enumKey() ) ) .OR. ;
               hb_FileMatch( hb_DirSepToOS( cProject ), hb_DirSepDel( hb_FNameDir( hb_DirSepToOS( tmp:__enumKey() ) ) ) )
               IF lFilterNegative
                  IF tmp:__enumKey() $ hProjectReqList
                     hb_HDel( hProjectReqList, tmp:__enumKey() )
                  ENDIF
               ELSE
                  hProjectReqList[ tmp:__enumKey() ] := tmp:__enumKey()
               ENDIF
            ENDIF
         NEXT
      NEXT
   ENDIF

   IF Empty( hProjectReqList )
      RETURN
   ENDIF

   /* Clearing envvars that may interact with hbmk2 */

   IF s_lCoreBuild
      /* Saving original install dirs to our own variables */
      hb_SetEnv( "_HB_INSTALL_BIN", GetEnv( "HB_INSTALL_BIN" ) )
      hb_SetEnv( "_HB_INSTALL_LIB", GetEnv( "HB_INSTALL_LIB" ) )
      hb_SetEnv( "_HB_INSTALL_DYN", GetEnv( "HB_INSTALL_DYN" ) )
      hb_SetEnv( "_HB_INSTALL_INC", GetEnv( "HB_INSTALL_INC" ) )
      hb_SetEnv( "_HB_INSTALL_MAN", GetEnv( "HB_INSTALL_MAN" ) )
      hb_SetEnv( "_HB_INSTALL_ETC", GetEnv( "HB_INSTALL_ETC" ) )
      hb_SetEnv( "_HB_INSTALL_CONTRIB", GetEnv( "HB_INSTALL_CONTRIB" ) )
      hb_SetEnv( "_HB_COMPILER_VER", GetEnv( "HB_COMPILER_VER" ) )

      /* Override hbmk2 auto-detection. WARNING: Must be in sync with global.mk logic */
      hb_SetEnv( "HB_INSTALL_PREFIX", s_cRoot )
      hb_SetEnv( "HB_INSTALL_BIN", s_cRoot + "bin/" + GetEnv( "HB_PLATFORM" ) + "/" + GetEnv( "HB_COMPILER" ) + GetEnv( "HB_BUILD_NAME" ) )
      hb_SetEnv( "HB_INSTALL_LIB", s_cRoot + "lib/" + GetEnv( "HB_PLATFORM" ) + "/" + GetEnv( "HB_COMPILER" ) + GetEnv( "HB_BUILD_NAME" ) )
      hb_SetEnv( "HB_INSTALL_DYN" )
      hb_SetEnv( "HB_INSTALL_INC", s_cRoot + "include" )
   ENDIF

   /* Start building */

   OutStd( hb_StrFormat( "! Started %1$s...", sc_hActions[ nAction ] ) + hb_eol() )

   build_projects( nAction, hProjectList, hProjectReqList, "" )

   OutStd( hb_StrFormat( "! Finished %1$s...", sc_hActions[ nAction ] ) + hb_eol() )

   RETURN

STATIC PROCEDURE build_projects( nAction, hProjectList, hProjectReqList, cOptionsUser )

   LOCAL aPairList
   LOCAL aSortedList

   LOCAL cOptions
   LOCAL lInstall
   LOCAL cMakeFlags

   LOCAL cProject
   LOCAL lPrimary
   LOCAL lContainer

   LOCAL cDynSuffix

   LOCAL nErrorLevel

   /* Signal that we're doing a Harbour build */
   hb_SetEnv( "_HB_BUILD_", "yes" )

   /* Preprocessing */

   IF Len( hProjectReqList ) > 1
      OutStd( hb_StrFormat( "! Calculating build order for %1$d projects...", Len( hProjectReqList ) ) + hb_eol() )
   ENDIF

   aPairList := {}

   FOR EACH cProject IN hProjectReqList
      call_hbmk2_hbinfo( s_cHome, cProject, hProjectList[ cProject ] )
      DeptLinesToDeptPairList( aPairList, cProject, hProjectList[ cProject ][ "aDept" ] )
   NEXT

   aSortedList := TopoSort( aPairList )

   /* Add referenced project not present in our list and featuring an .hbp
      file */
   FOR EACH cProject IN aSortedList
      IF AddProject( hProjectList, @cProject )
         call_hbmk2_hbinfo( s_cHome, cProject, hProjectList[ cProject ] )
         hProjectList[ cProject ][ "lFromContainer" ] := NIL
      ENDIF
   NEXT

   /* Load project information for dependencies too
      (we need "cType" to decide about dynamic build) */
   IF GetEnv( "HB_BUILD_CONTRIB_DYN" ) == "yes"
      FOR EACH cProject IN aSortedList
         IF ! cProject $ hProjectReqList .AND. ;
            cProject $ hProjectList .AND. ;
            ! "lChecked" $ hProjectList[ cProject ]
            call_hbmk2_hbinfo( s_cHome, cProject, hProjectList[ cProject ] )
         ENDIF
      NEXT
   ENDIF

   /* Convert action to hbmk2 options */

   cOptions := " -inc"
   SWITCH nAction
   CASE _ACT_INC_CLEAN
      cOptions += " -clean"
      EXIT
   CASE _ACT_INC_REBUILD
   CASE _ACT_INC_REBUILD_INST
      cOptions += " -rebuildall"
      EXIT
   ENDSWITCH

   IF s_lCoreBuild
      cMakeFlags := GetEnv( "MAKEFLAGS" )
      IF " -j " $ " " + cMakeFlags + " "
         /* GNU Make uses job server to limit number of concurrent operations
            We cannot read it from MAKEFLAGS so I set it to arbitrary value: 8 */
         cOptions += " -jobs=8"
      ENDIF
   ENDIF

   lInstall := ;
      nAction == _ACT_INC_INST .OR. ;
      nAction == _ACT_INC_REBUILD_INST

   hb_SetEnv( iif( s_lCoreBuild, "_HB_INST_CORE", "_HB_INST_NONCORE" ), iif( lInstall, "yes", NIL ) )

   /* Build the dependencies and primary targets in sorted order */

   FOR EACH cProject IN aSortedList DESCEND
      IF cProject $ hProjectList

         lPrimary := cProject $ hProjectReqList
         lContainer := "lFromContainer" $ hProjectList[ cProject ]

         IF ( nErrorLevel := call_hbmk2( s_cHome, cProject, iif( lPrimary .OR. lContainer, iif( lContainer, cOptions, cOptions + cOptionsUser ), " -inc" ), NIL ) ) == 0

            /* Build dynamic lib */
            IF GetEnv( "HB_BUILD_CONTRIB_DYN" ) == "yes" .AND. hProjectList[ cProject ][ "cType" ] == "hblib"
               /* Is this a platform where import libs are used? */
               IF "|" + hProjectList[ cProject ][ "cPlatform" ] + "|" $ "|win|dos|os2|"
                  cDynSuffix := "_dll"
               ELSE
                  cDynSuffix := ""
               ENDIF
               call_hbmk2( s_cHome, cProject, iif( lPrimary .OR. lContainer, iif( lContainer, cOptions, cOptions + cOptionsUser ), " -inc" ), cDynSuffix )
            ENDIF

            IF lPrimary .OR. lContainer

               /* Compile documentation */
               IF lInstall
                  mk_hbd( hb_FNameDir( hb_DirSepToOS( s_cHome + cProject ) ) )
               ENDIF
            ENDIF
         ELSE
            /* Ignore certain non-fatal hbmk2 return values */
            IF nErrorLevel != 10 .AND. ;
               nErrorLevel != 20 .AND. ;
               nErrorLevel != 50
               ErrorLevel( nErrorLevel )
               EXIT
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE call_hbmk2_hbinfo( cProjectRoot, cProjectName, hProject )

   LOCAL cStdOut
   LOCAL tmp, tmp1, tmp2
   LOCAL hInfo

   LOCAL nErrorLevel

   LOCAL cProjectPath := hb_DirSepToOS( cProjectRoot + cProjectName )

   hProject[ "cType" ] := ""
   hProject[ "aDept" ] := {}
   hProject[ "lChecked" ] := NIL

   IF ( nErrorLevel := call_hbmk2( cProjectRoot, cProjectName, " --hbinfo",,, @cStdOut ) ) == 0

      IF ! HB_ISHASH( hInfo := hb_jsonDecode( cStdOut ) )
         OutStd( "! Warning: Received invalid result from 'hbmk2 --hbinfo'" + hb_eol() )
      ENDIF

      hProject[ "cType" ] := hbmk2_hbinfo_getitem( hInfo, "targettype" )
      hProject[ "cOutputName" ] := hbmk2_hbinfo_getitem( hInfo, "outputname" )
      hProject[ "cDynSuffix" ] := hbmk2_hbinfo_getitem( hInfo, "dynsuffix" )
      hProject[ "cPlatform" ] := hbmk2_hbinfo_getitem( hInfo, "platform" )

      FOR EACH tmp IN hb_ATokens( hbmk2_hbinfo_getitem( hInfo, "hbctree" ), .T. )
         IF ! Empty( tmp )
#ifdef __PLATFORM__DOS
            /* Ignore long filenames on MS-DOS hosts */
            IF hb_BLen( hb_FNameName( LTrim( tmp ) ) ) > 8
               LOOP
            ENDIF
#endif
            /* Convert .hbc reference to an .hbp one */
            tmp1 := hb_FNameExtSet( hb_DirSepToOS( LTrim( tmp ) ), ".hbp" )
            /* Calculate its full path */
            tmp2 := hb_PathNormalize( hb_PathJoin( hb_DirSepToOS( hb_cwd() ), tmp1 ) )
            /* Rebase its full path onto the project store root */
            tmp1 := hb_PathNormalize( hb_PathRelativize( ;
               hb_PathNormalize( hb_PathJoin( hb_DirSepToOS( hb_cwd() ), hb_DirSepToOS( s_cHome ) ) ), ;
               tmp2 ) )

            /* Do not add any .hbc reference that resides outside the project
               store directory tree. This case can be detected by verifying if
               the full path remained unchanged after rebasing to project store
               root. */
            IF ! tmp1 == tmp2
               AAdd( hProject[ "aDept" ], { "nDepth" => Len( tmp ) - Len( LTrim( tmp ) ), ;
                  "cFileName_HBP" => StrTran( tmp1, "\", "/" ) } )
            ENDIF
         ENDIF
      NEXT
   ELSE
      OutStd( hb_StrFormat( "! Warning: 'hbmk2 %1$s --hbinfo' failed with exit code %2$d", cProjectPath, nErrorLevel ) + hb_eol() )
   ENDIF

   RETURN

STATIC FUNCTION hbmk2_hbinfo_getitem( hInfo, cItem )
   RETURN iif( HB_ISHASH( hInfo ), hb_HGetDef( hInfo, cItem, "" ), "" )

STATIC FUNCTION call_hbmk2( cProjectRoot, cProjectName, cOptionsPre, cDynSuffix, cStdErr, cStdOut )

   LOCAL nErrorLevel
   LOCAL cOptionsLibDyn := ""
   LOCAL cCommand

   LOCAL cProjectPath := hb_DirSepToOS( cProjectRoot + cProjectName )
   LOCAL cGlobalConf := hb_DirSepToOS( s_cRoot + "config/" )

   /* Making sure that user settings do not interfere with the std build process. */
   hb_SetEnv( "HBMK_OPTIONS" )
   hb_SetEnv( "HARBOUR" )
   hb_SetEnv( "HARBOURCMD" )
   hb_SetEnv( "CLIPPER" )
   hb_SetEnv( "CLIPPERCMD" )
   hb_SetEnv( "_HB_DYNSUFF" )

   IF cDynSuffix != NIL
      IF ! cDynSuffix == ""
         /* Request dll version of project dependencies (the implibs) to be
            linked, on non-*nix platforms (experimental) */
         hb_SetEnv( "_HB_DYNSUFF", cDynSuffix )
      ENDIF

      cOptionsPre += " -hbdyn"

      IF hb_vfExists( hb_FNameExtSet( cProjectPath, ".hbc" ) )
         cOptionsLibDyn += " " + hb_FNameExtSet( cProjectPath, ".hbc" )
      ENDIF
   ENDIF

   /* Relative directory to the project from hbpost.hbm */
   hb_SetEnv( "_HB_PROJECT_HOME", hb_PathNormalize( hb_PathRelativize( ;
      hb_PathNormalize( hb_PathJoin( hb_cwd(), cGlobalConf ) ), ;
      hb_PathNormalize( hb_PathJoin( hb_cwd(), hb_FNameDir( cProjectPath ) ) ) ) ) )

   /* <projectname>[/<subproject>] */
   hb_SetEnv( "_HB_PROJECT_SUBDIR", hb_DirSepDel( hb_FNameDir( cProjectName ) ) )

   cCommand := s_cBinDir + "hbmk2" + ;
      " -lang=en -quiet -width=0 -autohbm-" + ;
      " @" + StrTran( cGlobalConf + "hbpre", "\", "/" ) + ;
      cOptionsPre + ;
      " " + StrTran( cProjectPath, "\", "/" ) + ;
      " @" + StrTran( cGlobalConf + "hbpost", "\", "/" ) + ;
      StrTran( cOptionsLibDyn, "\", "/" )

   IF PCount() >= 5
      nErrorLevel := hb_processRun( cCommand,, @cStdOut, @cStdErr )
   ELSE
      nErrorLevel := mk_hb_processRun( cCommand )
   ENDIF

   RETURN nErrorLevel

STATIC FUNCTION mk_hb_processRun( cCommand, ... )

   OutStd( cCommand + hb_eol() )

   RETURN hb_processRun( cCommand, ... )

STATIC FUNCTION mk_hbd( cDir )

   LOCAL cName
   LOCAL cDocDir
   LOCAL tmp

   LOCAL aErrMsg
   LOCAL aEntry

   IF ! ( cDocDir := GetEnv( "HB_INSTALL_DOC" ) ) == "" .AND. ! cDocDir == "no"

      IF Empty( cName := DirGetName( cDir ) )
         cName := "harbour"
      ENDIF

      aErrMsg := {}
      aEntry := __hbdoc_LoadDir( cDir, cName, aErrMsg )

      FOR EACH tmp IN aErrMsg
         OutErr( hb_StrFormat( "! %1$s", tmp ) + hb_eol() )
      NEXT

      IF ! Empty( aEntry )
         cName := hb_DirSepToOS( cDocDir ) + hb_ps() + cName + ".hbd"
         IF __hbdoc_SaveHBD( cName, aEntry )
            mk_hb_vfTimeSet( cName )
            OutStd( hb_StrFormat( "! Compiled documentation: %1$s <= %2$s", cName, cDir ) + hb_eol() )
            RETURN .T.
         ELSE
            OutErr( hb_StrFormat( "! Error: Saving '%1$s'", cName ) + hb_eol() )
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION DirGetName( cDir )

   LOCAL cName := hb_FNameName( hb_DirSepDel( cDir ) )

   IF cName == "" .OR. cName == "." .OR. cName == ".."
      RETURN ""
   ENDIF

   RETURN cName

/* Convert indented list of line to tree / list of parent-child pairs */
STATIC PROCEDURE DeptLinesToDeptPairList( aPairList, cParent, aFlatTree )

   LOCAL hFlatTreeElement
   LOCAL hNode, hNewNode, tmp
   LOCAL nLevel, nDepth

   AddDeptPair( aPairList, "", cParent )

   hNode := { "child" => {}, "name" => cParent, "parent" => }
   nLevel := 0
   FOR EACH hFlatTreeElement IN aFlatTree
      /* Min() protects against jumping more than one level down in one step */
      nDepth := Min( hFlatTreeElement[ "nDepth" ], nLevel + 1 )
      hNewNode := { "child" => {}, "name" => hFlatTreeElement[ "cFileName_HBP" ], "cargo" => hFlatTreeElement }
      IF nDepth > nLevel
         hNode := ATail( hNode[ "child" ] )
      ELSEIF nDepth < nLevel
         FOR tmp := nDepth + 1 TO nLevel
            hNode := hNode[ "parent" ]
         NEXT
      ENDIF
      hNewNode[ "parent" ] := hNode
      AAdd( hNode[ "child" ], hNewNode )
      nLevel := nDepth
      AddDeptPair( aPairList, hNewNode[ "parent" ][ "name" ], hNewNode[ "name" ] )
   NEXT

   RETURN

/* Add parent-child dependency to the list */
STATIC PROCEDURE AddDeptPair( aPairList, cParent, cChild )

   IF AScan( aPairList, {| tmp | tmp[ 1 ] == cParent .AND. tmp[ 2 ] == cChild } ) == 0
      AAdd( aPairList, { cParent, cChild } )
   ENDIF

   RETURN

/* Topological sort of the dependency graph */
STATIC FUNCTION TopoSort( aEdgeList )

   LOCAL aList := {}
   LOCAL hTopNodes := { => }

   LOCAL n, m
   LOCAL tmp

   FOR EACH n IN aEdgeList
      IF AScan( aEdgeList, {| tmp | tmp[ 2 ] == n[ 1 ] } ) == 0
         hTopNodes[ n[ 1 ] ] := NIL
      ENDIF
   NEXT

   DO WHILE ! Empty( hTopNodes )
      n := hb_HKeyAt( hTopNodes, 1 )
      hb_HDelAt( hTopNodes, 1 )

      IF ! Empty( n )
         AAdd( aList, n )
      ENDIF

      FOR EACH tmp IN aEdgeList
         IF tmp[ 1 ] == n
            m := tmp[ 2 ]
            tmp[ 1 ] := tmp[ 2 ] := NIL  /* set to invalid value. TOOPT: Delete this member from list */
            IF AScan( aEdgeList, {| tmp | tmp[ 2 ] == m } ) == 0
               hTopNodes[ m ] := NIL
            ENDIF
         ENDIF
      NEXT
   ENDDO

   FOR EACH tmp IN aEdgeList
      IF !( tmp[ 1 ] == NIL .AND. tmp[ 2 ] == NIL )
         OutStd( hb_StrFormat( "! Warning: Circular reference in dependency tree (%1$s - %2$s)", tmp[ 1 ], tmp[ 2 ] ) + hb_eol() )
      ENDIF
   NEXT

   RETURN aList

STATIC FUNCTION AddProject( hProjectList, cFileName )

   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   IF ! cFileName == ""

      cFileName := hb_DirSepToOS( AllTrim( cFileName ) )

      hb_FNameSplit( cFileName, @cDir, @cName, @cExt )

      DO CASE
      CASE cName == ""
         cName := DirGetName( cDir )
      CASE cDir == ""
         cDir := cName
      ENDCASE
      IF Empty( cExt )
         cExt := ".hbp"
      ENDIF

      cFileName := hb_FNameMerge( cDir, cName, cExt )

      IF hb_vfExists( hb_DirSepToOS( s_cHome + cFileName ) )
         cFileName := StrTran( cFileName, "\", "/" )
         IF ! cFileName $ hProjectList
            hProjectList[ cFileName ] := { => }
            RETURN .T.
         ENDIF
      ELSE
         OutStd( hb_StrFormat( "! Warning: Project not found, cannot be added: %1$s", hb_DirSepToOS( s_cHome + cFileName ) ) + hb_eol() )
      ENDIF
   ENDIF

   RETURN .F.

/* Build all projects that have a .hbp file matching the name of its project
   store subdir. Also support projects with multiple subprojects if it has
   a 'makesub.txt' text file with a list of those subprojects. */
STATIC PROCEDURE LoadProjectListAutomatic( hProjectList )

   LOCAL aFile
   LOCAL tmp

   LOCAL cDir := hb_DirSepAdd( hb_DirSepToOS( s_cHome ) )

   FOR EACH aFile IN hb_vfDirectory( cDir, "D" )
      IF "D" $ aFile[ F_ATTR ] .AND. !( aFile[ F_NAME ] == "." .OR. aFile[ F_NAME ] == ".." )
         IF hb_vfExists( cDir + ( tmp := aFile[ F_NAME ] + hb_ps() + hb_FNameExtSet( aFile[ F_NAME ], ".hbp" ) ) )
            AddProject( hProjectList, tmp )
         ENDIF
         IF hb_vfExists( tmp := ( cDir + aFile[ F_NAME ] + hb_ps() + "makesub.txt" ) )
            FOR EACH tmp IN hb_ATokens( hb_MemoRead( tmp ), .T. )
               IF ! tmp == ""
                  AddProject( hProjectList, aFile[ F_NAME ] + hb_ps() + hb_DirSepToOS( tmp ) )
               ENDIF
            NEXT
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE LoadProjectListFromString( hProjectList, cString )

   LOCAL cItem

   FOR EACH cItem IN hb_ATokens( cString,, .T. )
      AddProject( hProjectList, cItem )
   NEXT

   RETURN

STATIC FUNCTION mk_hb_vfTimeSet( cFileName )

   STATIC s_tVCS

   LOCAL cStdOut, cStdErr

   IF s_tVCS == NIL
      IF hb_processRun( "git log -1 --format=format:%ci",, @cStdOut, @cStdErr ) != 0
         cStdOut := hb_ATokens( hb_MemoRead( s_cRoot + "include" + hb_ps() + "_repover.txt" ), .T. )
         cStdOut := iif( Len( cStdOut ) >= 2, cStdOut[ 2 ], "" )
      ENDIF

      s_tVCS := hb_CToT( cStdOut, "yyyy-mm-dd", "hh:mm:ss" )

      IF ! Empty( s_tVCS )
         s_tVCS -= ( ( ( iif( SubStr( cStdOut, 21, 1 ) == "-", -1, 1 ) * 60 * ;
                       ( Val( SubStr( cStdOut, 22, 2 ) ) * 60 + ;
                         Val( SubStr( cStdOut, 24, 2 ) ) ) ) - hb_UTCOffset() ) / 86400 )
      ENDIF
   ENDIF

   RETURN Empty( s_tVCS ) .OR. hb_vfTimeSet( cFileName, s_tVCS )
